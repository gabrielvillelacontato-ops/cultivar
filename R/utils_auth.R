#' Utilitarios de autenticacao e gestao de operadores
#'
#' Funcoes puras (recebem conexao DBI, retornam dados ou levantam erro).
#' Toda escrita registra no audit_log na mesma transacao quando aplicavel.
#'
#' Hash de PIN via sodium::password_store (algoritmo scrypt; lento por
#' design para resistir a brute-force; salt embutido no proprio hash).
#'
#' Coluna pin_salt do schema mantida por compatibilidade com migration 001,
#' mas armazena raw vazio - o salt real fica embutido no pin_hash.
#'
#' @noRd

# ---------------------------------------------------------------------
# Constantes locais
# ---------------------------------------------------------------------

.PAPEIS_VALIDOS <- c("operador", "supervisor", "admin")
.PIN_REGEX <- "^[0-9]{4}$"

# ---------------------------------------------------------------------
# Validacao de entrada
# ---------------------------------------------------------------------

.validar_pin_formato <- function(pin) {
  if (is.null(pin) || length(pin) != 1L || is.na(pin)) {
    stop("PIN deve ser string nao vazia.", call. = FALSE)
  }
  if (!is.character(pin) || !grepl(.PIN_REGEX, pin)) {
    stop("PIN deve conter exatamente 4 digitos (0-9).", call. = FALSE)
  }
  invisible(NULL)
}

.validar_papel <- function(papel) {
  if (is.null(papel) || length(papel) != 1L || is.na(papel)) {
    stop("Papel deve ser string nao vazia.", call. = FALSE)
  }
  if (!papel %in% .PAPEIS_VALIDOS) {
    stop("Papel invalido: '", papel, "'. Valores aceitos: ",
         paste(.PAPEIS_VALIDOS, collapse = ", "), call. = FALSE)
  }
  invisible(NULL)
}

.validar_nome <- function(nome) {
  if (is.null(nome) || length(nome) != 1L || is.na(nome)) {
    stop("Nome deve ser string nao vazia.", call. = FALSE)
  }
  if (!is.character(nome)) {
    stop("Nome deve ser string.", call. = FALSE)
  }
  trimmed <- trimws(nome)
  if (nchar(trimmed) == 0L) {
    stop("Nome nao pode ser apenas espacos em branco.", call. = FALSE)
  }
  if (nchar(trimmed) > 100L) {
    stop("Nome muito longo (max 100 caracteres).", call. = FALSE)
  }
  invisible(NULL)
}

# ---------------------------------------------------------------------
# Hash e verificacao
# ---------------------------------------------------------------------

hash_pin <- function(pin) {
  .validar_pin_formato(pin)
  sodium::password_store(pin)
}

verificar_pin <- function(pin, pin_hash) {
  .validar_pin_formato(pin)
  if (is.null(pin_hash) || (length(pin_hash) == 1L && is.na(pin_hash))) {
    return(FALSE)
  }
  tryCatch(
    sodium::password_verify(pin_hash, pin),
    error = function(e) FALSE
  )
}

# ---------------------------------------------------------------------
# Estado do sistema (setup inicial)
# ---------------------------------------------------------------------

contar_operadores_ativos <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM operadores
     WHERE tenant_id = ? AND deleted_at IS NULL AND ativo = 1;",
    params = list(tenant_id)
  )$n
}

contar_admins_ativos <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM operadores
     WHERE tenant_id = ? AND deleted_at IS NULL AND ativo = 1
       AND papel = 'admin';",
    params = list(tenant_id)
  )$n
}

precisa_setup_inicial <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  contar_operadores_ativos(con, tenant_id) == 0L
}

# ---------------------------------------------------------------------
# Criacao de operadores
# ---------------------------------------------------------------------

criar_operador <- function(con, nome, pin, papel,
                           email = NA_character_,
                           criado_por_id = NA_integer_,
                           tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  .validar_nome(nome)
  .validar_pin_formato(pin)
  .validar_papel(papel)
  
  nome <- trimws(nome)
  
  existe <- DBI::dbGetQuery(
    con,
    "SELECT id FROM operadores WHERE tenant_id = ? AND nome = ? LIMIT 1;",
    params = list(tenant_id, nome)
  )
  if (nrow(existe) > 0L) {
    stop("Ja existe operador com nome '", nome, "' neste tenant.",
         call. = FALSE)
  }
  
  if (precisa_setup_inicial(con, tenant_id) && papel != "admin") {
    stop("O primeiro operador do sistema deve ter papel 'admin'. ",
         "Recebido: '", papel, "'.", call. = FALSE)
  }
  
  hash <- hash_pin(pin)
  salt_placeholder <- as.raw(0L)
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "INSERT INTO operadores
         (tenant_id, nome, email, pin_hash, pin_salt, papel,
          ativo, tentativas_falhas)
       VALUES (?, ?, ?, ?, ?, ?, 1, 0);",
      params = list(tenant_id, nome,
                    if (is.na(email)) NA_character_ else email,
                    hash, list(salt_placeholder), papel)
    )
    novo_id <- DBI::dbGetQuery(
      con,
      "SELECT id FROM operadores WHERE tenant_id = ? AND nome = ?;",
      params = list(tenant_id, nome)
    )$id[1]
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          valores_depois)
       VALUES (?, ?, 'operadores', ?, 'INSERT', ?);",
      params = list(
        tenant_id,
        if (is.na(criado_por_id)) NA_integer_ else as.integer(criado_por_id),
        as.integer(novo_id),
        sprintf('{"nome":"%s","papel":"%s"}', nome, papel)
      )
    )
    DBI::dbCommit(con)
    as.integer(novo_id)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao criar operador: ", conditionMessage(e), call. = FALSE)
  })
}

buscar_operador_por_nome <- function(con, nome,
                                     tenant_id = TENANT_DEFAULT_ID) {
  DBI::dbGetQuery(
    con,
    "SELECT id, nome, email, pin_hash, papel, ativo,
            tentativas_falhas, bloqueado_ate, deleted_at
     FROM operadores
     WHERE tenant_id = ? AND nome = ? LIMIT 1;",
    params = list(tenant_id, nome)
  )
}

# ---------------------------------------------------------------------
# Lockout
# ---------------------------------------------------------------------

esta_bloqueado <- function(bloqueado_ate) {
  if (is.null(bloqueado_ate) || is.na(bloqueado_ate) || !nzchar(bloqueado_ate)) {
    return(FALSE)
  }
  bloqueio <- tryCatch(
    as.POSIXct(bloqueado_ate, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    error = function(e) NA
  )
  if (is.na(bloqueio)) return(FALSE)
  bloqueio > Sys.time()
}

registrar_login_falho <- function(con, operador_id,
                                  tenant_id = TENANT_DEFAULT_ID,
                                  contexto = NA_character_) {
  if (is.na(operador_id)) {
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, acao, contexto)
       VALUES (?, NULL, 'operadores', 'LOGIN_FALHO', ?);",
      params = list(tenant_id,
                    if (is.na(contexto)) "Usuario inexistente" else contexto)
    )
    return(invisible(NULL))
  }
  
  operador_id <- as.integer(operador_id)
  
  DBI::dbBegin(con)
  tryCatch({
    op <- DBI::dbGetQuery(
      con,
      "SELECT tentativas_falhas FROM operadores WHERE id = ?;",
      params = list(operador_id)
    )
    tentativas <- as.integer(op$tentativas_falhas[1]) + 1L
    
    if (tentativas >= PIN_MAX_TENTATIVAS) {
      bloqueio <- format(
        Sys.time() + PIN_LOCKOUT_MINUTOS * 60,
        "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
      )
      DBI::dbExecute(
        con,
        "UPDATE operadores
           SET tentativas_falhas = ?, bloqueado_ate = ?
         WHERE id = ?;",
        params = list(tentativas, bloqueio, operador_id)
      )
      DBI::dbExecute(
        con,
        "INSERT INTO audit_log
           (tenant_id, operador_id, entidade_tabela, entidade_id,
            acao, contexto)
         VALUES (?, ?, 'operadores', ?, 'LOCKOUT', ?);",
        params = list(tenant_id, operador_id, operador_id,
                      sprintf("Bloqueio apos %d tentativas", tentativas))
      )
    } else {
      DBI::dbExecute(
        con,
        "UPDATE operadores SET tentativas_falhas = ? WHERE id = ?;",
        params = list(tentativas, operador_id)
      )
      DBI::dbExecute(
        con,
        "INSERT INTO audit_log
           (tenant_id, operador_id, entidade_tabela, entidade_id,
            acao, contexto)
         VALUES (?, ?, 'operadores', ?, 'LOGIN_FALHO', ?);",
        params = list(tenant_id, operador_id, operador_id,
                      sprintf("Tentativa %d/%d", tentativas, PIN_MAX_TENTATIVAS))
      )
    }
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao registrar login falho: ", conditionMessage(e),
         call. = FALSE)
  })
  invisible(NULL)
}

registrar_login_sucesso <- function(con, operador_id,
                                    tenant_id = TENANT_DEFAULT_ID) {
  operador_id <- as.integer(operador_id)
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE operadores
         SET tentativas_falhas = 0, bloqueado_ate = NULL
       WHERE id = ?;",
      params = list(operador_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao)
       VALUES (?, ?, 'operadores', ?, 'LOGIN');",
      params = list(tenant_id, operador_id, operador_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao registrar login: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

registrar_logout <- function(con, operador_id,
                             tenant_id = TENANT_DEFAULT_ID) {
  DBI::dbExecute(
    con,
    "INSERT INTO audit_log
       (tenant_id, operador_id, entidade_tabela, entidade_id, acao)
     VALUES (?, ?, 'operadores', ?, 'LOGOUT');",
    params = list(tenant_id, as.integer(operador_id), as.integer(operador_id))
  )
  invisible(NULL)
}

# ---------------------------------------------------------------------
# Login orquestrado
# ---------------------------------------------------------------------

autenticar <- function(con, nome, pin, tenant_id = TENANT_DEFAULT_ID) {
  .validar_nome(nome)
  .validar_pin_formato(pin)
  
  op <- buscar_operador_por_nome(con, trimws(nome), tenant_id)
  
  if (nrow(op) == 0L) {
    registrar_login_falho(con, NA_integer_, tenant_id,
                          contexto = sprintf("Nome '%s' nao encontrado",
                                             trimws(nome)))
    return(list(sucesso = FALSE, motivo = "usuario_nao_encontrado",
                operador = NULL))
  }
  
  if (!is.na(op$deleted_at[1])) {
    return(list(sucesso = FALSE, motivo = "arquivado", operador = NULL))
  }
  if (op$ativo[1] != 1L) {
    return(list(sucesso = FALSE, motivo = "inativo", operador = NULL))
  }
  if (esta_bloqueado(op$bloqueado_ate[1])) {
    return(list(sucesso = FALSE, motivo = "bloqueado",
                operador = NULL, bloqueado_ate = op$bloqueado_ate[1]))
  }
  
  if (verificar_pin(pin, op$pin_hash[1])) {
    registrar_login_sucesso(con, op$id[1], tenant_id)
    op_atualizado <- buscar_operador_por_nome(con, op$nome[1], tenant_id)
    return(list(sucesso = TRUE, motivo = "ok", operador = op_atualizado))
  } else {
    registrar_login_falho(con, op$id[1], tenant_id,
                          contexto = "PIN incorreto")
    return(list(sucesso = FALSE, motivo = "pin_incorreto", operador = NULL))
  }
}

# ---------------------------------------------------------------------
# Mudanca de PIN
# ---------------------------------------------------------------------

mudar_pin <- function(con, operador_id, pin_novo,
                      solicitante_id, pin_atual = NULL,
                      tenant_id = TENANT_DEFAULT_ID) {
  .validar_pin_formato(pin_novo)
  operador_id <- as.integer(operador_id)
  solicitante_id <- as.integer(solicitante_id)
  
  auto_mudanca <- (operador_id == solicitante_id)
  
  solicitante <- DBI::dbGetQuery(
    con,
    "SELECT papel, deleted_at FROM operadores WHERE id = ? AND tenant_id = ?;",
    params = list(solicitante_id, tenant_id)
  )
  if (nrow(solicitante) == 0L || !is.na(solicitante$deleted_at[1])) {
    stop("Solicitante nao encontrado ou arquivado.", call. = FALSE)
  }
  
  if (auto_mudanca) {
    if (is.null(pin_atual)) {
      stop("Auto-mudanca de PIN exige pin_atual.", call. = FALSE)
    }
    op <- DBI::dbGetQuery(
      con, "SELECT pin_hash FROM operadores WHERE id = ?;",
      params = list(operador_id)
    )
    if (!verificar_pin(pin_atual, op$pin_hash[1])) {
      stop("PIN atual incorreto.", call. = FALSE)
    }
  } else {
    if (solicitante$papel[1] != "admin") {
      stop("Apenas admins podem resetar PIN de outro operador.",
           call. = FALSE)
    }
  }
  
  novo_hash <- hash_pin(pin_novo)
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE operadores
         SET pin_hash = ?, tentativas_falhas = 0, bloqueado_ate = NULL
       WHERE id = ?;",
      params = list(novo_hash, operador_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'operadores', ?, 'UPDATE', ?);",
      params = list(tenant_id, solicitante_id, operador_id,
                    if (auto_mudanca) "Auto-mudanca de PIN"
                    else "Reset de PIN por admin")
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao mudar PIN: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------
# Mudanca de papel
# ---------------------------------------------------------------------

mudar_papel <- function(con, operador_id, papel_novo, solicitante_id,
                        tenant_id = TENANT_DEFAULT_ID) {
  .validar_papel(papel_novo)
  operador_id <- as.integer(operador_id)
  solicitante_id <- as.integer(solicitante_id)
  
  solicitante <- DBI::dbGetQuery(
    con,
    "SELECT papel, deleted_at FROM operadores WHERE id = ? AND tenant_id = ?;",
    params = list(solicitante_id, tenant_id)
  )
  if (nrow(solicitante) == 0L || !is.na(solicitante$deleted_at[1])) {
    stop("Solicitante nao encontrado ou arquivado.", call. = FALSE)
  }
  if (solicitante$papel[1] != "admin") {
    stop("Apenas admins podem mudar papel.", call. = FALSE)
  }
  
  op <- DBI::dbGetQuery(
    con,
    "SELECT papel, ativo, deleted_at FROM operadores
     WHERE id = ? AND tenant_id = ?;",
    params = list(operador_id, tenant_id)
  )
  if (nrow(op) == 0L) {
    stop("Operador nao encontrado.", call. = FALSE)
  }
  papel_antigo <- op$papel[1]
  
  if (papel_antigo == papel_novo) {
    return(invisible(NULL))
  }
  
  if (papel_antigo == "admin" && papel_novo != "admin") {
    admins <- contar_admins_ativos(con, tenant_id)
    if (admins <= 1L) {
      stop("Nao e possivel rebaixar o ultimo admin ativo do sistema.",
           call. = FALSE)
    }
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE operadores SET papel = ? WHERE id = ?;",
      params = list(papel_novo, operador_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          valores_antes, valores_depois)
       VALUES (?, ?, 'operadores', ?, 'UPDATE', ?, ?);",
      params = list(
        tenant_id, solicitante_id, operador_id,
        sprintf('{"papel":"%s"}', papel_antigo),
        sprintf('{"papel":"%s"}', papel_novo)
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao mudar papel: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------
# Desativacao / reativacao
# ---------------------------------------------------------------------

desativar_operador <- function(con, operador_id, solicitante_id,
                               tenant_id = TENANT_DEFAULT_ID) {
  operador_id <- as.integer(operador_id)
  solicitante_id <- as.integer(solicitante_id)
  
  solicitante <- DBI::dbGetQuery(
    con,
    "SELECT papel FROM operadores WHERE id = ? AND tenant_id = ?
       AND deleted_at IS NULL;",
    params = list(solicitante_id, tenant_id)
  )
  if (nrow(solicitante) == 0L || solicitante$papel[1] != "admin") {
    stop("Apenas admins podem desativar operadores.", call. = FALSE)
  }
  
  op <- DBI::dbGetQuery(
    con,
    "SELECT papel, ativo FROM operadores WHERE id = ? AND tenant_id = ?;",
    params = list(operador_id, tenant_id)
  )
  if (nrow(op) == 0L) stop("Operador nao encontrado.", call. = FALSE)
  
  if (op$papel[1] == "admin" && op$ativo[1] == 1L) {
    admins <- contar_admins_ativos(con, tenant_id)
    if (admins <= 1L) {
      stop("Nao e possivel desativar o ultimo admin ativo.", call. = FALSE)
    }
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE operadores SET ativo = 0 WHERE id = ?;",
      params = list(operador_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'operadores', ?, 'UPDATE', 'Operador desativado');",
      params = list(tenant_id, solicitante_id, operador_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao desativar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

reativar_operador <- function(con, operador_id, solicitante_id,
                              tenant_id = TENANT_DEFAULT_ID) {
  operador_id <- as.integer(operador_id)
  solicitante_id <- as.integer(solicitante_id)
  
  solicitante <- DBI::dbGetQuery(
    con,
    "SELECT papel FROM operadores WHERE id = ? AND tenant_id = ?
       AND deleted_at IS NULL;",
    params = list(solicitante_id, tenant_id)
  )
  if (nrow(solicitante) == 0L || solicitante$papel[1] != "admin") {
    stop("Apenas admins podem reativar operadores.", call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE operadores SET ativo = 1, tentativas_falhas = 0,
                              bloqueado_ate = NULL
       WHERE id = ? AND tenant_id = ?;",
      params = list(operador_id, tenant_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'operadores', ?, 'UPDATE', 'Operador reativado');",
      params = list(tenant_id, solicitante_id, operador_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao reativar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}