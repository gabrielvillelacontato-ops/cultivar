# =====================================================================
# CultivaR - Editor de composicao de meios (script administrativo)
# =====================================================================
#
# ATENCAO: este script permite editar diretamente a composicao de um
# meio ja cadastrado. Use APENAS em casos justificados:
#
#   - Correcao de erro de digitacao na composicao original
#   - Ajuste apos revisao bibliografica de referencia primaria
#   - Adicao de componente faltante identificado por revisor
#
# ALCOA+ e preservado:
#   - Todas as alteracoes vao para audit_log com timestamp, operador,
#     valores antes/depois e MOTIVO obrigatorio.
#   - Preparos ja iniciados NAO sao afetados: eles usam snapshot
#     (preparo_componentes_usados foi populado no momento do inicio,
#     independente do meio_componentes). Apenas preparos FUTUROS
#     verao a nova composicao.
#
# PROTECOES ATIVAS:
#   - Operador deve ter papel = 'admin'
#   - Meio arquivado nao pode ser editado (restaure primeiro)
#   - Meio bloqueado_preparo emite AVISO mas permite edicao
#     (caso comum: bloqueio foi por composicao suspeita a ser corrigida)
#
# Fluxo de uso:
#   1. Preencher CONFIG no topo (meio_id, operador_id, motivo)
#   2. Rodar bloco "LEITURA" (ler_composicao()) para confirmar estado atual
#   3. Escolher operacao (adicionar/alterar/remover) - SEMPRE rodar com
#      dry_run = TRUE primeiro para preview
#   4. Ao confirmar visualmente, rodar de novo com dry_run = FALSE
#   5. Rodar ler_composicao() de novo para conferir estado final
#
# NAO commitar este arquivo com config real preenchida. Reverta o topo
# antes de commitar.
# =====================================================================

library(DBI)
library(RSQLite)

devtools::load_all()  # carrega utilitarios do pacote cultivaR

# ---------------------------------------------------------------------
# Unidades permitidas (espelho do CHECK do schema)
# ---------------------------------------------------------------------

.UNIDADES_VALIDAS <- c(
  "mg/L", "ug/L", "g/L", "ug/mL", "mg/mL",
  "uM", "nM", "mM", "M", "%", "ppm", "x"
)

# ---------------------------------------------------------------------
# CONFIG - preencher antes de executar
# ---------------------------------------------------------------------

CONFIG <- list(
  # Caminho do banco (default = banco de dev)
  db_path = DB_PATH_DEV,
  
  # ID do meio a editar
  meio_id = NA_integer_,
  
  # ID do operador autorizado (deve ter papel = 'admin')
  operador_id = NA_integer_,
  
  # Motivo do ajuste - vai para audit_log$contexto (OBRIGATORIO)
  motivo = ""
)

# ---------------------------------------------------------------------
# Validacoes de contexto
# ---------------------------------------------------------------------

.validar_config <- function(cfg) {
  if (is.na(cfg$meio_id))
    stop("Preencha CONFIG$meio_id.", call. = FALSE)
  if (is.na(cfg$operador_id))
    stop("Preencha CONFIG$operador_id.", call. = FALSE)
  if (!nzchar(trimws(cfg$motivo)))
    stop("Preencha CONFIG$motivo (obrigatorio para audit trail).",
         call. = FALSE)
  if (!file.exists(cfg$db_path))
    stop("Banco nao encontrado: ", cfg$db_path, call. = FALSE)
}

.validar_operador_admin <- function(con, operador_id, tenant_id) {
  op <- DBI::dbGetQuery(
    con,
    "SELECT id, papel, ativo, deleted_at FROM operadores
     WHERE id = ? AND tenant_id = ?;",
    params = list(as.integer(operador_id), tenant_id)
  )
  if (nrow(op) == 0L) {
    stop("Operador nao encontrado (id=", operador_id, ").",
         call. = FALSE)
  }
  if (op$ativo[1] != 1L || !is.na(op$deleted_at[1])) {
    stop("Operador inativo ou arquivado.", call. = FALSE)
  }
  if (op$papel[1] != "admin") {
    stop("Este script exige operador com papel = 'admin'. ",
         "Papel atual: '", op$papel[1], "'.", call. = FALSE)
  }
  invisible(TRUE)
}

.validar_meio_editavel <- function(con, meio_id) {
  m <- DBI::dbGetQuery(
    con,
    "SELECT id, codigo_curto, nome, bloqueado_preparo, deleted_at
     FROM meios WHERE id = ?;",
    params = list(as.integer(meio_id))
  )
  if (nrow(m) == 0L) {
    stop("Meio nao encontrado (id=", meio_id, ").", call. = FALSE)
  }
  if (!is.na(m$deleted_at[1])) {
    stop("Meio '", m$codigo_curto[1],
         "' esta arquivado. Restaure antes de editar composicao.",
         call. = FALSE)
  }
  if (m$bloqueado_preparo[1] == 1L) {
    cat("\n[AVISO] Meio '", m$codigo_curto[1],
        "' esta bloqueado_preparo. ",
        "Edicao permitida (pode ser justamente o caso ",
        "que motivou o bloqueio), mas confirme se e mesmo ",
        "isso que quer fazer.\n\n", sep = "")
  }
  invisible(m)
}

.conectar <- function(cfg) {
  con <- DBI::dbConnect(RSQLite::SQLite(), cfg$db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

# Guarda de contexto: valida config, admin e meio editavel;
# retorna conexao pronta para uso.
.abrir_contexto <- function(cfg) {
  .validar_config(cfg)
  con <- .conectar(cfg)
  tryCatch({
    .validar_operador_admin(con, cfg$operador_id, TENANT_DEFAULT_ID)
    .validar_meio_editavel(con, cfg$meio_id)
    con
  }, error = function(e) {
    DBI::dbDisconnect(con)
    stop(conditionMessage(e), call. = FALSE)
  })
}

# ---------------------------------------------------------------------
# LEITURA
# ---------------------------------------------------------------------

ler_composicao <- function(cfg = CONFIG) {
  con <- .abrir_contexto(cfg)
  on.exit(DBI::dbDisconnect(con))
  
  meio <- DBI::dbGetQuery(
    con,
    "SELECT id, codigo_curto, nome, bloqueado_preparo
     FROM meios WHERE id = ?;",
    params = list(as.integer(cfg$meio_id))
  )
  cat("=== Meio ===\n")
  cat("  ID:       ", meio$id[1], "\n")
  cat("  Codigo:   ", meio$codigo_curto[1], "\n")
  cat("  Nome:     ", meio$nome[1], "\n")
  cat("  Bloqueado:", if (meio$bloqueado_preparo[1] == 1L) "SIM" else "nao",
      "\n\n")
  
  comp <- DBI::dbGetQuery(
    con,
    "SELECT mc.id AS mc_id, mc.componente_id, c.nome, c.formula,
            mc.concentracao_mg_l, mc.valor_original, mc.unidade_original,
            mc.ordem_exibicao, mc.observacao
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = ?
     ORDER BY mc.ordem_exibicao, c.nome;",
    params = list(as.integer(cfg$meio_id))
  )
  
  cat("=== Composicao (", nrow(comp), " componentes) ===\n", sep = "")
  if (nrow(comp) == 0L) {
    cat("  (sem componentes)\n")
  } else {
    print(comp[, c("mc_id", "componente_id", "nome", "concentracao_mg_l",
                   "ordem_exibicao")],
          row.names = FALSE)
  }
  cat("\n")
  
  invisible(comp)
}

# ---------------------------------------------------------------------
# ADICIONAR componente
# ---------------------------------------------------------------------

adicionar_componente <- function(componente_id, concentracao_mg_l,
                                 ordem_exibicao = NULL,
                                 valor_original = NA_real_,
                                 unidade_original = NA_character_,
                                 observacao = NA_character_,
                                 dry_run = TRUE,
                                 cfg = CONFIG) {
  componente_id <- as.integer(componente_id)
  meio_id <- as.integer(cfg$meio_id)
  
  if (!is.numeric(concentracao_mg_l) || is.na(concentracao_mg_l) ||
      concentracao_mg_l <= 0) {
    stop("concentracao_mg_l deve ser > 0.", call. = FALSE)
  }
  if (concentracao_mg_l > 1e6) {
    stop("concentracao_mg_l acima do limite do schema (1.000.000 mg/L).",
         call. = FALSE)
  }
  if (!is.na(unidade_original) &&
      !unidade_original %in% .UNIDADES_VALIDAS) {
    stop("unidade_original invalida: '", unidade_original,
         "'. Aceitas: ", paste(.UNIDADES_VALIDAS, collapse = ", "),
         call. = FALSE)
  }
  
  con <- .abrir_contexto(cfg)
  on.exit(DBI::dbDisconnect(con))
  
  comp <- DBI::dbGetQuery(
    con,
    "SELECT id, nome FROM componentes WHERE id = ?;",
    params = list(componente_id)
  )
  if (nrow(comp) == 0L) {
    stop("Componente nao encontrado (id=", componente_id, ").",
         call. = FALSE)
  }
  
  ja_existe <- DBI::dbGetQuery(
    con,
    "SELECT id FROM meio_componentes
     WHERE meio_id = ? AND componente_id = ?;",
    params = list(meio_id, componente_id)
  )
  if (nrow(ja_existe) > 0L) {
    stop("Componente '", comp$nome[1], "' ja esta no meio. ",
         "Use alterar_concentracao() em vez de adicionar.",
         call. = FALSE)
  }
  
  if (is.null(ordem_exibicao) || is.na(ordem_exibicao)) {
    ordem_max <- DBI::dbGetQuery(
      con,
      "SELECT COALESCE(MAX(ordem_exibicao), 0) AS m
       FROM meio_componentes WHERE meio_id = ?;",
      params = list(meio_id)
    )$m[1]
    ordem_exibicao <- as.integer(ordem_max) + 1L
  }
  ordem_exibicao <- as.integer(ordem_exibicao)
  
  cat("\n[", if (dry_run) "DRY RUN" else "APLICANDO", "] ",
      "Adicionar componente:\n", sep = "")
  cat("  Componente:  ", comp$nome[1], " (id=", componente_id, ")\n",
      sep = "")
  cat("  Concentracao:", concentracao_mg_l, "mg/L\n")
  cat("  Ordem:       ", ordem_exibicao, "\n\n")
  
  if (dry_run) {
    cat("Nada foi alterado. Rode com dry_run = FALSE para aplicar.\n")
    return(invisible(NULL))
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "INSERT INTO meio_componentes
         (meio_id, componente_id, concentracao_mg_l,
          valor_original, unidade_original, ordem_exibicao, observacao)
       VALUES (?, ?, ?, ?, ?, ?, ?);",
      params = list(
        meio_id, componente_id, as.numeric(concentracao_mg_l),
        if (is.na(valor_original)) NA_real_ else as.numeric(valor_original),
        if (is.na(unidade_original)) NA_character_
        else as.character(unidade_original),
        ordem_exibicao,
        if (is.na(observacao)) NA_character_ else as.character(observacao)
      )
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto, valores_depois)
       VALUES (?, ?, 'meio_componentes', ?, 'INSERT', ?, ?);",
      params = list(
        TENANT_DEFAULT_ID, as.integer(cfg$operador_id), meio_id,
        paste("dev/06_editar_composicao: ADD |", cfg$motivo),
        sprintf('{"componente_id":%d,"concentracao_mg_l":%s,"ordem":%d}',
                componente_id, concentracao_mg_l, ordem_exibicao)
      )
    )
    DBI::dbCommit(con)
    cat("OK. Componente adicionado e audit_log registrado.\n")
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha: ", conditionMessage(e), call. = FALSE)
  })
  
  invisible(NULL)
}

# ---------------------------------------------------------------------
# ALTERAR concentracao de componente existente
# ---------------------------------------------------------------------

alterar_concentracao <- function(componente_id, nova_concentracao_mg_l,
                                 dry_run = TRUE,
                                 cfg = CONFIG) {
  componente_id <- as.integer(componente_id)
  meio_id <- as.integer(cfg$meio_id)
  
  if (!is.numeric(nova_concentracao_mg_l) ||
      is.na(nova_concentracao_mg_l) ||
      nova_concentracao_mg_l <= 0) {
    stop("nova_concentracao_mg_l deve ser > 0.", call. = FALSE)
  }
  if (nova_concentracao_mg_l > 1e6) {
    stop("nova_concentracao_mg_l acima do limite do schema (1.000.000 mg/L).",
         call. = FALSE)
  }
  
  con <- .abrir_contexto(cfg)
  on.exit(DBI::dbDisconnect(con))
  
  atual <- DBI::dbGetQuery(
    con,
    "SELECT mc.id AS mc_id, mc.concentracao_mg_l, c.nome
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = ? AND mc.componente_id = ?;",
    params = list(meio_id, componente_id)
  )
  if (nrow(atual) == 0L) {
    stop("Componente id=", componente_id,
         " nao faz parte deste meio.", call. = FALSE)
  }
  
  antiga <- atual$concentracao_mg_l[1]
  cat("\n[", if (dry_run) "DRY RUN" else "APLICANDO", "] ",
      "Alterar concentracao:\n", sep = "")
  cat("  Componente:", atual$nome[1], "\n")
  cat("  De:        ", antiga, "mg/L\n")
  cat("  Para:      ", nova_concentracao_mg_l, "mg/L\n")
  cat("  Delta:     ", nova_concentracao_mg_l - antiga, "mg/L\n\n")
  
  if (dry_run) {
    cat("Nada foi alterado. Rode com dry_run = FALSE para aplicar.\n")
    return(invisible(NULL))
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE meio_componentes SET concentracao_mg_l = ?
       WHERE id = ?;",
      params = list(as.numeric(nova_concentracao_mg_l), atual$mc_id[1])
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto, valores_antes, valores_depois)
       VALUES (?, ?, 'meio_componentes', ?, 'UPDATE', ?, ?, ?);",
      params = list(
        TENANT_DEFAULT_ID, as.integer(cfg$operador_id), meio_id,
        paste("dev/06_editar_composicao: UPDATE |", cfg$motivo),
        sprintf('{"componente_id":%d,"concentracao_mg_l":%s}',
                componente_id, antiga),
        sprintf('{"componente_id":%d,"concentracao_mg_l":%s}',
                componente_id, nova_concentracao_mg_l)
      )
    )
    DBI::dbCommit(con)
    cat("OK. Concentracao alterada e audit_log registrado.\n")
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha: ", conditionMessage(e), call. = FALSE)
  })
  
  invisible(NULL)
}

# ---------------------------------------------------------------------
# REMOVER componente do meio
# ---------------------------------------------------------------------

remover_componente <- function(componente_id,
                               dry_run = TRUE,
                               cfg = CONFIG) {
  componente_id <- as.integer(componente_id)
  meio_id <- as.integer(cfg$meio_id)
  
  con <- .abrir_contexto(cfg)
  on.exit(DBI::dbDisconnect(con))
  
  atual <- DBI::dbGetQuery(
    con,
    "SELECT mc.id AS mc_id, mc.concentracao_mg_l, mc.ordem_exibicao,
            c.nome
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = ? AND mc.componente_id = ?;",
    params = list(meio_id, componente_id)
  )
  if (nrow(atual) == 0L) {
    stop("Componente id=", componente_id,
         " nao faz parte deste meio.", call. = FALSE)
  }
  
  cat("\n[", if (dry_run) "DRY RUN" else "APLICANDO", "] ",
      "Remover componente:\n", sep = "")
  cat("  Componente:", atual$nome[1], "\n")
  cat("  Que tinha: ", atual$concentracao_mg_l[1], "mg/L\n\n")
  
  if (dry_run) {
    cat("Nada foi alterado. Rode com dry_run = FALSE para aplicar.\n")
    return(invisible(NULL))
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "DELETE FROM meio_componentes WHERE id = ?;",
      params = list(atual$mc_id[1])
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto, valores_antes)
       VALUES (?, ?, 'meio_componentes', ?, 'DELETE', ?, ?);",
      params = list(
        TENANT_DEFAULT_ID, as.integer(cfg$operador_id), meio_id,
        paste("dev/06_editar_composicao: DELETE |", cfg$motivo),
        sprintf(
          '{"componente_id":%d,"concentracao_mg_l":%s,"ordem":%d}',
          componente_id, atual$concentracao_mg_l[1],
          atual$ordem_exibicao[1]
        )
      )
    )
    DBI::dbCommit(con)
    cat("OK. Componente removido e audit_log registrado.\n")
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha: ", conditionMessage(e), call. = FALSE)
  })
  
  invisible(NULL)
}

# ---------------------------------------------------------------------
# RENUMERAR ordem_exibicao (compacta para 1..N na ordem atual)
# ---------------------------------------------------------------------

renumerar_ordem <- function(dry_run = TRUE, cfg = CONFIG) {
  meio_id <- as.integer(cfg$meio_id)
  
  con <- .abrir_contexto(cfg)
  on.exit(DBI::dbDisconnect(con))
  
  comp <- DBI::dbGetQuery(
    con,
    "SELECT id, componente_id, ordem_exibicao
     FROM meio_componentes WHERE meio_id = ?
     ORDER BY ordem_exibicao, id;",
    params = list(meio_id)
  )
  if (nrow(comp) == 0L) {
    cat("Meio sem componentes.\n")
    return(invisible(NULL))
  }
  
  comp$nova_ordem <- seq_len(nrow(comp))
  precisa_mudar <- comp$ordem_exibicao != comp$nova_ordem
  n_mudar <- sum(precisa_mudar)
  
  cat("\n[", if (dry_run) "DRY RUN" else "APLICANDO", "] ",
      "Renumerar ordem_exibicao:\n", sep = "")
  cat("  Componentes que mudam:", n_mudar, "de", nrow(comp), "\n\n")
  
  if (n_mudar == 0L) {
    cat("Ja esta numerado sequencialmente. Nada a fazer.\n")
    return(invisible(NULL))
  }
  
  print(comp[precisa_mudar, c("componente_id", "ordem_exibicao",
                              "nova_ordem")],
        row.names = FALSE)
  
  if (dry_run) {
    cat("\nNada foi alterado. Rode com dry_run = FALSE para aplicar.\n")
    return(invisible(NULL))
  }
  
  DBI::dbBegin(con)
  tryCatch({
    for (i in which(precisa_mudar)) {
      DBI::dbExecute(
        con,
        "UPDATE meio_componentes SET ordem_exibicao = ? WHERE id = ?;",
        params = list(as.integer(comp$nova_ordem[i]), comp$id[i])
      )
    }
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto)
       VALUES (?, ?, 'meio_componentes', ?, 'UPDATE', ?);",
      params = list(
        TENANT_DEFAULT_ID, as.integer(cfg$operador_id), meio_id,
        sprintf("dev/06_editar_composicao: RENUMERAR (%d itens) | %s",
                n_mudar, cfg$motivo)
      )
    )
    DBI::dbCommit(con)
    cat("\nOK.", n_mudar, "linhas atualizadas e audit_log registrado.\n")
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha: ", conditionMessage(e), call. = FALSE)
  })
  
  invisible(NULL)
}

# ---------------------------------------------------------------------
# EXEMPLO DE USO (comentado - descomente e adapte)
# ---------------------------------------------------------------------

# CONFIG$meio_id     <- 1L
# CONFIG$operador_id <- 1L
# CONFIG$motivo      <- "Correcao apos revisao de Murashige 1962 Tabela II"
#
# ler_composicao()
#
# alterar_concentracao(componente_id = 3,
#                      nova_concentracao_mg_l = 100,
#                      dry_run = TRUE)
#
# # alterar_concentracao(componente_id = 3,
# #                      nova_concentracao_mg_l = 100,
# #                      dry_run = FALSE)
#
# ler_composicao()