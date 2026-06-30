#' Utilitarios de CRUD em meios de cultura
#'
#' Funcoes puras (recebem conexao DBI, retornam dados ou levantam erro).
#' Toda escrita registra no audit_log na mesma transacao.
#'
#' Composicao (meio_componentes) NAO e editavel por esta camada no MVP.
#' Para alteracoes de composicao, ver dev/06_editar_composicao.R.
#'
#' Permissoes encapsuladas em .checar_papel_*. Modulos Shiny tambem devem
#' validar permissoes na UI (ocultar botoes), mas a verificacao backend
#' aqui e a fonte de verdade.
#'
#' @noRd

# ---------------------------------------------------------------------
# Constantes e helpers
# ---------------------------------------------------------------------

.PAPEIS_SUPERVISOR_OU_ADMIN <- c("supervisor", "admin")
.PAPEIS_ADMIN <- c("admin")

#' Verifica papel do solicitante; levanta erro se nao autorizado
#' @noRd
.checar_papel <- function(con, solicitante_id, papeis_aceitos,
                          tenant_id = TENANT_DEFAULT_ID) {
  solicitante_id <- as.integer(solicitante_id)
  s <- DBI::dbGetQuery(
    con,
    "SELECT papel, deleted_at, ativo FROM operadores
     WHERE id = ? AND tenant_id = ?;",
    params = list(solicitante_id, tenant_id)
  )
  if (nrow(s) == 0L) {
    stop("Solicitante nao encontrado (id=", solicitante_id, ").",
         call. = FALSE)
  }
  if (!is.na(s$deleted_at[1]) || s$ativo[1] != 1L) {
    stop("Solicitante arquivado ou inativo.", call. = FALSE)
  }
  if (!s$papel[1] %in% papeis_aceitos) {
    stop("Permissao insuficiente. Acao requer papel: ",
         paste(papeis_aceitos, collapse = " ou "),
         ". Voce e: ", s$papel[1], ".", call. = FALSE)
  }
  invisible(NULL)
}

#' Gera timestamp ISO 8601 UTC atual
#' @noRd
.now_utc <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

# ---------------------------------------------------------------------
# Listagem
# ---------------------------------------------------------------------

#' Lista meios do tenant
#'
#' @param incluir_arquivados Se TRUE, inclui meios com deleted_at preenchido.
#' @param filtro_categoria Opcional: id da categoria para filtrar.
#' @return data.frame com colunas: id, codigo_curto, nome, categoria_id,
#'   categoria_nome, ph_alvo, flag_incerteza, nota_incerteza,
#'   bloqueado_preparo, deleted_at, n_componentes, criado_em, atualizado_em.
#' @noRd
listar_meios <- function(con, incluir_arquivados = FALSE,
                         filtro_categoria = NULL,
                         tenant_id = TENANT_DEFAULT_ID) {
  where_clauses <- c("m.tenant_id = ?")
  params <- list(tenant_id)
  
  if (!incluir_arquivados) {
    where_clauses <- c(where_clauses, "m.deleted_at IS NULL")
  }
  if (!is.null(filtro_categoria) && !is.na(filtro_categoria)) {
    where_clauses <- c(where_clauses, "m.categoria_id = ?")
    params <- c(params, list(as.integer(filtro_categoria)))
  }
  
  sql <- paste0(
    "SELECT m.id, m.codigo_curto, m.nome,
            m.categoria_id, cm.nome AS categoria_nome,
            m.ph_alvo, m.flag_incerteza, m.nota_incerteza,
            m.bloqueado_preparo, m.deleted_at,
            (SELECT COUNT(*) FROM meio_componentes mc
             WHERE mc.meio_id = m.id) AS n_componentes,
            m.criado_em, m.atualizado_em
     FROM meios m
     LEFT JOIN categorias_meio cm ON cm.id = m.categoria_id
     WHERE ", paste(where_clauses, collapse = " AND "),
    " ORDER BY m.codigo_curto;"
  )
  DBI::dbGetQuery(con, sql, params = params)
}

#' Busca textual em meios (nome ou codigo_curto)
#' @noRd
buscar_meios <- function(con, termo, incluir_arquivados = FALSE,
                         tenant_id = TENANT_DEFAULT_ID) {
  if (is.null(termo) || !nzchar(trimws(termo))) {
    return(listar_meios(con, incluir_arquivados = incluir_arquivados,
                        tenant_id = tenant_id))
  }
  termo_like <- paste0("%", trimws(termo), "%")
  where_clauses <- c("m.tenant_id = ?",
                     "(m.codigo_curto LIKE ? OR m.nome LIKE ?)")
  params <- list(tenant_id, termo_like, termo_like)
  
  if (!incluir_arquivados) {
    where_clauses <- c(where_clauses, "m.deleted_at IS NULL")
  }
  
  sql <- paste0(
    "SELECT m.id, m.codigo_curto, m.nome,
            m.categoria_id, cm.nome AS categoria_nome,
            m.ph_alvo, m.flag_incerteza, m.nota_incerteza,
            m.bloqueado_preparo, m.deleted_at,
            (SELECT COUNT(*) FROM meio_componentes mc
             WHERE mc.meio_id = m.id) AS n_componentes,
            m.criado_em, m.atualizado_em
     FROM meios m
     LEFT JOIN categorias_meio cm ON cm.id = m.categoria_id
     WHERE ", paste(where_clauses, collapse = " AND "),
    " ORDER BY m.codigo_curto;"
  )
  DBI::dbGetQuery(con, sql, params = params)
}

#' Detalhe completo de um meio (com composicao)
#'
#' Retorna lista com:
#'   - meio: data.frame com 1 linha
#'   - composicao: data.frame com componentes
#' @noRd
detalhe_meio <- function(con, meio_id, tenant_id = TENANT_DEFAULT_ID) {
  meio_id <- as.integer(meio_id)
  meio <- DBI::dbGetQuery(
    con,
    "SELECT m.id, m.codigo_curto, m.nome,
            m.categoria_id, cm.nome AS categoria_nome,
            m.ph_alvo, m.flag_incerteza, m.nota_incerteza,
            m.bloqueado_preparo, m.deleted_at,
            m.criado_em, m.atualizado_em
     FROM meios m
     LEFT JOIN categorias_meio cm ON cm.id = m.categoria_id
     WHERE m.id = ? AND m.tenant_id = ?;",
    params = list(meio_id, tenant_id)
  )
  if (nrow(meio) == 0L) {
    stop("Meio nao encontrado (id=", meio_id, ").", call. = FALSE)
  }
  
  composicao <- DBI::dbGetQuery(
    con,
    "SELECT mc.componente_id, c.nome, c.formula, c.cas, c.massa_molar,
            cc.nome AS categoria_nome,
            mc.concentracao_mg_l, mc.valor_original, mc.unidade_original,
            mc.ordem_exibicao, mc.observacao
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     LEFT JOIN categorias_componente cc ON cc.id = c.categoria_id
     WHERE mc.meio_id = ?
     ORDER BY mc.ordem_exibicao, c.nome;",
    params = list(meio_id)
  )
  
  list(meio = meio, composicao = composicao)
}

# ---------------------------------------------------------------------
# Categorias (lookup)
# ---------------------------------------------------------------------

#' Lista categorias de meio (para dropdowns)
#' @noRd
listar_categorias_meio <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  DBI::dbGetQuery(
    con,
    "SELECT id, nome FROM categorias_meio
     WHERE tenant_id = ? ORDER BY nome;",
    params = list(tenant_id)
  )
}

# ---------------------------------------------------------------------
# Atualizacao de campos do meio
# ---------------------------------------------------------------------

#' Atualiza campos do meio (nao mexe em composicao)
#'
#' Campos editaveis:
#'   - nome, codigo_curto, categoria_id, ph_alvo
#'   - flag_incerteza, nota_incerteza
#'   - bloqueado_preparo (apenas admin pode SETAR para 0; supervisor pode setar 1)
#'
#' Validacoes:
#'   - codigo_curto unico no tenant (excluindo o proprio)
#'   - categoria_id existe
#'   - ph_alvo entre 0 e 14 ou NA
#'
#' @param campos Lista nomeada com chaves entre as editaveis acima.
#'   Apenas as chaves presentes sao atualizadas.
#' @noRd
atualizar_meio <- function(con, meio_id, campos, solicitante_id,
                           tenant_id = TENANT_DEFAULT_ID) {
  meio_id <- as.integer(meio_id)
  solicitante_id <- as.integer(solicitante_id)
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN, tenant_id)
  
  if (!is.list(campos) || length(campos) == 0L) {
    stop("Campos vazios; nada a atualizar.", call. = FALSE)
  }
  
  # Carrega meio atual
  meio_atual <- DBI::dbGetQuery(
    con, "SELECT * FROM meios WHERE id = ? AND tenant_id = ?;",
    params = list(meio_id, tenant_id)
  )
  if (nrow(meio_atual) == 0L) {
    stop("Meio nao encontrado.", call. = FALSE)
  }
  if (!is.na(meio_atual$deleted_at[1])) {
    stop("Meio arquivado nao pode ser editado. Restaure antes.",
         call. = FALSE)
  }
  
  # Carrega papel do solicitante (para regra de bloqueado_preparo)
  papel_solicitante <- DBI::dbGetQuery(
    con, "SELECT papel FROM operadores WHERE id = ?;",
    params = list(solicitante_id)
  )$papel[1]
  
  # Validacoes campo a campo
  campos_validos <- c("nome", "codigo_curto", "categoria_id", "ph_alvo",
                      "flag_incerteza", "nota_incerteza",
                      "bloqueado_preparo")
  invalidos <- setdiff(names(campos), campos_validos)
  if (length(invalidos) > 0L) {
    stop("Campos nao editaveis: ",
         paste(invalidos, collapse = ", "), ".", call. = FALSE)
  }
  
  if (!is.null(campos$codigo_curto)) {
    cc <- trimws(campos$codigo_curto)
    if (!nzchar(cc)) stop("codigo_curto nao pode ser vazio.", call. = FALSE)
    if (nchar(cc) > 16L) stop("codigo_curto muito longo (max 16).",
                              call. = FALSE)
    dup <- DBI::dbGetQuery(
      con,
      "SELECT id FROM meios WHERE tenant_id = ? AND codigo_curto = ?
         AND id != ? LIMIT 1;",
      params = list(tenant_id, cc, meio_id)
    )
    if (nrow(dup) > 0L) {
      stop("Ja existe outro meio com codigo_curto '", cc, "'.",
           call. = FALSE)
    }
    campos$codigo_curto <- cc
  }
  
  if (!is.null(campos$nome)) {
    nm <- trimws(campos$nome)
    if (!nzchar(nm)) stop("nome nao pode ser vazio.", call. = FALSE)
    if (nchar(nm) > 200L) stop("nome muito longo (max 200).", call. = FALSE)
    campos$nome <- nm
  }
  
  if (!is.null(campos$categoria_id) && !is.na(campos$categoria_id)) {
    cat_ok <- DBI::dbGetQuery(
      con,
      "SELECT id FROM categorias_meio WHERE id = ? AND tenant_id = ?;",
      params = list(as.integer(campos$categoria_id), tenant_id)
    )
    if (nrow(cat_ok) == 0L) {
      stop("Categoria nao existe (id=", campos$categoria_id, ").",
           call. = FALSE)
    }
    campos$categoria_id <- as.integer(campos$categoria_id)
  }
  
  if (!is.null(campos$ph_alvo) && !is.na(campos$ph_alvo)) {
    ph <- as.numeric(campos$ph_alvo)
    if (is.na(ph) || ph < 0 || ph > 14) {
      stop("ph_alvo deve estar entre 0 e 14 (ou NA).", call. = FALSE)
    }
    campos$ph_alvo <- ph
  }
  
  if (!is.null(campos$flag_incerteza)) {
    fi <- as.integer(campos$flag_incerteza)
    if (!fi %in% c(0L, 1L)) {
      stop("flag_incerteza deve ser 0 ou 1.", call. = FALSE)
    }
    campos$flag_incerteza <- fi
  }
  
  # Regra: apenas admin pode setar bloqueado_preparo para 0 (desbloquear).
  # Supervisor pode setar para 1 (bloquear), mas nao desbloquear.
  if (!is.null(campos$bloqueado_preparo)) {
    bp <- as.integer(campos$bloqueado_preparo)
    if (!bp %in% c(0L, 1L)) {
      stop("bloqueado_preparo deve ser 0 ou 1.", call. = FALSE)
    }
    bp_atual <- as.integer(meio_atual$bloqueado_preparo[1])
    if (bp == 0L && bp_atual == 1L && papel_solicitante != "admin") {
      stop("Apenas admin pode desbloquear um meio.", call. = FALSE)
    }
    campos$bloqueado_preparo <- bp
  }
  
  # Monta UPDATE dinamico
  set_clauses <- paste0(names(campos), " = ?")
  set_clauses <- c(set_clauses, "atualizado_em = ?")
  valores <- c(unname(campos), list(.now_utc()))
  
  DBI::dbBegin(con)
  tryCatch({
    sql <- paste0(
      "UPDATE meios SET ", paste(set_clauses, collapse = ", "),
      " WHERE id = ? AND tenant_id = ?;"
    )
    DBI::dbExecute(con, sql,
                   params = c(valores, list(meio_id, tenant_id)))
    
    # Audit log: registra valores antes/depois apenas dos campos modificados
    antes_json <- .meio_para_json(meio_atual, names(campos))
    depois <- meio_atual
    for (k in names(campos)) depois[[k]] <- campos[[k]]
    depois_json <- .meio_para_json(depois, names(campos))
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          valores_antes, valores_depois)
       VALUES (?, ?, 'meios', ?, 'UPDATE', ?, ?);",
      params = list(tenant_id, solicitante_id, meio_id,
                    antes_json, depois_json)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao atualizar meio: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

#' Serializa subset de campos do meio para JSON (audit log)
#' @noRd
.meio_para_json <- function(meio_row, campos) {
  pares <- sapply(campos, function(k) {
    valor <- meio_row[[k]][1]
    if (is.null(valor) || (length(valor) == 1L && is.na(valor))) {
      sprintf('"%s":null', k)
    } else if (is.numeric(valor)) {
      sprintf('"%s":%s', k, valor)
    } else {
      sprintf('"%s":"%s"', k, gsub('"', '\\\\"', as.character(valor)))
    }
  })
  paste0("{", paste(pares, collapse = ","), "}")
}

# ---------------------------------------------------------------------
# Arquivamento (soft delete) e restauracao
# ---------------------------------------------------------------------

#' Arquiva um meio (soft delete via deleted_at)
#'
#' Supervisor+ pode arquivar. Meio arquivado nao aparece em listagens
#' padrao nem pode ser usado em preparos.
#' @noRd
arquivar_meio <- function(con, meio_id, solicitante_id,
                          tenant_id = TENANT_DEFAULT_ID) {
  meio_id <- as.integer(meio_id)
  solicitante_id <- as.integer(solicitante_id)
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN, tenant_id)
  
  meio <- DBI::dbGetQuery(
    con, "SELECT id, codigo_curto, deleted_at FROM meios
          WHERE id = ? AND tenant_id = ?;",
    params = list(meio_id, tenant_id)
  )
  if (nrow(meio) == 0L) stop("Meio nao encontrado.", call. = FALSE)
  if (!is.na(meio$deleted_at[1])) {
    stop("Meio ja esta arquivado.", call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE meios SET deleted_at = ?, atualizado_em = ?
       WHERE id = ?;",
      params = list(.now_utc(), .now_utc(), meio_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'meios', ?, 'DELETE', 'Arquivamento (soft delete)');",
      params = list(tenant_id, solicitante_id, meio_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao arquivar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

#' Restaura meio arquivado (apenas admin)
#' @noRd
restaurar_meio <- function(con, meio_id, solicitante_id,
                           tenant_id = TENANT_DEFAULT_ID) {
  meio_id <- as.integer(meio_id)
  solicitante_id <- as.integer(solicitante_id)
  .checar_papel(con, solicitante_id, .PAPEIS_ADMIN, tenant_id)
  
  meio <- DBI::dbGetQuery(
    con, "SELECT id, deleted_at FROM meios
          WHERE id = ? AND tenant_id = ?;",
    params = list(meio_id, tenant_id)
  )
  if (nrow(meio) == 0L) stop("Meio nao encontrado.", call. = FALSE)
  if (is.na(meio$deleted_at[1])) {
    stop("Meio nao esta arquivado.", call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE meios SET deleted_at = NULL, atualizado_em = ?
       WHERE id = ?;",
      params = list(.now_utc(), meio_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'meios', ?, 'UPDATE', 'Restauracao de arquivamento');",
      params = list(tenant_id, solicitante_id, meio_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao restaurar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}
# ---------------------------------------------------------------------
# Criar meio
# ---------------------------------------------------------------------

#' Cria um novo meio de cultura
#'
#' Apenas supervisores e admins podem criar meios. Operadores comuns
#' recebem erro de permissao.
#'
#' Validacoes:
#'   - Solicitante existe, esta ativo e tem papel supervisor/admin
#'   - nome: nao vazio (apos trim), ate 200 chars
#'   - codigo_curto: nao vazio, alfanumerico + underscore, ate 20 chars,
#'     unico por tenant (mesmo entre meios arquivados, devido a UNIQUE constraint)
#'   - categoria_id: existe no mesmo tenant
#'   - pop_id (opcional): se fornecido, existe no mesmo tenant
#'   - ph_alvo (opcional): se fornecido, entre 0 e 14
#'   - flag_incerteza: 0 ou 1; se 1 entao nota_incerteza obrigatoria
#'   - bloqueado_preparo: 0 ou 1; se 1 entao nota_incerteza obrigatoria
#'
#' INSERT em meios + INSERT em audit_log na mesma transacao.
#'
#' @return Inteiro: id do novo meio.
#' @noRd
criar_meio <- function(con, nome, codigo_curto, categoria_id,
                       criado_por_id,
                       pop_id = NA_integer_,
                       referencia = NA_character_,
                       doi = NA_character_,
                       ph_alvo = NA_real_,
                       observacoes = NA_character_,
                       flag_incerteza = 0L,
                       nota_incerteza = NA_character_,
                       bloqueado_preparo = 0L,
                       tenant_id = TENANT_DEFAULT_ID) {
  
  # ---- Validacao: solicitante ----
  criado_por_id <- as.integer(criado_por_id)
  solic <- DBI::dbGetQuery(
    con,
    "SELECT id, papel, ativo, deleted_at FROM operadores
     WHERE id = ? AND tenant_id = ?;",
    params = list(criado_por_id, tenant_id)
  )
  if (nrow(solic) == 0L) {
    stop("Solicitante nao encontrado.", call. = FALSE)
  }
  if (solic$ativo[1] != 1L || !is.na(solic$deleted_at[1])) {
    stop("Solicitante inativo ou arquivado.", call. = FALSE)
  }
  if (!solic$papel[1] %in% c("supervisor", "admin")) {
    stop("Apenas supervisores e admins podem criar meios. ",
         "Solicite ao supervisor responsavel.", call. = FALSE)
  }
  
  # ---- Validacao: nome ----
  if (is.null(nome) || length(nome) != 1L || is.na(nome) ||
      !is.character(nome)) {
    stop("Nome do meio e obrigatorio.", call. = FALSE)
  }
  nome <- trimws(nome)
  if (!nzchar(nome)) {
    stop("Nome do meio nao pode ser vazio.", call. = FALSE)
  }
  if (nchar(nome) > 200L) {
    stop("Nome do meio nao pode ter mais de 200 caracteres.", call. = FALSE)
  }
  
  # ---- Validacao: codigo_curto ----
  if (is.null(codigo_curto) || length(codigo_curto) != 1L ||
      is.na(codigo_curto) || !is.character(codigo_curto)) {
    stop("Codigo curto e obrigatorio.", call. = FALSE)
  }
  codigo_curto <- trimws(codigo_curto)
  if (!nzchar(codigo_curto)) {
    stop("Codigo curto nao pode ser vazio.", call. = FALSE)
  }
  if (nchar(codigo_curto) > 20L) {
    stop("Codigo curto nao pode ter mais de 20 caracteres.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_]+$", codigo_curto)) {
    stop("Codigo curto so pode ter letras, numeros e underscore. ",
         "Valor recebido: '", codigo_curto, "'.", call. = FALSE)
  }
  
  # Verifica unicidade (constraint UNIQUE pega arquivados tambem)
  existente <- DBI::dbGetQuery(
    con,
    "SELECT id, deleted_at FROM meios
     WHERE tenant_id = ? AND codigo_curto = ?;",
    params = list(tenant_id, codigo_curto)
  )
  if (nrow(existente) > 0L) {
    if (is.na(existente$deleted_at[1])) {
      stop("Ja existe um meio ativo com codigo '", codigo_curto, "'.",
           call. = FALSE)
    } else {
      stop("Codigo '", codigo_curto,
           "' pertence a um meio arquivado. Restaure-o ou escolha outro codigo.",
           call. = FALSE)
    }
  }
  
  # ---- Validacao: categoria ----
  categoria_id <- as.integer(categoria_id)
  cat_row <- DBI::dbGetQuery(
    con,
    "SELECT id FROM categorias_meio WHERE id = ? AND tenant_id = ?;",
    params = list(categoria_id, tenant_id)
  )
  if (nrow(cat_row) == 0L) {
    stop("Categoria nao encontrada (id=", categoria_id, ").", call. = FALSE)
  }
  
  # ---- Validacao: pop_id (opcional) ----
  if (!is.na(pop_id)) {
    pop_id <- as.integer(pop_id)
    pop_row <- DBI::dbGetQuery(
      con,
      "SELECT id FROM pops WHERE id = ? AND tenant_id = ?;",
      params = list(pop_id, tenant_id)
    )
    if (nrow(pop_row) == 0L) {
      stop("POP nao encontrado (id=", pop_id, ").", call. = FALSE)
    }
  } else {
    pop_id <- NA_integer_
  }
  
  # ---- Validacao: ph_alvo (opcional) ----
  if (!is.na(ph_alvo)) {
    if (!is.numeric(ph_alvo) || length(ph_alvo) != 1L ||
        ph_alvo < 0 || ph_alvo > 14) {
      stop("ph_alvo deve estar entre 0 e 14 (ou NA).", call. = FALSE)
    }
    ph_alvo <- as.numeric(ph_alvo)
  } else {
    ph_alvo <- NA_real_
  }
  
  # ---- Validacao: flag_incerteza ----
  if (!is.numeric(flag_incerteza) && !is.integer(flag_incerteza)) {
    stop("flag_incerteza deve ser 0 ou 1.", call. = FALSE)
  }
  flag_incerteza <- as.integer(flag_incerteza)
  if (!flag_incerteza %in% c(0L, 1L)) {
    stop("flag_incerteza deve ser 0 ou 1.", call. = FALSE)
  }
  
  # ---- Validacao: bloqueado_preparo ----
  if (!is.numeric(bloqueado_preparo) && !is.integer(bloqueado_preparo)) {
    stop("bloqueado_preparo deve ser 0 ou 1.", call. = FALSE)
  }
  bloqueado_preparo <- as.integer(bloqueado_preparo)
  if (!bloqueado_preparo %in% c(0L, 1L)) {
    stop("bloqueado_preparo deve ser 0 ou 1.", call. = FALSE)
  }
  
  # ---- Validacao: nota_incerteza obrigatoria se flag ou bloqueio ativo ----
  if (flag_incerteza == 1L || bloqueado_preparo == 1L) {
    if (is.na(nota_incerteza) || !is.character(nota_incerteza) ||
        !nzchar(trimws(nota_incerteza))) {
      stop("Nota de incerteza e obrigatoria quando ",
           "flag_incerteza ou bloqueado_preparo esta ativo.", call. = FALSE)
    }
    nota_incerteza <- trimws(nota_incerteza)
  } else {
    nota_incerteza <- if (is.na(nota_incerteza)) NA_character_
    else trimws(nota_incerteza)
    if (!is.na(nota_incerteza) && !nzchar(nota_incerteza)) {
      nota_incerteza <- NA_character_
    }
  }
  
  # ---- Normalizacao de campos opcionais string ----
  normalizar_opcional <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x) || !is.character(x)) {
      return(NA_character_)
    }
    x <- trimws(x)
    if (!nzchar(x)) NA_character_ else x
  }
  referencia <- normalizar_opcional(referencia)
  doi <- normalizar_opcional(doi)
  observacoes <- normalizar_opcional(observacoes)
  
  # ---- Execucao em transacao ----
  agora <- .now_utc()
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "INSERT INTO meios
         (tenant_id, categoria_id, pop_id, nome, codigo_curto,
          referencia, doi, ph_alvo, observacoes,
          flag_incerteza, nota_incerteza,
          criado_por, criado_em, atualizado_em,
          bloqueado_preparo)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
      params = list(
        tenant_id, categoria_id, pop_id, nome, codigo_curto,
        referencia, doi, ph_alvo, observacoes,
        flag_incerteza, nota_incerteza,
        criado_por_id, agora, agora,
        bloqueado_preparo
      )
    )
    
    novo_id <- DBI::dbGetQuery(
      con,
      "SELECT id FROM meios
       WHERE tenant_id = ? AND codigo_curto = ?;",
      params = list(tenant_id, codigo_curto)
    )$id[1]
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          valores_depois)
       VALUES (?, ?, 'meios', ?, 'INSERT', ?);",
      params = list(
        tenant_id, criado_por_id, as.integer(novo_id),
        sprintf(
          '{"codigo":"%s","nome":"%s","categoria_id":%d,"flag_incerteza":%d,"bloqueado_preparo":%d}',
          codigo_curto,
          gsub('"', '\\\\"', nome),
          categoria_id,
          flag_incerteza,
          bloqueado_preparo
        )
      )
    )
    
    DBI::dbCommit(con)
    as.integer(novo_id)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao criar meio: ", conditionMessage(e), call. = FALSE)
  })
}