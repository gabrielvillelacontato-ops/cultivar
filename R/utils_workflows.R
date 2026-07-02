#' Utilitarios de workflows
#'
#' Workflows sao pipelines pre-definidos com sequencia ordenada de
#' etapas, cada uma referenciando um meio de cultura.
#'
#' Este arquivo cobre CRUD completo de workflows e etapas, respeitando:
#'   - Soft delete via workflows.deleted_at
#'   - Ordenacao 1..N contigua em workflow_etapas.ordem
#'   - UNIQUE (workflow_id, ordem) do schema
#'   - ALCOA+ via audit_log
#'
#' Convencoes reaproveitadas do projeto:
#'   - .checar_papel() para autorizacao
#'   - .now_utc() para timestamps
#'   - .PAPEIS_SUPERVISOR_OU_ADMIN, .PAPEIS_ADMIN para regras
#'
#' @noRd

# =====================================================================
# LEITURA
# =====================================================================

#' Lista workflows do tenant
#'
#' @param con Conexao DBI.
#' @param incluir_arquivados Se TRUE, inclui workflows com deleted_at.
#'   Default FALSE.
#' @param tenant_id Tenant. Default TENANT_DEFAULT_ID.
#'
#' @return data.frame com colunas: id, nome, referencia, doi, descricao,
#'   deleted_at, criado_em, criado_por, n_etapas. Ordenado por nome asc.
#'
#' @noRd
listar_workflows <- function(con, incluir_arquivados = FALSE,
                             tenant_id = TENANT_DEFAULT_ID) {
  where_arq <- if (isTRUE(incluir_arquivados)) {
    ""
  } else {
    " AND w.deleted_at IS NULL"
  }
  
  sql <- paste0(
    "SELECT w.id, w.nome, w.referencia, w.doi, w.descricao,
            w.deleted_at, w.criado_em, w.criado_por,
            (SELECT COUNT(*) FROM workflow_etapas we
             WHERE we.workflow_id = w.id) AS n_etapas
     FROM workflows w
     WHERE w.tenant_id = ?", where_arq,
    " ORDER BY w.nome ASC;"
  )
  
  DBI::dbGetQuery(con, sql, params = list(tenant_id))
}

#' Busca workflows por termo (nome, referencia)
#'
#' Se termo vazio ou NULL, delega para listar_workflows.
#'
#' @noRd
buscar_workflows <- function(con, termo = "",
                             incluir_arquivados = FALSE,
                             tenant_id = TENANT_DEFAULT_ID) {
  if (is.null(termo) || !nzchar(trimws(termo))) {
    return(listar_workflows(con,
                            incluir_arquivados = incluir_arquivados,
                            tenant_id = tenant_id))
  }
  
  termo_like <- paste0("%", trimws(termo), "%")
  where_clauses <- c("w.tenant_id = ?",
                     "(w.nome LIKE ? OR w.referencia LIKE ?)")
  params <- list(tenant_id, termo_like, termo_like)
  
  if (!incluir_arquivados) {
    where_clauses <- c(where_clauses, "w.deleted_at IS NULL")
  }
  
  sql <- paste0(
    "SELECT w.id, w.nome, w.referencia, w.doi, w.descricao,
            w.deleted_at, w.criado_em, w.criado_por,
            (SELECT COUNT(*) FROM workflow_etapas we
             WHERE we.workflow_id = w.id) AS n_etapas
     FROM workflows w
     WHERE ", paste(where_clauses, collapse = " AND "),
    " ORDER BY w.nome ASC;"
  )
  
  DBI::dbGetQuery(con, sql, params = params)
}

#' Detalhe completo de um workflow (com etapas)
#'
#' Retorna workflow mesmo se arquivado. Da erro apenas se nao existir
#' ou pertencer a outro tenant.
#'
#' @return list(workflow, etapas) onde:
#'   - workflow: data.frame com 1 linha (todos os campos + criado_por_nome)
#'   - etapas: data.frame ordenado por ordem asc, com meio_codigo e meio_nome
#'
#' @noRd
detalhe_workflow <- function(con, workflow_id,
                             tenant_id = TENANT_DEFAULT_ID) {
  workflow_id <- as.integer(workflow_id)
  
  workflow <- DBI::dbGetQuery(
    con,
    "SELECT w.id, w.tenant_id, w.nome, w.referencia, w.doi,
            w.descricao, w.deleted_at, w.criado_em, w.criado_por,
            op.nome AS criado_por_nome
     FROM workflows w
     LEFT JOIN operadores op ON op.id = w.criado_por
     WHERE w.id = ? AND w.tenant_id = ?;",
    params = list(workflow_id, tenant_id)
  )
  if (nrow(workflow) == 0L) {
    stop("Workflow nao encontrado (id=", workflow_id, ").",
         call. = FALSE)
  }
  
  etapas <- DBI::dbGetQuery(
    con,
    "SELECT we.id, we.workflow_id, we.meio_id, we.ordem,
            we.nome_etapa, we.duracao, we.condicoes, we.observacoes,
            m.codigo_curto AS meio_codigo, m.nome AS meio_nome
     FROM workflow_etapas we
     JOIN meios m ON m.id = we.meio_id
     WHERE we.workflow_id = ?
     ORDER BY we.ordem ASC;",
    params = list(workflow_id)
  )
  
  list(workflow = workflow, etapas = etapas)
}

# =====================================================================
# ESCRITA - WORKFLOWS
# =====================================================================

#' Cria um workflow novo
#'
#' Permissao: supervisor+.
#'
#' @param con Conexao DBI.
#' @param nome Nome do workflow (unico no tenant, considerando arquivados).
#' @param referencia Referencia bibliografica (opcional).
#' @param doi DOI (opcional).
#' @param descricao Descricao livre (opcional).
#' @param criado_por_id ID do solicitante (deve ser supervisor+).
#' @param tenant_id Tenant.
#'
#' @return integer: id do novo workflow.
#' @noRd
criar_workflow <- function(con, nome,
                           referencia = NA_character_,
                           doi = NA_character_,
                           descricao = NA_character_,
                           criado_por_id,
                           tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  
  # Validacoes de formato
  if (is.null(nome) || is.na(nome) || !nzchar(trimws(as.character(nome)))) {
    stop("Nome do workflow obrigatorio.", call. = FALSE)
  }
  nome <- trimws(as.character(nome))
  if (nchar(nome) > 200L) {
    stop("Nome excede 200 caracteres.", call. = FALSE)
  }
  
  # Autorizacao
  .checar_papel(con, criado_por_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  # Unicidade (considera arquivados)
  existe <- DBI::dbGetQuery(
    con,
    "SELECT id FROM workflows WHERE tenant_id = ? AND nome = ? LIMIT 1;",
    params = list(tenant_id, nome)
  )
  if (nrow(existe) > 0L) {
    stop("Ja existe workflow com nome '", nome, "' neste tenant ",
         "(incluindo arquivados).", call. = FALSE)
  }
  
  # Normaliza opcionais
  ref_final <- if (is.null(referencia) || is.na(referencia) ||
                   !nzchar(trimws(as.character(referencia)))) {
    NA_character_
  } else {
    trimws(as.character(referencia))
  }
  doi_final <- if (is.null(doi) || is.na(doi) ||
                   !nzchar(trimws(as.character(doi)))) {
    NA_character_
  } else {
    trimws(as.character(doi))
  }
  desc_final <- if (is.null(descricao) || is.na(descricao) ||
                    !nzchar(trimws(as.character(descricao)))) {
    NA_character_
  } else {
    as.character(descricao)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "INSERT INTO workflows
         (tenant_id, nome, referencia, doi, descricao, criado_por)
       VALUES (?, ?, ?, ?, ?, ?);",
      params = list(tenant_id, nome, ref_final, doi_final, desc_final,
                    as.integer(criado_por_id))
    )
    novo_id <- DBI::dbGetQuery(
      con,
      "SELECT id FROM workflows WHERE tenant_id = ? AND nome = ?;",
      params = list(tenant_id, nome)
    )$id[1]
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, valores_depois)
       VALUES (?, ?, 'workflows', ?, 'INSERT', ?);",
      params = list(
        tenant_id, as.integer(criado_por_id), as.integer(novo_id),
        sprintf('{"nome":"%s"}',
                gsub('"', '\\\\"', nome, fixed = FALSE))
      )
    )
    DBI::dbCommit(con)
    as.integer(novo_id)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao criar workflow: ", conditionMessage(e), call. = FALSE)
  })
}

#' Atualiza campos permitidos de um workflow
#'
#' Permissao: supervisor+.
#' Campos permitidos: nome, referencia, doi, descricao.
#' Nao aceita: workflow arquivado (restaure primeiro).
#'
#' @param campos list com subset de {nome, referencia, doi, descricao}.
#' @noRd
atualizar_workflow <- function(con, workflow_id, campos,
                               solicitante_id,
                               tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  workflow_id <- as.integer(workflow_id)
  
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  # Verifica existencia e estado
  wf <- DBI::dbGetQuery(
    con,
    "SELECT id, nome, referencia, doi, descricao, deleted_at
     FROM workflows WHERE id = ? AND tenant_id = ?;",
    params = list(workflow_id, tenant_id)
  )
  if (nrow(wf) == 0L) {
    stop("Workflow nao encontrado (id=", workflow_id, ").",
         call. = FALSE)
  }
  if (!is.na(wf$deleted_at[1])) {
    stop("Workflow esta arquivado. Restaure antes de editar.",
         call. = FALSE)
  }
  
  # Whitelist de campos
  campos_ok <- c("nome", "referencia", "doi", "descricao")
  campos <- campos[intersect(names(campos), campos_ok)]
  if (length(campos) == 0L) {
    stop("Nenhum campo valido para atualizar. ",
         "Aceitos: ", paste(campos_ok, collapse = ", "), ".",
         call. = FALSE)
  }
  # Salvaguarda extra contra injecao
  stopifnot(all(names(campos) %in% campos_ok))
  
  # Se nome muda, valida
  if ("nome" %in% names(campos)) {
    novo_nome <- campos$nome
    if (is.null(novo_nome) || is.na(novo_nome) ||
        !nzchar(trimws(as.character(novo_nome)))) {
      stop("Nome do workflow nao pode ser vazio.", call. = FALSE)
    }
    novo_nome <- trimws(as.character(novo_nome))
    if (nchar(novo_nome) > 200L) {
      stop("Nome excede 200 caracteres.", call. = FALSE)
    }
    if (!identical(novo_nome, wf$nome[1])) {
      conflito <- DBI::dbGetQuery(
        con,
        "SELECT id FROM workflows
         WHERE tenant_id = ? AND nome = ? AND id != ? LIMIT 1;",
        params = list(tenant_id, novo_nome, workflow_id)
      )
      if (nrow(conflito) > 0L) {
        stop("Ja existe outro workflow com nome '", novo_nome,
             "' neste tenant.", call. = FALSE)
      }
    }
    campos$nome <- novo_nome
  }
  
  # Normaliza NA em string para os outros campos
  for (cmp in c("referencia", "doi", "descricao")) {
    if (cmp %in% names(campos)) {
      v <- campos[[cmp]]
      if (is.null(v) || is.na(v) || !nzchar(trimws(as.character(v)))) {
        campos[[cmp]] <- NA_character_
      } else {
        campos[[cmp]] <- if (cmp == "descricao") {
          as.character(v)
        } else {
          trimws(as.character(v))
        }
      }
    }
  }
  
  # Monta UPDATE dinamico
  set_clauses <- paste0(names(campos), " = ?")
  set_sql <- paste(set_clauses, collapse = ", ")
  params <- unname(campos)
  params[[length(params) + 1]] <- workflow_id
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      paste0("UPDATE workflows SET ", set_sql, " WHERE id = ?;"),
      params = params
    )
    contexto <- sprintf(
      "Atualizacao de workflow: campos [%s]",
      paste(names(campos), collapse = ", ")
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'workflows', ?, 'UPDATE', ?);",
      params = list(tenant_id, as.integer(solicitante_id),
                    workflow_id, contexto)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao atualizar workflow: ", conditionMessage(e),
         call. = FALSE)
  })
  invisible(NULL)
}

#' Arquiva um workflow (soft delete)
#'
#' Permissao: admin.
#'
#' @noRd
arquivar_workflow <- function(con, workflow_id, solicitante_id,
                              tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  workflow_id <- as.integer(workflow_id)
  
  .checar_papel(con, solicitante_id, .PAPEIS_ADMIN, tenant_id)
  
  wf <- DBI::dbGetQuery(
    con,
    "SELECT id, nome, deleted_at FROM workflows
     WHERE id = ? AND tenant_id = ?;",
    params = list(workflow_id, tenant_id)
  )
  if (nrow(wf) == 0L) {
    stop("Workflow nao encontrado (id=", workflow_id, ").",
         call. = FALSE)
  }
  if (!is.na(wf$deleted_at[1])) {
    stop("Workflow ja esta arquivado.", call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE workflows SET deleted_at = ? WHERE id = ?;",
      params = list(.now_utc(), workflow_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'workflows', ?, 'DELETE',
               'Arquivamento (soft delete)');",
      params = list(tenant_id, as.integer(solicitante_id), workflow_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao arquivar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

#' Restaura um workflow arquivado
#'
#' Permissao: admin.
#'
#' @noRd
restaurar_workflow <- function(con, workflow_id, solicitante_id,
                               tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  workflow_id <- as.integer(workflow_id)
  
  .checar_papel(con, solicitante_id, .PAPEIS_ADMIN, tenant_id)
  
  wf <- DBI::dbGetQuery(
    con,
    "SELECT id, nome, deleted_at FROM workflows
     WHERE id = ? AND tenant_id = ?;",
    params = list(workflow_id, tenant_id)
  )
  if (nrow(wf) == 0L) {
    stop("Workflow nao encontrado (id=", workflow_id, ").",
         call. = FALSE)
  }
  if (is.na(wf$deleted_at[1])) {
    stop("Workflow nao esta arquivado.", call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE workflows SET deleted_at = NULL WHERE id = ?;",
      params = list(workflow_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto)
       VALUES (?, ?, 'workflows', ?, 'UPDATE',
               'Restauracao apos arquivamento');",
      params = list(tenant_id, as.integer(solicitante_id), workflow_id)
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao restaurar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}
# =====================================================================
# ESCRITA - ETAPAS
# =====================================================================

# ---------------------------------------------------------------------
# Constante: offset para renumeracao (evita conflito com UNIQUE)
# CHECK do schema exige ordem >= 1, entao nao podemos usar negacao.
# Limite pratico: menos de OFFSET etapas por workflow.
# ---------------------------------------------------------------------

.WF_OFFSET_RENUM <- 10000L

# ---------------------------------------------------------------------
# Helper interno: valida meio existe, pertence ao tenant e nao arquivado
# ---------------------------------------------------------------------

#' @noRd
.wf_validar_meio <- function(con, meio_id, tenant_id) {
  m <- DBI::dbGetQuery(
    con,
    "SELECT id, codigo_curto, deleted_at FROM meios
     WHERE id = ? AND tenant_id = ?;",
    params = list(as.integer(meio_id), tenant_id)
  )
  if (nrow(m) == 0L) {
    stop("Meio nao encontrado (id=", meio_id, ").", call. = FALSE)
  }
  if (!is.na(m$deleted_at[1])) {
    stop("Meio '", m$codigo_curto[1], "' esta arquivado. ",
         "Restaure o meio antes de referencia-lo em etapa.",
         call. = FALSE)
  }
  invisible(m$codigo_curto[1])
}

# ---------------------------------------------------------------------
# Helper interno: valida workflow existe, pertence ao tenant, nao arq.
# ---------------------------------------------------------------------

#' @noRd
.wf_validar_workflow_editavel <- function(con, workflow_id, tenant_id) {
  wf <- DBI::dbGetQuery(
    con,
    "SELECT id, nome, deleted_at FROM workflows
     WHERE id = ? AND tenant_id = ?;",
    params = list(as.integer(workflow_id), tenant_id)
  )
  if (nrow(wf) == 0L) {
    stop("Workflow nao encontrado (id=", workflow_id, ").",
         call. = FALSE)
  }
  if (!is.na(wf$deleted_at[1])) {
    stop("Workflow '", wf$nome[1], "' esta arquivado. ",
         "Restaure antes de editar etapas.", call. = FALSE)
  }
  invisible(wf$nome[1])
}

# ---------------------------------------------------------------------
# adicionar_etapa (append no final)
# ---------------------------------------------------------------------

#' Adiciona uma etapa ao final de um workflow
#'
#' Permissao: supervisor+.
#'
#' @param nome_etapa Descricao curta (obrigatorio).
#' @param duracao Texto livre, opcional (ex: "2 dias", "48h").
#' @param condicoes Texto livre, opcional (ex: "22C escuro").
#' @param observacoes Texto livre, opcional.
#'
#' @return integer: id da nova etapa.
#' @noRd
adicionar_etapa <- function(con, workflow_id, meio_id, nome_etapa,
                            duracao = NA_character_,
                            condicoes = NA_character_,
                            observacoes = NA_character_,
                            solicitante_id,
                            tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  workflow_id <- as.integer(workflow_id)
  meio_id <- as.integer(meio_id)
  
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  if (is.null(nome_etapa) || is.na(nome_etapa) ||
      !nzchar(trimws(as.character(nome_etapa)))) {
    stop("nome_etapa obrigatorio.", call. = FALSE)
  }
  nome_etapa <- trimws(as.character(nome_etapa))
  
  .wf_validar_workflow_editavel(con, workflow_id, tenant_id)
  .wf_validar_meio(con, meio_id, tenant_id)
  
  # Ordem = max atual + 1
  max_ordem <- DBI::dbGetQuery(
    con,
    "SELECT COALESCE(MAX(ordem), 0) AS m
     FROM workflow_etapas WHERE workflow_id = ?;",
    params = list(workflow_id)
  )$m[1]
  nova_ordem <- as.integer(max_ordem) + 1L
  
  if (nova_ordem >= .WF_OFFSET_RENUM) {
    stop("Workflow atingiu limite de ", .WF_OFFSET_RENUM - 1L,
         " etapas.", call. = FALSE)
  }
  
  # Normaliza opcionais
  nz <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) {
      NA_character_
    } else {
      trimws(as.character(x))
    }
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "INSERT INTO workflow_etapas
         (workflow_id, meio_id, ordem, nome_etapa,
          duracao, condicoes, observacoes)
       VALUES (?, ?, ?, ?, ?, ?, ?);",
      params = list(workflow_id, meio_id, nova_ordem, nome_etapa,
                    nz(duracao), nz(condicoes), nz(observacoes))
    )
    novo_id <- DBI::dbGetQuery(
      con,
      "SELECT id FROM workflow_etapas
       WHERE workflow_id = ? AND ordem = ?;",
      params = list(workflow_id, nova_ordem)
    )$id[1]
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto)
       VALUES (?, ?, 'workflow_etapas', ?, 'INSERT', ?);",
      params = list(
        tenant_id, as.integer(solicitante_id), as.integer(novo_id),
        sprintf("Etapa adicionada ao workflow %d em ordem %d",
                workflow_id, nova_ordem)
      )
    )
    DBI::dbCommit(con)
    as.integer(novo_id)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao adicionar etapa: ", conditionMessage(e),
         call. = FALSE)
  })
}

# ---------------------------------------------------------------------
# inserir_etapa (insere em posicao especifica, renumera posteriores)
# ---------------------------------------------------------------------

#' Insere uma etapa em posicao especifica, renumerando as posteriores
#'
#' Permissao: supervisor+.
#'
#' Estrategia: usa offset temporario (.WF_OFFSET_RENUM) para escapar
#' da UNIQUE (workflow_id, ordem) durante a renumeracao.
#'
#' @param ordem_desejada Posicao 1-indexada onde inserir. Deve estar
#'   entre 1 e (n_etapas_atual + 1). Se for n_etapas + 1, equivale a
#'   adicionar_etapa (append no final).
#'
#' @return integer: id da nova etapa.
#' @noRd
inserir_etapa <- function(con, workflow_id, ordem_desejada, meio_id,
                          nome_etapa,
                          duracao = NA_character_,
                          condicoes = NA_character_,
                          observacoes = NA_character_,
                          solicitante_id,
                          tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  workflow_id <- as.integer(workflow_id)
  meio_id <- as.integer(meio_id)
  ordem_desejada <- as.integer(ordem_desejada)
  
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  if (is.null(nome_etapa) || is.na(nome_etapa) ||
      !nzchar(trimws(as.character(nome_etapa)))) {
    stop("nome_etapa obrigatorio.", call. = FALSE)
  }
  nome_etapa <- trimws(as.character(nome_etapa))
  
  if (is.na(ordem_desejada) || ordem_desejada < 1L) {
    stop("ordem_desejada deve ser >= 1.", call. = FALSE)
  }
  
  .wf_validar_workflow_editavel(con, workflow_id, tenant_id)
  .wf_validar_meio(con, meio_id, tenant_id)
  
  # Conta etapas atuais
  n_atual <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM workflow_etapas WHERE workflow_id = ?;",
    params = list(workflow_id)
  )$n[1]
  
  if (ordem_desejada > n_atual + 1L) {
    stop("ordem_desejada = ", ordem_desejada,
         " excede posicao maxima permitida (", n_atual + 1L, ").",
         call. = FALSE)
  }
  if (n_atual + 1L >= .WF_OFFSET_RENUM) {
    stop("Workflow atingiu limite de ", .WF_OFFSET_RENUM - 1L,
         " etapas.", call. = FALSE)
  }
  
  nz <- function(x) {
    if (is.null(x) || is.na(x) || !nzchar(trimws(as.character(x)))) {
      NA_character_
    } else {
      trimws(as.character(x))
    }
  }
  
  DBI::dbBegin(con)
  tryCatch({
    # Passo 1: desloca etapas com ordem >= desejada para faixa alta
    DBI::dbExecute(
      con,
      "UPDATE workflow_etapas
       SET ordem = ordem + ?
       WHERE workflow_id = ? AND ordem >= ?;",
      params = list(.WF_OFFSET_RENUM, workflow_id, ordem_desejada)
    )
    # Passo 2: insere na posicao desejada
    DBI::dbExecute(
      con,
      "INSERT INTO workflow_etapas
         (workflow_id, meio_id, ordem, nome_etapa,
          duracao, condicoes, observacoes)
       VALUES (?, ?, ?, ?, ?, ?, ?);",
      params = list(workflow_id, meio_id, ordem_desejada, nome_etapa,
                    nz(duracao), nz(condicoes), nz(observacoes))
    )
    novo_id <- DBI::dbGetQuery(
      con,
      "SELECT id FROM workflow_etapas
       WHERE workflow_id = ? AND ordem = ?;",
      params = list(workflow_id, ordem_desejada)
    )$id[1]
    # Passo 3: desloca de volta, agora ordem = original + 1
    DBI::dbExecute(
      con,
      "UPDATE workflow_etapas
       SET ordem = ordem - ? + 1
       WHERE workflow_id = ? AND ordem >= ?;",
      params = list(.WF_OFFSET_RENUM, workflow_id, .WF_OFFSET_RENUM)
    )
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto)
       VALUES (?, ?, 'workflow_etapas', ?, 'INSERT', ?);",
      params = list(
        tenant_id, as.integer(solicitante_id), as.integer(novo_id),
        sprintf("Etapa inserida no workflow %d em ordem %d (renumerou %d posteriores)",
                workflow_id, ordem_desejada,
                as.integer(n_atual - ordem_desejada + 1L))
      )
    )
    DBI::dbCommit(con)
    as.integer(novo_id)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao inserir etapa: ", conditionMessage(e),
         call. = FALSE)
  })
}

# ---------------------------------------------------------------------
# atualizar_etapa (edita campos de conteudo, NAO mexe ordem)
# ---------------------------------------------------------------------

#' Atualiza campos de conteudo de uma etapa
#'
#' Permissao: supervisor+.
#' Campos permitidos: meio_id, nome_etapa, duracao, condicoes, observacoes.
#' NAO permite mudar: workflow_id, ordem (usar inserir/reordenar).
#'
#' Se meio_id muda, valida que o novo meio existe e nao esta arquivado.
#'
#' @param campos list com subset dos campos permitidos.
#' @noRd
atualizar_etapa <- function(con, etapa_id, campos,
                            solicitante_id,
                            tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  etapa_id <- as.integer(etapa_id)
  
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  # Etapa existe? Pertence a workflow deste tenant e nao arquivado?
  etapa <- DBI::dbGetQuery(
    con,
    "SELECT we.id, we.workflow_id, we.meio_id, we.ordem, we.nome_etapa,
            we.duracao, we.condicoes, we.observacoes,
            w.tenant_id, w.deleted_at AS wf_deleted_at, w.nome AS wf_nome
     FROM workflow_etapas we
     JOIN workflows w ON w.id = we.workflow_id
     WHERE we.id = ?;",
    params = list(etapa_id)
  )
  if (nrow(etapa) == 0L) {
    stop("Etapa nao encontrada (id=", etapa_id, ").", call. = FALSE)
  }
  if (etapa$tenant_id[1] != tenant_id) {
    stop("Etapa nao pertence a este tenant.", call. = FALSE)
  }
  if (!is.na(etapa$wf_deleted_at[1])) {
    stop("Workflow '", etapa$wf_nome[1], "' esta arquivado. ",
         "Restaure antes de editar etapas.", call. = FALSE)
  }
  
  # Whitelist
  campos_ok <- c("meio_id", "nome_etapa", "duracao",
                 "condicoes", "observacoes")
  campos <- campos[intersect(names(campos), campos_ok)]
  if (length(campos) == 0L) {
    stop("Nenhum campo valido para atualizar. ",
         "Aceitos: ", paste(campos_ok, collapse = ", "), ".",
         call. = FALSE)
  }
  stopifnot(all(names(campos) %in% campos_ok))
  
  # Se meio_id muda, valida
  if ("meio_id" %in% names(campos)) {
    novo_meio <- suppressWarnings(as.integer(campos$meio_id))
    if (is.na(novo_meio)) {
      stop("meio_id invalido.", call. = FALSE)
    }
    .wf_validar_meio(con, novo_meio, tenant_id)
    campos$meio_id <- novo_meio
  }
  
  # Se nome_etapa muda, valida
  if ("nome_etapa" %in% names(campos)) {
    novo_nome <- campos$nome_etapa
    if (is.null(novo_nome) || is.na(novo_nome) ||
        !nzchar(trimws(as.character(novo_nome)))) {
      stop("nome_etapa nao pode ser vazio.", call. = FALSE)
    }
    campos$nome_etapa <- trimws(as.character(novo_nome))
  }
  
  # Normaliza NAs em texto para os opcionais
  for (cmp in c("duracao", "condicoes", "observacoes")) {
    if (cmp %in% names(campos)) {
      v <- campos[[cmp]]
      if (is.null(v) || is.na(v) ||
          !nzchar(trimws(as.character(v)))) {
        campos[[cmp]] <- NA_character_
      } else {
        campos[[cmp]] <- trimws(as.character(v))
      }
    }
  }
  
  # Monta UPDATE
  set_clauses <- paste0(names(campos), " = ?")
  set_sql <- paste(set_clauses, collapse = ", ")
  params <- unname(campos)
  params[[length(params) + 1]] <- etapa_id
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      paste0("UPDATE workflow_etapas SET ", set_sql, " WHERE id = ?;"),
      params = params
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto)
       VALUES (?, ?, 'workflow_etapas', ?, 'UPDATE', ?);",
      params = list(
        tenant_id, as.integer(solicitante_id), etapa_id,
        sprintf("Atualizacao de etapa (workflow %d, ordem %d): campos [%s]",
                etapa$workflow_id[1], etapa$ordem[1],
                paste(names(campos), collapse = ", "))
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao atualizar etapa: ", conditionMessage(e),
         call. = FALSE)
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------
# remover_etapa (remove e renumera posteriores)
# ---------------------------------------------------------------------

#' Remove uma etapa e renumera as posteriores para manter 1..N contigua
#'
#' Permissao: supervisor+.
#'
#' @noRd
remover_etapa <- function(con, etapa_id, solicitante_id,
                          tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  etapa_id <- as.integer(etapa_id)
  
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  etapa <- DBI::dbGetQuery(
    con,
    "SELECT we.id, we.workflow_id, we.ordem,
            w.tenant_id, w.deleted_at AS wf_deleted_at, w.nome AS wf_nome
     FROM workflow_etapas we
     JOIN workflows w ON w.id = we.workflow_id
     WHERE we.id = ?;",
    params = list(etapa_id)
  )
  if (nrow(etapa) == 0L) {
    stop("Etapa nao encontrada (id=", etapa_id, ").", call. = FALSE)
  }
  if (etapa$tenant_id[1] != tenant_id) {
    stop("Etapa nao pertence a este tenant.", call. = FALSE)
  }
  if (!is.na(etapa$wf_deleted_at[1])) {
    stop("Workflow '", etapa$wf_nome[1], "' esta arquivado. ",
         "Restaure antes de remover etapas.", call. = FALSE)
  }
  
  wf_id <- etapa$workflow_id[1]
  ordem_removida <- etapa$ordem[1]
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "DELETE FROM workflow_etapas WHERE id = ?;",
      params = list(etapa_id)
    )
    # Renumera posteriores: -1 em cada ordem > ordem_removida
    DBI::dbExecute(
      con,
      "UPDATE workflow_etapas
       SET ordem = ordem - 1
       WHERE workflow_id = ? AND ordem > ?;",
      params = list(wf_id, ordem_removida)
    )
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto)
       VALUES (?, ?, 'workflow_etapas', ?, 'DELETE', ?);",
      params = list(
        tenant_id, as.integer(solicitante_id), etapa_id,
        sprintf("Etapa removida do workflow %d (era ordem %d)",
                wf_id, ordem_removida)
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao remover etapa: ", conditionMessage(e),
         call. = FALSE)
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------
# reordenar_etapas (recebe vetor de IDs na nova sequencia)
# ---------------------------------------------------------------------

#' Reordena todas as etapas de um workflow
#'
#' Permissao: supervisor+.
#'
#' Recebe vetor de IDs de etapas na sequencia desejada. Valida que
#' contem exatamente todos os IDs atuais do workflow (sem faltar,
#' sem sobrar, sem duplicar).
#'
#' Estrategia: offset temporario (.WF_OFFSET_RENUM) para escapar da
#' UNIQUE constraint durante o reset das ordens.
#'
#' @param nova_ordem integer vector: IDs de etapas na sequencia
#'   desejada. Comprimento deve bater com n_etapas do workflow.
#'
#' @noRd
reordenar_etapas <- function(con, workflow_id, nova_ordem,
                             solicitante_id,
                             tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  workflow_id <- as.integer(workflow_id)
  nova_ordem <- as.integer(nova_ordem)
  
  .checar_papel(con, solicitante_id, .PAPEIS_SUPERVISOR_OU_ADMIN,
                tenant_id)
  
  .wf_validar_workflow_editavel(con, workflow_id, tenant_id)
  
  if (any(is.na(nova_ordem))) {
    stop("nova_ordem contem NA.", call. = FALSE)
  }
  if (length(nova_ordem) != length(unique(nova_ordem))) {
    stop("nova_ordem contem IDs duplicados.", call. = FALSE)
  }
  
  # IDs atuais das etapas do workflow
  atuais <- DBI::dbGetQuery(
    con,
    "SELECT id FROM workflow_etapas WHERE workflow_id = ?
     ORDER BY ordem ASC;",
    params = list(workflow_id)
  )$id
  
  if (length(atuais) == 0L) {
    stop("Workflow nao tem etapas para reordenar.", call. = FALSE)
  }
  if (length(nova_ordem) != length(atuais)) {
    stop("nova_ordem deve ter ", length(atuais),
         " IDs (workflow atual). Recebido: ", length(nova_ordem), ".",
         call. = FALSE)
  }
  if (!setequal(nova_ordem, atuais)) {
    stop("nova_ordem nao contem exatamente os IDs de etapas do ",
         "workflow. Verifique se falta ou sobra algum.",
         call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    # Passo 1: coloca todas as etapas em ordem alta unica (id + offset)
    # Garante que nenhum UPDATE subsequente vai violar UNIQUE.
    DBI::dbExecute(
      con,
      "UPDATE workflow_etapas
       SET ordem = ordem + ?
       WHERE workflow_id = ?;",
      params = list(.WF_OFFSET_RENUM, workflow_id)
    )
    # Passo 2: atualiza cada etapa para sua nova posicao 1..N
    for (i in seq_along(nova_ordem)) {
      DBI::dbExecute(
        con,
        "UPDATE workflow_etapas SET ordem = ? WHERE id = ?;",
        params = list(as.integer(i), as.integer(nova_ordem[i]))
      )
    }
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id,
          acao, contexto)
       VALUES (?, ?, 'workflows', ?, 'UPDATE', ?);",
      params = list(
        tenant_id, as.integer(solicitante_id), workflow_id,
        sprintf("Reordenacao de %d etapas do workflow %d",
                length(nova_ordem), workflow_id)
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao reordenar etapas: ", conditionMessage(e),
         call. = FALSE)
  })
  invisible(NULL)
}