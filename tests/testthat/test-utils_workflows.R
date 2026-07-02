# =====================================================================
# Testes de utils_workflows
# =====================================================================
# Cobertura:
#   - listar_workflows (com/sem arquivados, contagem de etapas)
#   - buscar_workflows (termo em nome ou referencia)
#   - detalhe_workflow (workflow + etapas, com JOIN em meios)
#   - criar_workflow, atualizar_workflow, arquivar/restaurar_workflow
#   - adicionar_etapa, inserir_etapa (com renumeracao)
#   - atualizar_etapa, remover_etapa, reordenar_etapas
# =====================================================================

# ---------------------------------------------------------------------
# Setup helpers
# ---------------------------------------------------------------------

.setup_banco_workflows <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  # Migration 001
  mig1 <- file.path(testthat::test_path(), "..", "..", "migrations",
                    "001_initial_schema.sql")
  sql1 <- paste(readLines(mig1, encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  sql1 <- db_strip_sql_comments(sql1)
  for (stmt in db_split_sql_statements(sql1)) {
    DBI::dbExecute(con, stmt)
  }
  
  # Migration 002 (bloqueado_preparo em meios - mesmo ALTER usado em outros testes)
  DBI::dbExecute(con,
                 "ALTER TABLE meios ADD COLUMN bloqueado_preparo INTEGER NOT NULL DEFAULT 0
     CHECK (bloqueado_preparo IN (0,1));"
  )
  
  # Migration 003 (soft delete + criado_por em workflows)
  DBI::dbExecute(con, "ALTER TABLE workflows ADD COLUMN deleted_at TEXT;")
  DBI::dbExecute(con,
                 "ALTER TABLE workflows ADD COLUMN criado_por INTEGER
     REFERENCES operadores(id);"
  )
  
  # Seeds minimos
  seed_categorias_componente(con)
  seed_categorias_meio(con)
  con
}

#' Cria operadores base (admin, supervisor, operador comum)
.setup_ops_workflows <- function(con) {
  id_admin <- criar_operador(con, "AdminWF", "1111", "admin")
  id_sup <- criar_operador(con, "SupervWF", "2222", "supervisor",
                           criado_por_id = id_admin)
  id_op <- criar_operador(con, "OperadorWF", "3333", "operador",
                          criado_por_id = id_admin)
  list(admin = id_admin, sup = id_sup, op = id_op)
}

#' Cria meios de teste para servir como fixture das etapas
.setup_meios_workflows <- function(con) {
  cat_id <- DBI::dbGetQuery(
    con,
    "SELECT id FROM categorias_meio WHERE nome = 'transformacao_genetica' LIMIT 1;"
  )$id[1]
  
  ids <- integer()
  for (cod in c("M1", "M2", "M3", "M4")) {
    DBI::dbExecute(
      con,
      "INSERT INTO meios (tenant_id, categoria_id, codigo_curto, nome,
                            flag_incerteza, bloqueado_preparo, deleted_at,
                            criado_em, atualizado_em)
       VALUES (1, ?, ?, ?, 0, 0, NULL, ?, ?);",
      params = list(cat_id, cod, paste("Meio", cod),
                    .now_utc(), .now_utc())
    )
    novo <- DBI::dbGetQuery(
      con,
      "SELECT id FROM meios WHERE codigo_curto = ?;",
      params = list(cod)
    )$id[1]
    ids <- c(ids, as.integer(novo))
  }
  names(ids) <- c("M1", "M2", "M3", "M4")
  ids
}

# ---------------------------------------------------------------------
# Testes: listar_workflows
# ---------------------------------------------------------------------

test_that("listar_workflows: sem workflows retorna data.frame vazio", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  
  res <- listar_workflows(con)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0L)
  expect_setequal(
    names(res),
    c("id", "nome", "referencia", "doi", "descricao",
      "deleted_at", "criado_em", "criado_por", "n_etapas")
  )
})

test_that("listar_workflows: retorna todos ordenados por nome asc", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  criar_workflow(con, "Zulu", criado_por_id = ops$admin)
  criar_workflow(con, "Alfa", criado_por_id = ops$admin)
  criar_workflow(con, "Mike", criado_por_id = ops$admin)
  
  res <- listar_workflows(con)
  expect_equal(nrow(res), 3L)
  expect_equal(res$nome, c("Alfa", "Mike", "Zulu"))
})

test_that("listar_workflows: default esconde arquivados", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id_a <- criar_workflow(con, "Ativo", criado_por_id = ops$admin)
  id_b <- criar_workflow(con, "Sera Arquivado", criado_por_id = ops$admin)
  arquivar_workflow(con, id_b, solicitante_id = ops$admin)
  
  res <- listar_workflows(con)
  expect_equal(nrow(res), 1L)
  expect_equal(res$nome[1], "Ativo")
})

test_that("listar_workflows: incluir_arquivados = TRUE retorna todos", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id_a <- criar_workflow(con, "Ativo", criado_por_id = ops$admin)
  id_b <- criar_workflow(con, "Arquivado", criado_por_id = ops$admin)
  arquivar_workflow(con, id_b, solicitante_id = ops$admin)
  
  res <- listar_workflows(con, incluir_arquivados = TRUE)
  expect_equal(nrow(res), 2L)
})

test_that("listar_workflows: n_etapas conta corretamente", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  id_wf <- criar_workflow(con, "Com Etapas", criado_por_id = ops$admin)
  adicionar_etapa(con, id_wf, meios["M1"], "E1", solicitante_id = ops$admin)
  adicionar_etapa(con, id_wf, meios["M2"], "E2", solicitante_id = ops$admin)
  adicionar_etapa(con, id_wf, meios["M3"], "E3", solicitante_id = ops$admin)
  
  id_wf2 <- criar_workflow(con, "Sem Etapas", criado_por_id = ops$admin)
  
  res <- listar_workflows(con)
  n_por_nome <- setNames(res$n_etapas, res$nome)
  expect_equal(n_por_nome[["Com Etapas"]], 3L)
  expect_equal(n_por_nome[["Sem Etapas"]], 0L)
})

# ---------------------------------------------------------------------
# Testes: buscar_workflows
# ---------------------------------------------------------------------

test_that("buscar_workflows: termo vazio delega para listar_workflows", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  criar_workflow(con, "Alfa", criado_por_id = ops$admin)
  criar_workflow(con, "Beta", criado_por_id = ops$admin)
  
  res_vazio <- buscar_workflows(con, termo = "")
  res_lista <- listar_workflows(con)
  expect_equal(nrow(res_vazio), nrow(res_lista))
})

test_that("buscar_workflows: filtra por match em nome", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  criar_workflow(con, "Cotton Pipeline", criado_por_id = ops$admin)
  criar_workflow(con, "Soja Basico", criado_por_id = ops$admin)
  
  res <- buscar_workflows(con, termo = "Cotton")
  expect_equal(nrow(res), 1L)
  expect_equal(res$nome[1], "Cotton Pipeline")
})

test_that("buscar_workflows: filtra por match em referencia", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  criar_workflow(con, "Alfa",
                 referencia = "Sunilkumar 2001",
                 criado_por_id = ops$admin)
  criar_workflow(con, "Beta",
                 referencia = "Outra Ref",
                 criado_por_id = ops$admin)
  
  res <- buscar_workflows(con, termo = "Sunilkumar")
  expect_equal(nrow(res), 1L)
  expect_equal(res$nome[1], "Alfa")
})

# ---------------------------------------------------------------------
# Testes: detalhe_workflow
# ---------------------------------------------------------------------

test_that("detalhe_workflow: id inexistente da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  
  expect_error(detalhe_workflow(con, 999L),
               "nao encontrado")
})

test_that("detalhe_workflow: retorna list(workflow, etapas)", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  id_wf <- criar_workflow(con, "Pipeline",
                          referencia = "Doe 2024",
                          criado_por_id = ops$admin)
  adicionar_etapa(con, id_wf, meios["M1"], "E1", solicitante_id = ops$admin)
  adicionar_etapa(con, id_wf, meios["M2"], "E2", solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, id_wf)
  expect_type(det, "list")
  expect_setequal(names(det), c("workflow", "etapas"))
  expect_equal(nrow(det$workflow), 1L)
  expect_equal(det$workflow$nome[1], "Pipeline")
  expect_equal(det$workflow$referencia[1], "Doe 2024")
  expect_equal(nrow(det$etapas), 2L)
  expect_equal(det$etapas$ordem, c(1L, 2L))
  # meio_codigo veio do JOIN
  expect_true("meio_codigo" %in% names(det$etapas))
})

test_that("detalhe_workflow: workflow arquivado retorna normalmente", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id_wf <- criar_workflow(con, "Sera Arquivado",
                          criado_por_id = ops$admin)
  arquivar_workflow(con, id_wf, solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, id_wf)
  expect_equal(nrow(det$workflow), 1L)
  expect_false(is.na(det$workflow$deleted_at[1]))
})
# ---------------------------------------------------------------------
# Testes: criar_workflow
# ---------------------------------------------------------------------

test_that("criar_workflow: caminho feliz retorna id integer", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Novo",
                       referencia = "Ref",
                       doi = "10.1/abc",
                       descricao = "Teste",
                       criado_por_id = ops$admin)
  expect_type(id, "integer")
  expect_true(id > 0L)
  
  wf <- DBI::dbGetQuery(con,
                        "SELECT nome, referencia, doi, descricao, criado_por
     FROM workflows WHERE id = ?;",
                        params = list(id))
  expect_equal(wf$nome[1], "Novo")
  expect_equal(wf$referencia[1], "Ref")
  expect_equal(wf$doi[1], "10.1/abc")
  expect_equal(wf$descricao[1], "Teste")
  expect_equal(wf$criado_por[1], ops$admin)
})

test_that("criar_workflow: nome vazio da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  expect_error(
    criar_workflow(con, "", criado_por_id = ops$admin),
    "obrigatorio"
  )
  expect_error(
    criar_workflow(con, "   ", criado_por_id = ops$admin),
    "obrigatorio"
  )
})

test_that("criar_workflow: nome duplicado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  criar_workflow(con, "Mesmo Nome", criado_por_id = ops$admin)
  expect_error(
    criar_workflow(con, "Mesmo Nome", criado_por_id = ops$admin),
    "Ja existe workflow"
  )
})

test_that("criar_workflow: nome duplicado considera arquivados", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Historia", criado_por_id = ops$admin)
  arquivar_workflow(con, id, solicitante_id = ops$admin)
  
  expect_error(
    criar_workflow(con, "Historia", criado_por_id = ops$admin),
    "incluindo arquivados"
  )
})

test_that("criar_workflow: operador comum nao pode criar", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  expect_error(
    criar_workflow(con, "Tentativa", criado_por_id = ops$op),
    "Permissao insuficiente|requer papel"
  )
})
test_that("criar_workflow: supervisor pode criar", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Por Supervisor",
                       criado_por_id = ops$sup)
  expect_true(id > 0L)
})

# ---------------------------------------------------------------------
# Testes: atualizar_workflow
# ---------------------------------------------------------------------

test_that("atualizar_workflow: atualiza campos permitidos", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Original", criado_por_id = ops$admin)
  
  atualizar_workflow(con, id,
                     campos = list(nome = "Renomeado",
                                   referencia = "Ref Nova",
                                   descricao = "Nova desc"),
                     solicitante_id = ops$admin)
  
  wf <- DBI::dbGetQuery(con,
                        "SELECT nome, referencia, descricao FROM workflows WHERE id = ?;",
                        params = list(id))
  expect_equal(wf$nome[1], "Renomeado")
  expect_equal(wf$referencia[1], "Ref Nova")
  expect_equal(wf$descricao[1], "Nova desc")
})

test_that("atualizar_workflow: workflow arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Arquiv", criado_por_id = ops$admin)
  arquivar_workflow(con, id, solicitante_id = ops$admin)
  
  expect_error(
    atualizar_workflow(con, id,
                       campos = list(descricao = "x"),
                       solicitante_id = ops$admin),
    "arquivado"
  )
})

test_that("atualizar_workflow: nome duplicado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  criar_workflow(con, "Existente", criado_por_id = ops$admin)
  id <- criar_workflow(con, "Editando", criado_por_id = ops$admin)
  
  expect_error(
    atualizar_workflow(con, id,
                       campos = list(nome = "Existente"),
                       solicitante_id = ops$admin),
    "Ja existe outro workflow"
  )
})

test_that("atualizar_workflow: operador comum nao pode", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Alvo", criado_por_id = ops$admin)
  
  expect_error(
    atualizar_workflow(con, id,
                       campos = list(descricao = "hack"),
                       solicitante_id = ops$op),
    "Permissao insuficiente|requer papel"
  )
})

test_that("atualizar_workflow: campos fora da whitelist sao ignorados", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Alvo", criado_por_id = ops$admin)
  
  # Passa tenant_id (nao aceito) - deve dar erro pois nada valido restou
  expect_error(
    atualizar_workflow(con, id,
                       campos = list(tenant_id = 999L),
                       solicitante_id = ops$admin),
    "Nenhum campo valido"
  )
})

test_that("atualizar_workflow: nome com espaco em branco vira erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Alvo", criado_por_id = ops$admin)
  
  expect_error(
    atualizar_workflow(con, id,
                       campos = list(nome = "   "),
                       solicitante_id = ops$admin),
    "vazio"
  )
})

# ---------------------------------------------------------------------
# Testes: arquivar_workflow
# ---------------------------------------------------------------------

test_that("arquivar_workflow: preenche deleted_at", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "Vai Arquivar", criado_por_id = ops$admin)
  arquivar_workflow(con, id, solicitante_id = ops$admin)
  
  wf <- DBI::dbGetQuery(con,
                        "SELECT deleted_at FROM workflows WHERE id = ?;",
                        params = list(id))
  expect_false(is.na(wf$deleted_at[1]))
})

test_that("arquivar_workflow: ja arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "X", criado_por_id = ops$admin)
  arquivar_workflow(con, id, solicitante_id = ops$admin)
  
  expect_error(
    arquivar_workflow(con, id, solicitante_id = ops$admin),
    "ja esta arquivado"
  )
})

test_that("arquivar_workflow: supervisor NAO pode (so admin)", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "X", criado_por_id = ops$admin)
  
  expect_error(
    arquivar_workflow(con, id, solicitante_id = ops$sup),
    "Permissao insuficiente|requer papel"
  )
})

# ---------------------------------------------------------------------
# Testes: restaurar_workflow
# ---------------------------------------------------------------------

test_that("restaurar_workflow: limpa deleted_at", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "X", criado_por_id = ops$admin)
  arquivar_workflow(con, id, solicitante_id = ops$admin)
  restaurar_workflow(con, id, solicitante_id = ops$admin)
  
  wf <- DBI::dbGetQuery(con,
                        "SELECT deleted_at FROM workflows WHERE id = ?;",
                        params = list(id))
  expect_true(is.na(wf$deleted_at[1]))
})

test_that("restaurar_workflow: nao arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "X", criado_por_id = ops$admin)
  
  expect_error(
    restaurar_workflow(con, id, solicitante_id = ops$admin),
    "nao esta arquivado"
  )
})

test_that("restaurar_workflow: supervisor NAO pode (so admin)", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  
  id <- criar_workflow(con, "X", criado_por_id = ops$admin)
  arquivar_workflow(con, id, solicitante_id = ops$admin)
  
  expect_error(
    restaurar_workflow(con, id, solicitante_id = ops$sup),
    "Permissao insuficiente|requer papel"
  )
})
# ---------------------------------------------------------------------
# Testes: adicionar_etapa
# ---------------------------------------------------------------------

test_that("adicionar_etapa: caminho feliz retorna id e ordem sequencial", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  
  id1 <- adicionar_etapa(con, wf, meios["M1"], "E1",
                         solicitante_id = ops$admin)
  id2 <- adicionar_etapa(con, wf, meios["M2"], "E2",
                         solicitante_id = ops$admin)
  id3 <- adicionar_etapa(con, wf, meios["M3"], "E3",
                         solicitante_id = ops$admin)
  
  expect_type(id1, "integer")
  expect_true(id1 > 0L && id2 > 0L && id3 > 0L)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$ordem, c(1L, 2L, 3L))
  expect_equal(det$etapas$nome_etapa, c("E1", "E2", "E3"))
})

test_that("adicionar_etapa: nome_etapa vazio da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  
  expect_error(
    adicionar_etapa(con, wf, meios["M1"], "",
                    solicitante_id = ops$admin),
    "obrigatorio"
  )
})

test_that("adicionar_etapa: workflow arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  arquivar_workflow(con, wf, solicitante_id = ops$admin)
  
  expect_error(
    adicionar_etapa(con, wf, meios["M1"], "E",
                    solicitante_id = ops$admin),
    "arquivado"
  )
})

test_that("adicionar_etapa: meio arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  # Arquiva o meio M1
  arquivar_meio(con, meios["M1"], solicitante_id = ops$admin)
  
  expect_error(
    adicionar_etapa(con, wf, meios["M1"], "E",
                    solicitante_id = ops$admin),
    "arquivado"
  )
})

test_that("adicionar_etapa: operador comum nao pode", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  
  expect_error(
    adicionar_etapa(con, wf, meios["M1"], "E",
                    solicitante_id = ops$op),
    "Permissao insuficiente|requer papel"
  )
})

# ---------------------------------------------------------------------
# Testes: inserir_etapa (COM RENUMERACAO)
# ---------------------------------------------------------------------

test_that("inserir_etapa: insere no meio renumera posteriores", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  adicionar_etapa(con, wf, meios["M1"], "A", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M2"], "B", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M3"], "C", solicitante_id = ops$admin)
  
  # Insere na posicao 2 (X vai entre A e B)
  inserir_etapa(con, wf, ordem_desejada = 2L,
                meio_id = meios["M4"], nome_etapa = "X",
                solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$ordem, c(1L, 2L, 3L, 4L))
  expect_equal(det$etapas$nome_etapa, c("A", "X", "B", "C"))
})

test_that("inserir_etapa: posicao = n+1 equivale a append", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  adicionar_etapa(con, wf, meios["M1"], "A", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M2"], "B", solicitante_id = ops$admin)
  
  inserir_etapa(con, wf, ordem_desejada = 3L,
                meio_id = meios["M3"], nome_etapa = "C",
                solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$ordem, c(1L, 2L, 3L))
  expect_equal(det$etapas$nome_etapa, c("A", "B", "C"))
})

test_that("inserir_etapa: posicao 1 empurra tudo", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  adicionar_etapa(con, wf, meios["M2"], "B", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M3"], "C", solicitante_id = ops$admin)
  
  inserir_etapa(con, wf, ordem_desejada = 1L,
                meio_id = meios["M1"], nome_etapa = "A",
                solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$nome_etapa, c("A", "B", "C"))
})

test_that("inserir_etapa: ordem 0 da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  
  expect_error(
    inserir_etapa(con, wf, ordem_desejada = 0L,
                  meio_id = meios["M1"], nome_etapa = "E",
                  solicitante_id = ops$admin),
    ">= 1"
  )
})

test_that("inserir_etapa: ordem > n+1 da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  adicionar_etapa(con, wf, meios["M1"], "A", solicitante_id = ops$admin)
  
  # Workflow tem 1 etapa, max permitido eh ordem 2
  expect_error(
    inserir_etapa(con, wf, ordem_desejada = 5L,
                  meio_id = meios["M2"], nome_etapa = "X",
                  solicitante_id = ops$admin),
    "excede posicao maxima"
  )
})

# ---------------------------------------------------------------------
# Testes: atualizar_etapa
# ---------------------------------------------------------------------

test_that("atualizar_etapa: atualiza nome_etapa e duracao", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_et <- adicionar_etapa(con, wf, meios["M1"], "Original",
                           solicitante_id = ops$admin)
  
  atualizar_etapa(con, id_et,
                  campos = list(nome_etapa = "Editado",
                                duracao = "2 semanas"),
                  solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$nome_etapa[1], "Editado")
  expect_equal(det$etapas$duracao[1], "2 semanas")
})

test_that("atualizar_etapa: pode mudar meio_id", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_et <- adicionar_etapa(con, wf, meios["M1"], "E",
                           solicitante_id = ops$admin)
  
  atualizar_etapa(con, id_et,
                  campos = list(meio_id = meios["M3"]),
                  solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$meio_id[1], as.integer(meios["M3"]))
  expect_equal(det$etapas$meio_codigo[1], "M3")
})

test_that("atualizar_etapa: meio_id novo arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_et <- adicionar_etapa(con, wf, meios["M1"], "E",
                           solicitante_id = ops$admin)
  arquivar_meio(con, meios["M3"], solicitante_id = ops$admin)
  
  expect_error(
    atualizar_etapa(con, id_et,
                    campos = list(meio_id = meios["M3"]),
                    solicitante_id = ops$admin),
    "arquivado"
  )
})

test_that("atualizar_etapa: workflow arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_et <- adicionar_etapa(con, wf, meios["M1"], "E",
                           solicitante_id = ops$admin)
  arquivar_workflow(con, wf, solicitante_id = ops$admin)
  
  expect_error(
    atualizar_etapa(con, id_et,
                    campos = list(nome_etapa = "Y"),
                    solicitante_id = ops$admin),
    "arquivado"
  )
})

test_that("atualizar_etapa: campos fora whitelist sao ignorados", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_et <- adicionar_etapa(con, wf, meios["M1"], "E",
                           solicitante_id = ops$admin)
  
  # Tenta mudar ordem (nao permitido)
  expect_error(
    atualizar_etapa(con, id_et,
                    campos = list(ordem = 5L),
                    solicitante_id = ops$admin),
    "Nenhum campo valido"
  )
})

# ---------------------------------------------------------------------
# Testes: remover_etapa (COM RENUMERACAO)
# ---------------------------------------------------------------------

test_that("remover_etapa: remove do meio e renumera posteriores", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  adicionar_etapa(con, wf, meios["M1"], "A", solicitante_id = ops$admin)
  id_b <- adicionar_etapa(con, wf, meios["M2"], "B",
                          solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M3"], "C", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M4"], "D", solicitante_id = ops$admin)
  
  remover_etapa(con, id_b, solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$nome_etapa, c("A", "C", "D"))
  expect_equal(det$etapas$ordem, c(1L, 2L, 3L))
})

test_that("remover_etapa: remove ultima nao afeta anteriores", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  adicionar_etapa(con, wf, meios["M1"], "A", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M2"], "B", solicitante_id = ops$admin)
  id_c <- adicionar_etapa(con, wf, meios["M3"], "C",
                          solicitante_id = ops$admin)
  
  remover_etapa(con, id_c, solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$nome_etapa, c("A", "B"))
  expect_equal(det$etapas$ordem, c(1L, 2L))
})

test_that("remover_etapa: remove primeira empurra todas para tras", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_a <- adicionar_etapa(con, wf, meios["M1"], "A",
                          solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M2"], "B", solicitante_id = ops$admin)
  adicionar_etapa(con, wf, meios["M3"], "C", solicitante_id = ops$admin)
  
  remover_etapa(con, id_a, solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$nome_etapa, c("B", "C"))
  expect_equal(det$etapas$ordem, c(1L, 2L))
})

test_that("remover_etapa: workflow arquivado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_et <- adicionar_etapa(con, wf, meios["M1"], "E",
                           solicitante_id = ops$admin)
  arquivar_workflow(con, wf, solicitante_id = ops$admin)
  
  expect_error(
    remover_etapa(con, id_et, solicitante_id = ops$admin),
    "arquivado"
  )
})

# ---------------------------------------------------------------------
# Testes: reordenar_etapas
# ---------------------------------------------------------------------

test_that("reordenar_etapas: aplica nova ordem", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_a <- adicionar_etapa(con, wf, meios["M1"], "A",
                          solicitante_id = ops$admin)
  id_b <- adicionar_etapa(con, wf, meios["M2"], "B",
                          solicitante_id = ops$admin)
  id_c <- adicionar_etapa(con, wf, meios["M3"], "C",
                          solicitante_id = ops$admin)
  
  # Inverte
  reordenar_etapas(con, wf, nova_ordem = c(id_c, id_b, id_a),
                   solicitante_id = ops$admin)
  
  det <- detalhe_workflow(con, wf)
  expect_equal(det$etapas$nome_etapa, c("C", "B", "A"))
  expect_equal(det$etapas$ordem, c(1L, 2L, 3L))
})

test_that("reordenar_etapas: id duplicado da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_a <- adicionar_etapa(con, wf, meios["M1"], "A",
                          solicitante_id = ops$admin)
  id_b <- adicionar_etapa(con, wf, meios["M2"], "B",
                          solicitante_id = ops$admin)
  
  expect_error(
    reordenar_etapas(con, wf, nova_ordem = c(id_a, id_a),
                     solicitante_id = ops$admin),
    "duplicados"
  )
})

test_that("reordenar_etapas: id faltando da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf <- criar_workflow(con, "WF", criado_por_id = ops$admin)
  id_a <- adicionar_etapa(con, wf, meios["M1"], "A",
                          solicitante_id = ops$admin)
  id_b <- adicionar_etapa(con, wf, meios["M2"], "B",
                          solicitante_id = ops$admin)
  id_c <- adicionar_etapa(con, wf, meios["M3"], "C",
                          solicitante_id = ops$admin)
  
  # Falta id_c
  expect_error(
    reordenar_etapas(con, wf, nova_ordem = c(id_a, id_b),
                     solicitante_id = ops$admin),
    "deve ter"
  )
})

test_that("reordenar_etapas: id de outro workflow da erro", {
  con <- .setup_banco_workflows()
  on.exit(DBI::dbDisconnect(con))
  ops <- .setup_ops_workflows(con)
  meios <- .setup_meios_workflows(con)
  
  wf1 <- criar_workflow(con, "WF1", criado_por_id = ops$admin)
  wf2 <- criar_workflow(con, "WF2", criado_por_id = ops$admin)
  
  id_a <- adicionar_etapa(con, wf1, meios["M1"], "A",
                          solicitante_id = ops$admin)
  id_b <- adicionar_etapa(con, wf1, meios["M2"], "B",
                          solicitante_id = ops$admin)
  # Etapa que pertence ao WF2
  id_x <- adicionar_etapa(con, wf2, meios["M3"], "X",
                          solicitante_id = ops$admin)
  
  # Tenta reordenar WF1 com id de etapa do WF2
  expect_error(
    reordenar_etapas(con, wf1, nova_ordem = c(id_a, id_x),
                     solicitante_id = ops$admin),
    "deve ter|nao contem"
  )
})