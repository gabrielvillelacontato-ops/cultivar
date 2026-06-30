# =====================================================================
# Testes de utils_preparo
# =====================================================================

.setup_banco_preparo <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  mig1 <- file.path(testthat::test_path(), "..", "..", "migrations",
                    "001_initial_schema.sql")
  sql1 <- paste(readLines(mig1, encoding = "UTF-8", warn = FALSE),
                collapse = "\n")
  sql1 <- db_strip_sql_comments(sql1)
  for (stmt in db_split_sql_statements(sql1)) {
    DBI::dbExecute(con, stmt)
  }
  
  DBI::dbExecute(con,
                 "ALTER TABLE meios ADD COLUMN bloqueado_preparo INTEGER NOT NULL DEFAULT 0
     CHECK (bloqueado_preparo IN (0,1));"
  )
  
  seed_categorias_componente(con)
  seed_categorias_meio(con)
  con
}

.criar_meio_com_composicao <- function(con, codigo = "TST",
                                       bloqueado = 0L, deleted = NA,
                                       n_componentes = 3L) {
  cat_meio_id <- DBI::dbGetQuery(con,
                                 "SELECT id FROM categorias_meio LIMIT 1;")$id[1]
  
  DBI::dbExecute(
    con,
    "INSERT INTO meios (tenant_id, categoria_id, codigo_curto, nome,
                        flag_incerteza, bloqueado_preparo, deleted_at,
                        criado_em, atualizado_em)
     VALUES (1, ?, ?, ?, 0, ?, ?, ?, ?);",
    params = list(cat_meio_id, codigo, paste("Meio", codigo),
                  bloqueado,
                  if (is.na(deleted)) NA_character_ else deleted,
                  .now_utc(), .now_utc())
  )
  meio_id <- DBI::dbGetQuery(con,
                             "SELECT id FROM meios WHERE codigo_curto = ?;",
                             params = list(codigo))$id[1]
  
  cat_comp_id <- DBI::dbGetQuery(con,
                                 "SELECT id FROM categorias_componente LIMIT 1;")$id[1]
  
  comp_ids <- integer(0)
  concs <- c(1650, 1900, 30000)[seq_len(n_componentes)]
  nomes <- c(paste0("Comp_", codigo, "_A"),
             paste0("Comp_", codigo, "_B"),
             paste0("Comp_", codigo, "_C"))[seq_len(n_componentes)]
  
  for (i in seq_len(n_componentes)) {
    DBI::dbExecute(con,
                   "INSERT INTO componentes (tenant_id, categoria_id, nome)
       VALUES (1, ?, ?);",
                   params = list(cat_comp_id, nomes[i]))
    cid <- DBI::dbGetQuery(con,
                           "SELECT id FROM componentes WHERE nome = ?;",
                           params = list(nomes[i]))$id[1]
    comp_ids <- c(comp_ids, cid)
    
    DBI::dbExecute(con,
                   "INSERT INTO meio_componentes (meio_id, componente_id,
                                       concentracao_mg_l, ordem_exibicao)
       VALUES (?, ?, ?, ?);",
                   params = list(meio_id, cid, concs[i], i))
  }
  
  list(meio_id = meio_id, componente_ids = comp_ids)
}

# ---------------------------------------------------------------------
# iniciar_preparo
# ---------------------------------------------------------------------

test_that("iniciar_preparo: cria rascunho com snapshot completo", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST", n_componentes = 3L)
  
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  expect_true(is.integer(pid))
  
  p <- DBI::dbGetQuery(con,
                       "SELECT status, volume_final_ml, lote_interno FROM preparos WHERE id = ?;",
                       params = list(pid))
  expect_equal(p$status[1], "rascunho")
  expect_equal(p$volume_final_ml[1], 1000)
  expect_match(p$lote_interno[1], "^TST_\\d{4}-\\d{2}-\\d{2}_0001$")
  
  snap <- DBI::dbGetQuery(con,
                          "SELECT componente_id, massa_teorica_mg, massa_pesada_mg
     FROM preparo_componentes_usados WHERE preparo_id = ?
     ORDER BY id;",
                          params = list(pid))
  expect_equal(nrow(snap), 3L)
  expect_equal(snap$massa_teorica_mg[1], 1650, tolerance = 1e-9)
  expect_equal(snap$massa_teorica_mg[2], 1900, tolerance = 1e-9)
  expect_equal(snap$massa_teorica_mg[3], 30000, tolerance = 1e-9)
  expect_true(all(is.na(snap$massa_pesada_mg)))
})

test_that("iniciar_preparo: meio bloqueado da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "PIA", bloqueado = 1L)
  expect_error(
    iniciar_preparo(con, m$meio_id, 1000, id_op),
    "bloqueado para preparo"
  )
})

test_that("iniciar_preparo: meio arquivado da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "ARQ", deleted = .now_utc())
  expect_error(
    iniciar_preparo(con, m$meio_id, 1000, id_op),
    "arquivado"
  )
})

test_that("iniciar_preparo: volume invalido da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  expect_error(iniciar_preparo(con, m$meio_id, 0, id_op), "volume_ml")
  expect_error(iniciar_preparo(con, m$meio_id, -100, id_op), "volume_ml")
  expect_error(iniciar_preparo(con, m$meio_id, NA, id_op), "volume_ml")
})

test_that("iniciar_preparo: meio sem componentes da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  
  cat_meio_id <- DBI::dbGetQuery(con,
                                 "SELECT id FROM categorias_meio LIMIT 1;")$id[1]
  DBI::dbExecute(con,
                 "INSERT INTO meios (tenant_id, categoria_id, codigo_curto, nome,
                          flag_incerteza, bloqueado_preparo,
                          criado_em, atualizado_em)
     VALUES (1, ?, 'VAZ', 'Meio Vazio', 0, 0, ?, ?);",
                 params = list(cat_meio_id, .now_utc(), .now_utc()))
  mid <- DBI::dbGetQuery(con,
                         "SELECT id FROM meios WHERE codigo_curto = 'VAZ';")$id[1]
  
  expect_error(
    iniciar_preparo(con, mid, 1000, id_op),
    "nao tem componentes"
  )
})

test_that("iniciar_preparo: registra audit_log INSERT", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 500, id_op)
  
  audit <- DBI::dbGetQuery(con,
                           "SELECT acao, valores_depois FROM audit_log
     WHERE entidade_tabela = 'preparos' AND entidade_id = ?;",
                           params = list(pid))
  expect_equal(audit$acao[1], "INSERT")
  expect_true(grepl("rascunho", audit$valores_depois[1]))
})

# ---------------------------------------------------------------------
# carregar_preparo
# ---------------------------------------------------------------------

test_that("carregar_preparo: retorna meio + componentes", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  res <- carregar_preparo(con, pid)
  expect_equal(res$preparo$status[1], "rascunho")
  expect_equal(res$preparo$meio_codigo[1], "TST")
  expect_equal(res$preparo$operador_nome[1], "Mauro")
  expect_equal(nrow(res$componentes), 3L)
})

test_that("carregar_preparo: id inexistente da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  expect_error(carregar_preparo(con, 99999), "nao encontrado")
})

# ---------------------------------------------------------------------
# listar_rascunhos_operador
# ---------------------------------------------------------------------

test_that("listar_rascunhos_operador: retorna so do operador", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_a <- criar_operador(con, "Alice", "1111", "admin")
  id_b <- criar_operador(con, "Bob", "2222", "operador",
                         criado_por_id = id_a)
  m1 <- .criar_meio_com_composicao(con, "MA")
  m2 <- .criar_meio_com_composicao(con, "MB")
  
  iniciar_preparo(con, m1$meio_id, 1000, id_a)
  iniciar_preparo(con, m2$meio_id, 1000, id_b)
  
  res_a <- listar_rascunhos_operador(con, id_a)
  res_b <- listar_rascunhos_operador(con, id_b)
  expect_equal(nrow(res_a), 1L)
  expect_equal(nrow(res_b), 1L)
  expect_equal(res_a$meio_codigo[1], "MA")
  expect_equal(res_b$meio_codigo[1], "MB")
})

test_that("listar_rascunhos_operador: conta pesagens", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST", n_componentes = 3L)
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  salvar_pesagem(con, pid, m$componente_ids[1], 1650, id_op)
  
  res <- listar_rascunhos_operador(con, id_op)
  expect_equal(res$n_pesados[1], 1L)
  expect_equal(res$n_total[1], 3L)
})

# ---------------------------------------------------------------------
# salvar_pesagem
# ---------------------------------------------------------------------

test_that("salvar_pesagem: dentro de 5% classifica como ok", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  res <- salvar_pesagem(con, pid, m$componente_ids[1], 1700, id_op)
  expect_equal(res$classificacao, "ok")
  expect_lt(res$desvio_percentual, 5)
})

test_that("salvar_pesagem: 5-10% classifica como atencao", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  res <- salvar_pesagem(con, pid, m$componente_ids[1], 1770, id_op)
  expect_equal(res$classificacao, "atencao")
})

test_that("salvar_pesagem: >10% classifica como fora_tolerancia", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  res <- salvar_pesagem(con, pid, m$componente_ids[1], 2000, id_op)
  expect_equal(res$classificacao, "fora_tolerancia")
})

test_that("salvar_pesagem: primeira pesagem promove rascunho -> em_preparo", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  status_antes <- DBI::dbGetQuery(con,
                                  "SELECT status FROM preparos WHERE id = ?;",
                                  params = list(pid))$status[1]
  expect_equal(status_antes, "rascunho")
  
  salvar_pesagem(con, pid, m$componente_ids[1], 1650, id_op)
  
  status_depois <- DBI::dbGetQuery(con,
                                   "SELECT status FROM preparos WHERE id = ?;",
                                   params = list(pid))$status[1]
  expect_equal(status_depois, "em_preparo")
})

test_that("salvar_pesagem: operador diferente do dono da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_a <- criar_operador(con, "Alice", "1111", "admin")
  id_b <- criar_operador(con, "Bob", "2222", "operador",
                         criado_por_id = id_a)
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_a)
  
  expect_error(
    salvar_pesagem(con, pid, m$componente_ids[1], 1650, id_b),
    "operador dono"
  )
})

test_that("salvar_pesagem: componente fora do snapshot da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  expect_error(
    salvar_pesagem(con, pid, 99999, 1650, id_op),
    "nao faz parte"
  )
})

test_that("salvar_pesagem: massa <= 0 da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  expect_error(
    salvar_pesagem(con, pid, m$componente_ids[1], 0, id_op),
    "massa_real_mg"
  )
  expect_error(
    salvar_pesagem(con, pid, m$componente_ids[1], -100, id_op),
    "massa_real_mg"
  )
})

# ---------------------------------------------------------------------
# salvar_ph_observacoes
# ---------------------------------------------------------------------

test_that("salvar_ph_observacoes: valida intervalo de pH", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  expect_error(
    salvar_ph_observacoes(con, pid, ph_medido = 15, operador_id = id_op),
    "entre 0 e 14"
  )
  expect_error(
    salvar_ph_observacoes(con, pid, ph_medido = -1, operador_id = id_op),
    "entre 0 e 14"
  )
})

test_that("salvar_ph_observacoes: aceita pH valido", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  salvar_ph_observacoes(con, pid, ph_medido = 5.8,
                        observacoes = "ajustado com KOH",
                        operador_id = id_op)
  p <- DBI::dbGetQuery(con,
                       "SELECT ph_medido, observacoes FROM preparos WHERE id = ?;",
                       params = list(pid))
  expect_equal(p$ph_medido[1], 5.8, tolerance = 1e-9)
  expect_equal(p$observacoes[1], "ajustado com KOH")
})

# ---------------------------------------------------------------------
# concluir_preparo
# ---------------------------------------------------------------------

test_that("concluir_preparo: sem todas pesagens da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST", n_componentes = 3L)
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  salvar_pesagem(con, pid, m$componente_ids[1], 1650, id_op)
  
  expect_error(
    concluir_preparo(con, pid, id_op),
    "2 componente"
  )
})

test_that("concluir_preparo: com todas pesagens funciona", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST", n_componentes = 3L)
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  salvar_pesagem(con, pid, m$componente_ids[1], 1650, id_op)
  salvar_pesagem(con, pid, m$componente_ids[2], 1900, id_op)
  salvar_pesagem(con, pid, m$componente_ids[3], 30000, id_op)
  
  res <- concluir_preparo(con, pid, id_op)
  expect_equal(res$n_componentes, 3L)
  expect_equal(res$n_fora_tolerancia, 0L)
  expect_match(res$lote_interno, "^TST_")
  
  status <- DBI::dbGetQuery(con,
                            "SELECT status FROM preparos WHERE id = ?;",
                            params = list(pid))$status[1]
  expect_equal(status, "concluido")
})

test_that("concluir_preparo: conta n_fora_tolerancia", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST", n_componentes = 3L)
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  salvar_pesagem(con, pid, m$componente_ids[1], 1650, id_op)
  salvar_pesagem(con, pid, m$componente_ids[2], 2200, id_op)
  salvar_pesagem(con, pid, m$componente_ids[3], 30000, id_op)
  
  res <- concluir_preparo(con, pid, id_op)
  expect_equal(res$n_fora_tolerancia, 1L)
})

# ---------------------------------------------------------------------
# descartar_preparo
# ---------------------------------------------------------------------

test_that("descartar_preparo: motivo obrigatorio", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  expect_error(descartar_preparo(con, pid, "", id_op), "Motivo")
  expect_error(descartar_preparo(con, pid, "   ", id_op), "Motivo")
  expect_error(descartar_preparo(con, pid, NA_character_, id_op), "Motivo")
})

test_that("descartar_preparo: operador descarta proprio rascunho", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_op <- criar_operador(con, "Op", "2222", "operador",
                          criado_por_id = id_admin)
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  descartar_preparo(con, pid, "errei o volume", id_op)
  status <- DBI::dbGetQuery(con,
                            "SELECT status FROM preparos WHERE id = ?;",
                            params = list(pid))$status[1]
  expect_equal(status, "descartado")
})

test_that("descartar_preparo: operador NAO descarta preparo de outro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_op_a <- criar_operador(con, "OpA", "2222", "operador",
                            criado_por_id = id_admin)
  id_op_b <- criar_operador(con, "OpB", "3333", "operador",
                            criado_por_id = id_admin)
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op_a)
  
  expect_error(
    descartar_preparo(con, pid, "alheio", id_op_b),
    "proprio preparo"
  )
})

test_that("descartar_preparo: supervisor descarta de qualquer um", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  id_op <- criar_operador(con, "Op", "3333", "operador",
                          criado_por_id = id_admin)
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  descartar_preparo(con, pid, "QC reprovou", id_sup)
  status <- DBI::dbGetQuery(con,
                            "SELECT status FROM preparos WHERE id = ?;",
                            params = list(pid))$status[1]
  expect_equal(status, "descartado")
})

test_that("descartar_preparo: ja descartado da erro", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST")
  pid <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  descartar_preparo(con, pid, "primeiro", id_op)
  expect_error(
    descartar_preparo(con, pid, "segundo", id_op),
    "ja esta descartado"
  )
})

# ---------------------------------------------------------------------
# listar_preparos
# ---------------------------------------------------------------------

test_that("listar_preparos: sem filtros retorna tudo", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_a <- criar_operador(con, "Alice", "1111", "admin")
  id_b <- criar_operador(con, "Bob", "2222", "operador",
                         criado_por_id = id_a)
  m1 <- .criar_meio_com_composicao(con, "MA")
  m2 <- .criar_meio_com_composicao(con, "MB")
  iniciar_preparo(con, m1$meio_id, 1000, id_a)
  iniciar_preparo(con, m2$meio_id, 500, id_b)
  
  res <- listar_preparos(con)
  expect_equal(nrow(res), 2L)
})

test_that("listar_preparos: filtro_status", {
  con <- .setup_banco_preparo()
  on.exit(DBI::dbDisconnect(con))
  id_op <- criar_operador(con, "Mauro", "1234", "admin")
  m <- .criar_meio_com_composicao(con, "TST", n_componentes = 1L)
  
  pid1 <- iniciar_preparo(con, m$meio_id, 1000, id_op)
  
  m2 <- .criar_meio_com_composicao(con, "TS2", n_componentes = 1L)
  pid2 <- iniciar_preparo(con, m2$meio_id, 500, id_op)
  salvar_pesagem(con, pid2, m2$componente_ids[1], 1650, id_op)
  concluir_preparo(con, pid2, id_op)
  
  res_concluidos <- listar_preparos(con, filtro_status = "concluido")
  expect_equal(nrow(res_concluidos), 1L)
  expect_equal(res_concluidos$id[1], pid2)
  
  res_rascunhos <- listar_preparos(con, filtro_status = "rascunho")
  expect_equal(nrow(res_rascunhos), 1L)
  expect_equal(res_rascunhos$id[1], pid1)
})