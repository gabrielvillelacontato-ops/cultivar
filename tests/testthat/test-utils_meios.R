# =====================================================================
# Testes de utils_meios
# =====================================================================
# Cobertura:
#   - listar_meios (com/sem arquivados, filtro categoria)
#   - buscar_meios (termo vazio = lista tudo; termo com match parcial)
#   - detalhe_meio (meio + composicao)
#   - listar_categorias_meio
#   - atualizar_meio (campos validos, validacoes, permissoes,
#     regra admin para desbloquear, audit log)
#   - arquivar_meio (soft delete + audit log)
#   - restaurar_meio (apenas admin)
# =====================================================================

# ---------------------------------------------------------------------
# Setup helpers
# ---------------------------------------------------------------------

.setup_banco_meios <- function() {
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
  
  # Seeds minimos
  seed_categorias_componente(con)
  seed_categorias_meio(con)
  con
}

#' Cria um meio minimo para testes
.criar_meio_teste <- function(con, codigo = "TST", nome = "Meio Teste",
                              categoria = "transformacao_genetica",
                              bloqueado = 0L, deleted = NA) {
  cat_id <- DBI::dbGetQuery(
    con, "SELECT id FROM categorias_meio WHERE nome = ? LIMIT 1;",
    params = list(categoria)
  )$id[1]
  
  DBI::dbExecute(
    con,
    "INSERT INTO meios (tenant_id, categoria_id, codigo_curto, nome,
                        flag_incerteza, bloqueado_preparo, deleted_at,
                        criado_em, atualizado_em)
     VALUES (1, ?, ?, ?, 0, ?, ?, ?, ?);",
    params = list(cat_id, codigo, nome, bloqueado,
                  if (is.na(deleted)) NA_character_ else deleted,
                  .now_utc(), .now_utc())
  )
  DBI::dbGetQuery(con, "SELECT id FROM meios WHERE codigo_curto = ?;",
                  params = list(codigo))$id[1]
}

#' Adiciona componente a um meio (para testar detalhe)
.adicionar_componente_meio <- function(con, meio_id, nome_comp, conc_mg_l,
                                       categoria = "outros") {
  cat_id <- DBI::dbGetQuery(
    con, "SELECT id FROM categorias_componente WHERE nome = ? LIMIT 1;",
    params = list(categoria)
  )$id[1]
  if (is.na(cat_id)) {
    DBI::dbExecute(con,
                   "INSERT INTO categorias_componente (tenant_id, nome) VALUES (1, ?);",
                   params = list(categoria))
    cat_id <- DBI::dbGetQuery(con,
                              "SELECT id FROM categorias_componente WHERE nome = ?;",
                              params = list(categoria))$id[1]
  }
  
  comp <- DBI::dbGetQuery(
    con, "SELECT id FROM componentes WHERE nome = ?;",
    params = list(nome_comp))
  if (nrow(comp) == 0L) {
    DBI::dbExecute(con,
                   "INSERT INTO componentes (tenant_id, categoria_id, nome)
       VALUES (1, ?, ?);",
                   params = list(cat_id, nome_comp))
    comp_id <- DBI::dbGetQuery(con,
                               "SELECT id FROM componentes WHERE nome = ?;",
                               params = list(nome_comp))$id[1]
  } else {
    comp_id <- comp$id[1]
  }
  
  DBI::dbExecute(
    con,
    "INSERT INTO meio_componentes (meio_id, componente_id, concentracao_mg_l)
     VALUES (?, ?, ?);",
    params = list(meio_id, comp_id, conc_mg_l)
  )
}

# ---------------------------------------------------------------------
# Listagem
# ---------------------------------------------------------------------

test_that("listar_meios: vazio retorna data.frame vazio", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  res <- listar_meios(con)
  expect_equal(nrow(res), 0L)
})

test_that("listar_meios: traz meios criados", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "AAA", "Meio A")
  .criar_meio_teste(con, "BBB", "Meio B")
  res <- listar_meios(con)
  expect_equal(nrow(res), 2L)
  expect_equal(sort(res$codigo_curto), c("AAA", "BBB"))
})

test_that("listar_meios: nao traz arquivados por padrao", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "AAA", "Meio Ativo")
  .criar_meio_teste(con, "BBB", "Meio Arquivado", deleted = .now_utc())
  res <- listar_meios(con)
  expect_equal(nrow(res), 1L)
  expect_equal(res$codigo_curto[1], "AAA")
})

test_that("listar_meios: incluir_arquivados=TRUE traz todos", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "AAA")
  .criar_meio_teste(con, "BBB", deleted = .now_utc())
  res <- listar_meios(con, incluir_arquivados = TRUE)
  expect_equal(nrow(res), 2L)
})

test_that("listar_meios: conta n_componentes", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id <- .criar_meio_teste(con, "AAA")
  .adicionar_componente_meio(con, id, "NH4NO3", 1650)
  .adicionar_componente_meio(con, id, "KNO3", 1900)
  res <- listar_meios(con)
  expect_equal(res$n_componentes[1], 2L)
})

test_that("listar_meios: filtro_categoria", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  cats <- listar_categorias_meio(con)
  cat1_id <- cats$id[1]
  cat2_id <- cats$id[2]
  .criar_meio_teste(con, "AAA", categoria = cats$nome[1])
  .criar_meio_teste(con, "BBB", categoria = cats$nome[2])
  res <- listar_meios(con, filtro_categoria = cat1_id)
  expect_equal(nrow(res), 1L)
  expect_equal(res$codigo_curto[1], "AAA")
})

# ---------------------------------------------------------------------
# Busca
# ---------------------------------------------------------------------

test_that("buscar_meios: termo vazio retorna tudo", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "AAA")
  .criar_meio_teste(con, "BBB")
  res <- buscar_meios(con, "")
  expect_equal(nrow(res), 2L)
})

test_that("buscar_meios: match parcial por codigo", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "MSR")
  .criar_meio_teste(con, "MS0")
  .criar_meio_teste(con, "B5")
  res <- buscar_meios(con, "MS")
  expect_equal(nrow(res), 2L)
})

test_that("buscar_meios: match por nome", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "AAA", nome = "Murashige Skoog Revised")
  .criar_meio_teste(con, "BBB", nome = "Gamborg B5")
  res <- buscar_meios(con, "Murashige")
  expect_equal(nrow(res), 1L)
  expect_equal(res$codigo_curto[1], "AAA")
})

test_that("buscar_meios: case-insensitive em SQLite default", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  .criar_meio_teste(con, "MSR", nome = "Murashige Skoog")
  # SQLite LIKE e case-insensitive para ASCII por padrao
  res <- buscar_meios(con, "murashige")
  expect_equal(nrow(res), 1L)
})

# ---------------------------------------------------------------------
# Detalhe
# ---------------------------------------------------------------------

test_that("detalhe_meio: retorna meio + composicao", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id <- .criar_meio_teste(con, "AAA", nome = "Teste A")
  .adicionar_componente_meio(con, id, "NH4NO3", 1650)
  .adicionar_componente_meio(con, id, "KNO3", 1900)
  res <- detalhe_meio(con, id)
  expect_equal(res$meio$codigo_curto[1], "AAA")
  expect_equal(nrow(res$composicao), 2L)
  expect_true("NH4NO3" %in% res$composicao$nome)
})

test_that("detalhe_meio: meio inexistente da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  expect_error(detalhe_meio(con, 99999), "nao encontrado")
})

test_that("detalhe_meio: meio sem composicao retorna composicao vazia", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id <- .criar_meio_teste(con, "VAZIO")
  res <- detalhe_meio(con, id)
  expect_equal(nrow(res$meio), 1L)
  expect_equal(nrow(res$composicao), 0L)
})

# ---------------------------------------------------------------------
# Categorias
# ---------------------------------------------------------------------

test_that("listar_categorias_meio: retorna seed", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  res <- listar_categorias_meio(con)
  expect_true(nrow(res) >= 3L)
})

# ---------------------------------------------------------------------
# Atualizacao - validacoes basicas
# ---------------------------------------------------------------------

test_that("atualizar_meio: sem permissao (operador) da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_op <- criar_operador(con, "Op", "2222", "operador",
                          criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA")
  
  expect_error(
    atualizar_meio(con, meio_id, list(nome = "Novo Nome"),
                   solicitante_id = id_op),
    "Permissao insuficiente"
  )
})

test_that("atualizar_meio: supervisor pode atualizar nome", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA", nome = "Original")
  
  atualizar_meio(con, meio_id, list(nome = "Novo Nome"),
                 solicitante_id = id_sup)
  
  m <- DBI::dbGetQuery(con, "SELECT nome FROM meios WHERE id = ?;",
                       params = list(meio_id))
  expect_equal(m$nome[1], "Novo Nome")
})

test_that("atualizar_meio: codigo_curto duplicado da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio1 <- .criar_meio_teste(con, "AAA")
  meio2 <- .criar_meio_teste(con, "BBB")
  
  expect_error(
    atualizar_meio(con, meio2, list(codigo_curto = "AAA"),
                   solicitante_id = id_admin),
    "Ja existe outro meio"
  )
})

test_that("atualizar_meio: campo invalido da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA")
  expect_error(
    atualizar_meio(con, meio_id, list(campo_inexistente = "x"),
                   solicitante_id = id_admin),
    "nao editaveis"
  )
})

test_that("atualizar_meio: ph fora de 0-14 da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA")
  expect_error(
    atualizar_meio(con, meio_id, list(ph_alvo = 15),
                   solicitante_id = id_admin),
    "ph_alvo"
  )
  expect_error(
    atualizar_meio(con, meio_id, list(ph_alvo = -1),
                   solicitante_id = id_admin),
    "ph_alvo"
  )
})

test_that("atualizar_meio: meio arquivado nao pode ser editado", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA", deleted = .now_utc())
  expect_error(
    atualizar_meio(con, meio_id, list(nome = "X"),
                   solicitante_id = id_admin),
    "arquivado"
  )
})

# ---------------------------------------------------------------------
# Regra: bloqueado_preparo
# ---------------------------------------------------------------------

test_that("atualizar_meio: supervisor pode BLOQUEAR", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA", bloqueado = 0L)
  
  atualizar_meio(con, meio_id, list(bloqueado_preparo = 1L),
                 solicitante_id = id_sup)
  bp <- DBI::dbGetQuery(con,
                        "SELECT bloqueado_preparo FROM meios WHERE id = ?;",
                        params = list(meio_id))$bloqueado_preparo[1]
  expect_equal(as.integer(bp), 1L)
})

test_that("atualizar_meio: supervisor NAO pode DESBLOQUEAR", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA", bloqueado = 1L)
  
  expect_error(
    atualizar_meio(con, meio_id, list(bloqueado_preparo = 0L),
                   solicitante_id = id_sup),
    "Apenas admin"
  )
})

test_that("atualizar_meio: admin pode DESBLOQUEAR", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA", bloqueado = 1L)
  
  atualizar_meio(con, meio_id, list(bloqueado_preparo = 0L),
                 solicitante_id = id_admin)
  bp <- DBI::dbGetQuery(con,
                        "SELECT bloqueado_preparo FROM meios WHERE id = ?;",
                        params = list(meio_id))$bloqueado_preparo[1]
  expect_equal(as.integer(bp), 0L)
})

# ---------------------------------------------------------------------
# Audit log
# ---------------------------------------------------------------------

test_that("atualizar_meio: registra audit_log UPDATE", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA", nome = "Original")
  
  atualizar_meio(con, meio_id, list(nome = "Modificado"),
                 solicitante_id = id_admin)
  
  audit <- DBI::dbGetQuery(con,
                           "SELECT acao, valores_antes, valores_depois FROM audit_log
     WHERE entidade_tabela = 'meios' AND entidade_id = ?;",
                           params = list(meio_id))
  expect_equal(nrow(audit), 1L)
  expect_equal(audit$acao[1], "UPDATE")
  expect_true(grepl("Original", audit$valores_antes[1]))
  expect_true(grepl("Modificado", audit$valores_depois[1]))
})

# ---------------------------------------------------------------------
# Arquivamento
# ---------------------------------------------------------------------

test_that("arquivar_meio: supervisor pode arquivar", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA")
  
  arquivar_meio(con, meio_id, solicitante_id = id_sup)
  
  m <- DBI::dbGetQuery(con,
                       "SELECT deleted_at FROM meios WHERE id = ?;",
                       params = list(meio_id))
  expect_false(is.na(m$deleted_at[1]))
})

test_that("arquivar_meio: operador NAO pode arquivar", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_op <- criar_operador(con, "Op", "2222", "operador",
                          criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA")
  
  expect_error(
    arquivar_meio(con, meio_id, solicitante_id = id_op),
    "Permissao insuficiente"
  )
})

test_that("arquivar_meio: ja arquivado da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA", deleted = .now_utc())
  expect_error(
    arquivar_meio(con, meio_id, solicitante_id = id_admin),
    "ja esta arquivado"
  )
})

test_that("arquivar_meio: registra audit_log DELETE", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA")
  arquivar_meio(con, meio_id, solicitante_id = id_admin)
  audit <- DBI::dbGetQuery(con,
                           "SELECT acao FROM audit_log
     WHERE entidade_tabela = 'meios' AND entidade_id = ?;",
                           params = list(meio_id))
  expect_true("DELETE" %in% audit$acao)
})

# ---------------------------------------------------------------------
# Restauracao
# ---------------------------------------------------------------------

test_that("restaurar_meio: admin pode restaurar", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA", deleted = .now_utc())
  
  restaurar_meio(con, meio_id, solicitante_id = id_admin)
  m <- DBI::dbGetQuery(con,
                       "SELECT deleted_at FROM meios WHERE id = ?;",
                       params = list(meio_id))
  expect_true(is.na(m$deleted_at[1]))
})

test_that("restaurar_meio: supervisor NAO pode restaurar", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  meio_id <- .criar_meio_teste(con, "AAA", deleted = .now_utc())
  
  expect_error(
    restaurar_meio(con, meio_id, solicitante_id = id_sup),
    "Permissao insuficiente"
  )
})

test_that("restaurar_meio: nao-arquivado da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  meio_id <- .criar_meio_teste(con, "AAA")
  expect_error(
    restaurar_meio(con, meio_id, solicitante_id = id_admin),
    "nao esta arquivado"
  )
})
# =====================================================================
# Testes de criar_meio
# =====================================================================

.obter_cat_id <- function(con, nome = "transformacao_genetica") {
  DBI::dbGetQuery(con,
                  "SELECT id FROM categorias_meio WHERE nome = ? LIMIT 1;",
                  params = list(nome))$id[1]
}

# ---------- Caminho feliz ----------

test_that("criar_meio: admin cria meio minimo retorna id", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  novo_id <- criar_meio(con,
                        nome = "Meio Novo",
                        codigo_curto = "MN1",
                        categoria_id = cat_id,
                        criado_por_id = id_admin)
  expect_true(is.integer(novo_id))
  
  m <- DBI::dbGetQuery(con,
                       "SELECT nome, codigo_curto, ph_alvo, flag_incerteza,
            bloqueado_preparo, nota_incerteza, criado_por
     FROM meios WHERE id = ?;",
                       params = list(novo_id))
  expect_equal(m$nome[1], "Meio Novo")
  expect_equal(m$codigo_curto[1], "MN1")
  expect_true(is.na(m$ph_alvo[1]))
  expect_equal(m$flag_incerteza[1], 0L)
  expect_equal(m$bloqueado_preparo[1], 0L)
  expect_true(is.na(m$nota_incerteza[1]))
  expect_equal(m$criado_por[1], id_admin)
})

test_that("criar_meio: supervisor cria meio completo", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_sup <- criar_operador(con, "Sup", "2222", "supervisor",
                           criado_por_id = id_admin)
  cat_id <- .obter_cat_id(con)
  
  novo_id <- criar_meio(con,
                        nome = "  Meio Completo  ",
                        codigo_curto = "MC1",
                        categoria_id = cat_id,
                        criado_por_id = id_sup,
                        referencia = "Murashige & Skoog 1962",
                        doi = "10.1111/j.1399-3054.1962.tb08052.x",
                        ph_alvo = 5.8,
                        observacoes = "uso geral",
                        flag_incerteza = 1L,
                        nota_incerteza = "fonte parcialmente verificada")
  expect_true(is.integer(novo_id))
  
  m <- DBI::dbGetQuery(con,
                       "SELECT nome, codigo_curto, referencia, doi, ph_alvo,
            observacoes, flag_incerteza, nota_incerteza
     FROM meios WHERE id = ?;",
                       params = list(novo_id))
  expect_equal(m$nome[1], "Meio Completo")  # trimado
  expect_equal(m$codigo_curto[1], "MC1")
  expect_equal(m$referencia[1], "Murashige & Skoog 1962")
  expect_equal(m$ph_alvo[1], 5.8, tolerance = 1e-9)
  expect_equal(m$flag_incerteza[1], 1L)
  expect_equal(m$nota_incerteza[1], "fonte parcialmente verificada")
})

test_that("criar_meio: registra audit_log INSERT", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  novo_id <- criar_meio(con,
                        nome = "Meio Aud",
                        codigo_curto = "MA1",
                        categoria_id = cat_id,
                        criado_por_id = id_admin)
  
  audit <- DBI::dbGetQuery(con,
                           "SELECT acao, valores_depois FROM audit_log
     WHERE entidade_tabela = 'meios' AND entidade_id = ?;",
                           params = list(novo_id))
  expect_equal(audit$acao[1], "INSERT")
  expect_true(grepl("MA1", audit$valores_depois[1]))
  expect_true(grepl("Meio Aud", audit$valores_depois[1]))
})

# ---------- Permissao ----------

test_that("criar_meio: operador comum nao pode criar", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  id_op <- criar_operador(con, "Op", "2222", "operador",
                          criado_por_id = id_admin)
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con,
               nome = "Meio X",
               codigo_curto = "MX1",
               categoria_id = cat_id,
               criado_por_id = id_op),
    "Apenas supervisores e admins"
  )
})

test_that("criar_meio: solicitante inexistente da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con,
               nome = "Meio X",
               codigo_curto = "MX1",
               categoria_id = cat_id,
               criado_por_id = 99999L),
    "Solicitante nao encontrado"
  )
})

# ---------- Validacao de nome e codigo ----------

test_that("criar_meio: nome vazio da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "   ", codigo_curto = "MX1",
               categoria_id = cat_id, criado_por_id = id_admin),
    "nao pode ser vazio"
  )
  expect_error(
    criar_meio(con, nome = NA_character_, codigo_curto = "MX1",
               categoria_id = cat_id, criado_por_id = id_admin),
    "obrigatorio"
  )
})

test_that("criar_meio: codigo_curto vazio da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "",
               categoria_id = cat_id, criado_por_id = id_admin),
    "obrigatorio|nao pode ser vazio"
  )
})

test_that("criar_meio: codigo_curto com caractere invalido da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX 1",
               categoria_id = cat_id, criado_por_id = id_admin),
    "letras, numeros e underscore"
  )
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX-1",
               categoria_id = cat_id, criado_por_id = id_admin),
    "letras, numeros e underscore"
  )
})

test_that("criar_meio: codigo_curto duplicado em meio ativo da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  criar_meio(con, nome = "Primeiro", codigo_curto = "DUP",
             categoria_id = cat_id, criado_por_id = id_admin)
  expect_error(
    criar_meio(con, nome = "Segundo", codigo_curto = "DUP",
               categoria_id = cat_id, criado_por_id = id_admin),
    "meio ativo"
  )
})

test_that("criar_meio: codigo_curto duplicado em meio arquivado da erro com msg diferente", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  id1 <- criar_meio(con, nome = "Primeiro", codigo_curto = "ARQ",
                    categoria_id = cat_id, criado_por_id = id_admin)
  # Arquiva direto via SQL (nao usar arquivar_meio que pode ter
  # logica extra que nao queremos no teste)
  DBI::dbExecute(con,
                 "UPDATE meios SET deleted_at = ? WHERE id = ?;",
                 params = list(.now_utc(), id1))
  
  expect_error(
    criar_meio(con, nome = "Segundo", codigo_curto = "ARQ",
               categoria_id = cat_id, criado_por_id = id_admin),
    "arquivado"
  )
})

# ---------- Validacao de FKs ----------

test_that("criar_meio: categoria inexistente da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX1",
               categoria_id = 99999L, criado_por_id = id_admin),
    "Categoria nao encontrada"
  )
})

test_that("criar_meio: pop_id inexistente da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX1",
               categoria_id = cat_id, criado_por_id = id_admin,
               pop_id = 99999L),
    "POP nao encontrado"
  )
})

# ---------- Validacao de campos opcionais ----------

test_that("criar_meio: ph_alvo fora do range da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX1",
               categoria_id = cat_id, criado_por_id = id_admin,
               ph_alvo = 15),
    "entre 0 e 14"
  )
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX2",
               categoria_id = cat_id, criado_por_id = id_admin,
               ph_alvo = -1),
    "entre 0 e 14"
  )
})

test_that("criar_meio: flag_incerteza=1 sem nota da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX1",
               categoria_id = cat_id, criado_por_id = id_admin,
               flag_incerteza = 1L),
    "Nota de incerteza"
  )
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX2",
               categoria_id = cat_id, criado_por_id = id_admin,
               flag_incerteza = 1L, nota_incerteza = "   "),
    "Nota de incerteza"
  )
})

test_that("criar_meio: bloqueado_preparo=1 sem nota da erro", {
  con <- .setup_banco_meios()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin", "1111", "admin")
  cat_id <- .obter_cat_id(con)
  
  expect_error(
    criar_meio(con, nome = "X", codigo_curto = "MX1",
               categoria_id = cat_id, criado_por_id = id_admin,
               bloqueado_preparo = 1L),
    "Nota de incerteza"
  )
})