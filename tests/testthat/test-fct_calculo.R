# =====================================================================
# Golden tests do nucleo cientifico do CultivaR
# =====================================================================
# 5 grupos: A) massa, B) conversao, C) formatacao, D) lote interno,
# E) orquestracao. Mais grupo F com casos de borda criticos.
#
# Estes testes sao a salvaguarda contra erros que descartariam lotes
# reais. Qualquer mudanca em fct_calculo.R deve manter todos verdes.
# =====================================================================

# Tolerancia para comparacoes de ponto flutuante
TOL <- 1e-9

# ---------------------------------------------------------------------
# Helpers de setup do banco de teste
# ---------------------------------------------------------------------

#' Cria banco SQLite em memoria com schema + tenant default + meios minimos
#' usados nos testes de lote/orquestracao.
.setup_banco_teste <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  # Aplica migration 001 (schema base)
  mig1 <- file.path(testthat::test_path(), "..", "..", "migrations",
                    "001_initial_schema.sql")
  sql1 <- paste(readLines(mig1, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  sql1 <- db_strip_sql_comments(sql1)
  for (stmt in db_split_sql_statements(sql1)) {
    DBI::dbExecute(con, stmt)
  }
  
  # Aplica apenas o efeito ESTRUTURAL da migration 002 (coluna bloqueado_preparo).
  # Pulamos as correcoes de dados (Fe-Na-EDTA, CuSO4, MES) porque o banco
  # de teste nao tem meios populados e os INSERTs com subqueries escalares
  # falhariam por NOT NULL.
  DBI::dbExecute(con,
                 "ALTER TABLE meios ADD COLUMN bloqueado_preparo INTEGER NOT NULL DEFAULT 0
     CHECK (bloqueado_preparo IN (0,1));"
  )
  
  # Seed minimo de categorias (helpers de teste dependem delas)
  seed_categorias_componente(con)
  seed_categorias_meio(con)
  
  con
}

#' Insere um meio minimo para teste de orquestracao
.inserir_meio_teste <- function(con, codigo, bloqueado = 0L, deleted = NA_character_) {
  cat_id <- DBI::dbGetQuery(
    con,
    "SELECT id FROM categorias_meio WHERE nome = 'transformacao_genetica' LIMIT 1;"
  )$id
  
  if (length(cat_id) == 0L) {
    DBI::dbExecute(con,
                   "INSERT INTO categorias_meio (tenant_id, nome) VALUES (1, 'transformacao_genetica');"
    )
    cat_id <- DBI::dbGetQuery(con,
                              "SELECT id FROM categorias_meio WHERE nome = 'transformacao_genetica';")$id
  }
  
  DBI::dbExecute(
    con,
    "INSERT INTO meios (tenant_id, categoria_id, nome, codigo_curto,
                        bloqueado_preparo, deleted_at, flag_incerteza)
     VALUES (1, ?, ?, ?, ?, ?, 0);",
    params = list(cat_id, paste("Teste", codigo), codigo, bloqueado, deleted)
  )
  DBI::dbGetQuery(con, "SELECT id FROM meios WHERE codigo_curto = ?;",
                  params = list(codigo))$id
}

#' Insere componente e seu vinculo com meio (com concentracao definida)
.inserir_componente_em_meio <- function(con, meio_id, nome_comp, conc_mg_l,
                                        categoria = "outros") {
  cat_id <- DBI::dbGetQuery(
    con, "SELECT id FROM categorias_componente WHERE nome = ? LIMIT 1;",
    params = list(categoria)
  )$id
  if (length(cat_id) == 0L) {
    DBI::dbExecute(con,
                   "INSERT INTO categorias_componente (tenant_id, nome) VALUES (1, ?);",
                   params = list(categoria))
    cat_id <- DBI::dbGetQuery(con,
                              "SELECT id FROM categorias_componente WHERE nome = ?;",
                              params = list(categoria))$id
  }
  
  comp_id <- DBI::dbGetQuery(
    con, "SELECT id FROM componentes WHERE nome = ?;",
    params = list(nome_comp)
  )$id
  if (length(comp_id) == 0L) {
    DBI::dbExecute(
      con,
      "INSERT INTO componentes (tenant_id, categoria_id, nome) VALUES (1, ?, ?);",
      params = list(cat_id, nome_comp)
    )
    comp_id <- DBI::dbGetQuery(
      con, "SELECT id FROM componentes WHERE nome = ?;",
      params = list(nome_comp)
    )$id
  }
  
  DBI::dbExecute(
    con,
    "INSERT INTO meio_componentes (meio_id, componente_id, concentracao_mg_l)
     VALUES (?, ?, ?);",
    params = list(meio_id, comp_id, conc_mg_l)
  )
}

# =====================================================================
# GRUPO A — calcular_massa
# =====================================================================

test_that("A1: NH4NO3 em 1L de MSR = 1650 mg", {
  expect_equal(calcular_massa(1650, 1000), 1650, tolerance = TOL)
})

test_that("A2: NH4NO3 em 500mL = 825 mg", {
  expect_equal(calcular_massa(1650, 500), 825, tolerance = TOL)
})

test_that("A3: sacarose 30 g/L em 1L = 30000 mg", {
  expect_equal(calcular_massa(30000, 1000), 30000, tolerance = TOL)
})

test_that("A4: CuSO4.5H2O 0.025 mg/L em 1L = 0.025 mg", {
  expect_equal(calcular_massa(0.025, 1000), 0.025, tolerance = TOL)
})

test_that("A5: volume zero da erro", {
  expect_error(calcular_massa(1650, 0), "volume_ml.*> 0")
})

test_that("A6: volume negativo da erro", {
  expect_error(calcular_massa(1650, -100), "volume_ml.*> 0")
})

test_that("A7: concentracao zero da erro", {
  expect_error(calcular_massa(0, 1000), "concentracao_mg_l.*> 0")
})

# =====================================================================
# GRUPO B — converter_para_mg_l
# =====================================================================

test_that("B1: 100 uM acetosiringona (MW 196.20) = 19.620 mg/L", {
  expect_equal(converter_para_mg_l(100, "uM", 196.20), 19.620, tolerance = TOL)
})

test_that("B2: 5 uM BAP (MW 225.25) = 1.12625 mg/L", {
  expect_equal(converter_para_mg_l(5, "uM", 225.25), 1.12625, tolerance = TOL)
})

test_that("B3: 7.5 mM MES (MW 195.24) = 1464.3 mg/L", {
  expect_equal(converter_para_mg_l(7.5, "mM", 195.24), 1464.3, tolerance = TOL)
})

test_that("B4: 30 g/L sacarose = 30000 mg/L", {
  expect_equal(converter_para_mg_l(30, "g/L"), 30000, tolerance = TOL)
})

test_that("B5: 1 percent (m/v) = 10000 mg/L", {
  expect_equal(converter_para_mg_l(1, "%"), 10000, tolerance = TOL)
})

test_that("B6: 0.2 percent Phytagel = 2000 mg/L", {
  expect_equal(converter_para_mg_l(0.2, "%"), 2000, tolerance = TOL)
})

test_that("B7: 1 mg/L identidade = 1 mg/L", {
  expect_equal(converter_para_mg_l(1, "mg/L"), 1, tolerance = TOL)
})

test_that("B8: 1 mg/mL = 1000 mg/L", {
  expect_equal(converter_para_mg_l(1, "mg/mL"), 1000, tolerance = TOL)
})

test_that("B9: 100 ug/mL = 100 mg/L", {
  expect_equal(converter_para_mg_l(100, "ug/mL"), 100, tolerance = TOL)
})

test_that("B10: 50 ug/L = 0.05 mg/L", {
  expect_equal(converter_para_mg_l(50, "ug/L"), 0.05, tolerance = TOL)
})

test_that("B11: 1 nM (MW 200) = 0.0002 mg/L", {
  expect_equal(converter_para_mg_l(1, "nM", 200), 0.0002, tolerance = TOL)
})

test_that("B12: uM sem MW da erro", {
  expect_error(converter_para_mg_l(100, "uM"), "massa_molar")
  expect_error(converter_para_mg_l(100, "uM", NA_real_), "massa_molar")
})

test_that("B13: mg/L com MW=NA funciona (nao usa MW)", {
  expect_equal(converter_para_mg_l(10, "mg/L", NA_real_), 10, tolerance = TOL)
})

test_that("B aliases: µM (unicode) = uM (ascii)", {
  expect_equal(
    converter_para_mg_l(100, "\u00b5M", 196.20),
    converter_para_mg_l(100, "uM", 196.20),
    tolerance = TOL
  )
})

test_that("B unidade invalida da erro descritivo", {
  expect_error(converter_para_mg_l(10, "kg/L"), "Unidade nao suportada")
})

# =====================================================================
# GRUPO C — formatar_massa
# =====================================================================

test_that("C1: 30000 mg = '30,0 g'", {
  expect_equal(formatar_massa(30000), "30 g")
})

test_that("C2: 1650 mg = '1,65 g'", {
  expect_equal(formatar_massa(1650), "1,65 g")
})

test_that("C3: 150 mg = '150 mg'", {
  expect_equal(formatar_massa(150), "150 mg")
})

test_that("C4: 0.025 mg = '25 ug'", {
  expect_equal(formatar_massa(0.025), "25 ug")
})

test_that("C5: 0.001 mg = '1 ug'", {
  expect_equal(formatar_massa(0.001), "1 ug")
})

test_that("C6: 0.0001 mg = '0,1 ug'", {
  expect_equal(formatar_massa(0.0001), "0,1 ug")
})

test_that("C7: 12345 mg = '12,3 g'", {
  expect_equal(formatar_massa(12345), "12,3 g")
})

test_that("C BAP 1.12625 mg formatado preserva precisao (>= 3 sig figs)", {
  res <- formatar_massa(1.12625)
  # Nao pode ser "1 mg" (perda de 12%)
  expect_false(res == "1 mg")
  # Deve ser "1,13 mg" (signif 3) ou similar
  expect_match(res, "^1,1\\d? mg$")
})

# =====================================================================
# GRUPO D — gerar_lote_interno
# =====================================================================

test_that("D1: primeiro lote do dia em banco vazio", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  lote <- gerar_lote_interno(con, "MSR", "2026-06-30")
  expect_equal(lote, "MSR_2026-06-30_0001")
})

test_that("D2: segundo lote do dia", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR")
  # Insere operador minimo
  DBI::dbExecute(con,
                 "INSERT INTO operadores (tenant_id, nome, pin_hash, pin_salt)
     VALUES (1, 'Teste', X'00', X'00');")
  op_id <- DBI::dbGetQuery(con, "SELECT id FROM operadores LIMIT 1;")$id
  DBI::dbExecute(con,
                 "INSERT INTO preparos (tenant_id, meio_id, operador_id, lote_interno, volume_final_ml)
     VALUES (1, ?, ?, 'MSR_2026-06-30_0001', 1000);",
                 params = list(meio_id, op_id))
  
  lote <- gerar_lote_interno(con, "MSR", "2026-06-30")
  expect_equal(lote, "MSR_2026-06-30_0002")
})

test_that("D3: sexto lote do dia com lacuna (max+1, nao count+1)", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR")
  DBI::dbExecute(con,
                 "INSERT INTO operadores (tenant_id, nome, pin_hash, pin_salt)
     VALUES (1, 'Teste', X'00', X'00');")
  op_id <- DBI::dbGetQuery(con, "SELECT id FROM operadores LIMIT 1;")$id
  
  # Insere lotes 0001, 0002, 0004, 0005 (com lacuna no 0003)
  for (n in c("0001", "0002", "0004", "0005")) {
    DBI::dbExecute(con,
                   "INSERT INTO preparos (tenant_id, meio_id, operador_id, lote_interno, volume_final_ml)
       VALUES (1, ?, ?, ?, 1000);",
                   params = list(meio_id, op_id, paste0("MSR_2026-06-30_", n)))
  }
  
  # Deve gerar 0006 (max=0005 + 1), nao 0005 (count=4 + 1)
  lote <- gerar_lote_interno(con, "MSR", "2026-06-30")
  expect_equal(lote, "MSR_2026-06-30_0006")
})

test_that("D4: meio igual, dia diferente, sequencial reinicia", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR")
  DBI::dbExecute(con,
                 "INSERT INTO operadores (tenant_id, nome, pin_hash, pin_salt)
     VALUES (1, 'Teste', X'00', X'00');")
  op_id <- DBI::dbGetQuery(con, "SELECT id FROM operadores LIMIT 1;")$id
  DBI::dbExecute(con,
                 "INSERT INTO preparos (tenant_id, meio_id, operador_id, lote_interno, volume_final_ml)
     VALUES (1, ?, ?, 'MSR_2026-06-30_0001', 1000);",
                 params = list(meio_id, op_id))
  
  lote <- gerar_lote_interno(con, "MSR", "2026-07-01")
  expect_equal(lote, "MSR_2026-07-01_0001")
})

test_that("D5: meio diferente, mesmo dia, sequencial independente", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR")
  DBI::dbExecute(con,
                 "INSERT INTO operadores (tenant_id, nome, pin_hash, pin_salt)
     VALUES (1, 'Teste', X'00', X'00');")
  op_id <- DBI::dbGetQuery(con, "SELECT id FROM operadores LIMIT 1;")$id
  DBI::dbExecute(con,
                 "INSERT INTO preparos (tenant_id, meio_id, operador_id, lote_interno, volume_final_ml)
     VALUES (1, ?, ?, 'MSR_2026-06-30_0001', 1000);",
                 params = list(meio_id, op_id))
  
  lote <- gerar_lote_interno(con, "B5", "2026-06-30")
  expect_equal(lote, "B5_2026-06-30_0001")
})

test_that("D6: sequencial 100 = '0100' (padding 4 digitos)", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR")
  DBI::dbExecute(con,
                 "INSERT INTO operadores (tenant_id, nome, pin_hash, pin_salt)
     VALUES (1, 'Teste', X'00', X'00');")
  op_id <- DBI::dbGetQuery(con, "SELECT id FROM operadores LIMIT 1;")$id
  DBI::dbExecute(con,
                 "INSERT INTO preparos (tenant_id, meio_id, operador_id, lote_interno, volume_final_ml)
     VALUES (1, ?, ?, 'MSR_2026-06-30_0099', 1000);",
                 params = list(meio_id, op_id))
  
  lote <- gerar_lote_interno(con, "MSR", "2026-06-30")
  expect_equal(lote, "MSR_2026-06-30_0100")
})

test_that("D7: sequencial 10000 expande sem truncamento", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR")
  DBI::dbExecute(con,
                 "INSERT INTO operadores (tenant_id, nome, pin_hash, pin_salt)
     VALUES (1, 'Teste', X'00', X'00');")
  op_id <- DBI::dbGetQuery(con, "SELECT id FROM operadores LIMIT 1;")$id
  DBI::dbExecute(con,
                 "INSERT INTO preparos (tenant_id, meio_id, operador_id, lote_interno, volume_final_ml)
     VALUES (1, ?, ?, 'MSR_2026-06-30_9999', 1000);",
                 params = list(meio_id, op_id))
  
  lote <- gerar_lote_interno(con, "MSR", "2026-06-30")
  expect_equal(lote, "MSR_2026-06-30_10000")
})

test_that("D data invalida da erro", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  expect_error(gerar_lote_interno(con, "MSR", "30/06/2026"), "YYYY-MM-DD")
  expect_error(gerar_lote_interno(con, "MSR", "2026-6-30"), "YYYY-MM-DD")
})

# =====================================================================
# GRUPO E — preparar_lote
# =====================================================================

test_that("E1: meio MSR retorna composicao com massas calculadas", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MSR_TEST")
  .inserir_componente_em_meio(con, meio_id, "NH4NO3", 1650, "macronutriente")
  .inserir_componente_em_meio(con, meio_id, "KNO3", 1900, "macronutriente")
  .inserir_componente_em_meio(con, meio_id, "Sacarose", 30000, "carbono")
  
  res <- preparar_lote(con, meio_id, 1000)
  expect_equal(nrow(res), 3)
  expect_equal(res$massa_mg[res$nome == "NH4NO3"], 1650, tolerance = TOL)
  expect_equal(res$massa_mg[res$nome == "Sacarose"], 30000, tolerance = TOL)
})

test_that("E2: volume 250 mL escala corretamente", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "MS0_TEST")
  .inserir_componente_em_meio(con, meio_id, "NH4NO3", 1650, "macronutriente")
  
  res <- preparar_lote(con, meio_id, 250)
  expect_equal(res$massa_mg[res$nome == "NH4NO3"], 412.5, tolerance = TOL)
})

test_that("E3: meio bloqueado_preparo da erro com motivo", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "PIA_TEST", bloqueado = 1L)
  DBI::dbExecute(con, "UPDATE meios SET nota_incerteza = 'AB salts pendentes' WHERE id = ?;",
                 params = list(meio_id))
  .inserir_componente_em_meio(con, meio_id, "Glicose", 10000, "carbono")
  
  expect_error(preparar_lote(con, meio_id, 100),
               "bloqueado para preparo.*AB salts pendentes")
})

test_that("E4: meio_id inexistente da erro", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  expect_error(preparar_lote(con, 99999, 1000), "Meio nao encontrado")
})

test_that("E5: volume zero da erro", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "TST")
  .inserir_componente_em_meio(con, meio_id, "NH4NO3", 1650, "macronutriente")
  expect_error(preparar_lote(con, meio_id, 0), "volume_ml.*> 0")
})

test_that("E meio arquivado (deleted_at) da erro", {
  con <- .setup_banco_teste()
  on.exit(DBI::dbDisconnect(con))
  meio_id <- .inserir_meio_teste(con, "ARQ", deleted = "2026-01-01T00:00:00Z")
  .inserir_componente_em_meio(con, meio_id, "NH4NO3", 1650, "macronutriente")
  expect_error(preparar_lote(con, meio_id, 1000), "arquivado")
})

# =====================================================================
# GRUPO F — Casos de borda
# =====================================================================

test_that("F1: calcular_massa com conc=NA da erro", {
  expect_error(calcular_massa(NA_real_, 1000), "concentracao_mg_l.*NA")
})

test_that("F2: calcular_massa com vol=NA da erro", {
  expect_error(calcular_massa(1650, NA_real_), "volume_ml.*NA")
})

test_that("F3: calcular_massa com vol=NaN da erro", {
  expect_error(calcular_massa(1650, NaN), "volume_ml")
})

test_that("F4: calcular_massa com vol=Inf da erro", {
  expect_error(calcular_massa(1650, Inf), "volume_ml.*finito")
})

test_that("F5: converter_para_mg_l com MW<=0 da erro", {
  expect_error(converter_para_mg_l(100, "uM", 0), "massa_molar.*> 0")
  expect_error(converter_para_mg_l(100, "uM", -10), "massa_molar.*> 0")
})

test_that("F6: vol decimal pequeno (0.5 mL) funciona", {
  expect_equal(calcular_massa(1650, 0.5), 0.825, tolerance = TOL)
})

test_that("F7: conc decimal muito pequena (0.0001 mg/L) funciona", {
  expect_equal(calcular_massa(0.0001, 1000), 0.0001, tolerance = TOL)
})

test_that("F8: unidade com umol/L explicito nao suportado (precisa uM)", {
  expect_error(converter_para_mg_l(100, "umol/L", 200), "Unidade nao suportada")
})

test_that("F9: massa exatamente 1 mg formatada como '1 mg' ou '1,00 mg'", {
  res <- formatar_massa(1)
  expect_match(res, "^1.*mg$")
  expect_false(grepl("g$", res) && !grepl("mg$", res))
})

test_that("F10: massa exatamente 1000 mg formatada como '1 g' ou '1,00 g'", {
  res <- formatar_massa(1000)
  expect_match(res, "^1.*g$")
})

test_that("F11: massa zero formatada como '0 mg'", {
  expect_equal(formatar_massa(0), "0 mg")
})

test_that("F12: formatar_massa vetorizada", {
  res <- formatar_massa(c(1650, 0.025, 0))
  expect_equal(length(res), 3)
  expect_match(res[1], "g$")
  expect_match(res[2], "ug$")
  expect_equal(res[3], "0 mg")
})