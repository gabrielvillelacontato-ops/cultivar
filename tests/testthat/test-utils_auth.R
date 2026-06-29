# =====================================================================
# Testes do modulo de autenticacao
# =====================================================================

# Helpers de setup
.setup_banco_auth <- function() {
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
  con
}

# ---------------------------------------------------------------------
# Validacao de formato
# ---------------------------------------------------------------------

test_that("PIN com 4 digitos passa validacao", {
  expect_silent(.validar_pin_formato("1234"))
  expect_silent(.validar_pin_formato("0000"))
})

test_that("PIN com formato invalido da erro", {
  expect_error(.validar_pin_formato("123"), "4 digitos")
  expect_error(.validar_pin_formato("12345"), "4 digitos")
  expect_error(.validar_pin_formato("abcd"), "4 digitos")
  expect_error(.validar_pin_formato("12 4"), "4 digitos")
  expect_error(.validar_pin_formato(NA_character_), "vazia")
  expect_error(.validar_pin_formato(NULL), "vazia")
})

test_that("Papel valido passa", {
  expect_silent(.validar_papel("operador"))
  expect_silent(.validar_papel("supervisor"))
  expect_silent(.validar_papel("admin"))
})

test_that("Papel invalido da erro", {
  expect_error(.validar_papel("root"), "invalido")
  expect_error(.validar_papel(""), "invalido")
  expect_error(.validar_papel(NA_character_), "vazia")
})

test_that("Nome valido passa", {
  expect_silent(.validar_nome("Mauro"))
  expect_silent(.validar_nome("Maria Silva"))
})

test_that("Nome invalido da erro", {
  expect_error(.validar_nome(""), "vazia|espacos")
  expect_error(.validar_nome("   "), "espacos")
  expect_error(.validar_nome(strrep("a", 101)), "muito longo")
})

# ---------------------------------------------------------------------
# Hash e verificacao
# ---------------------------------------------------------------------

test_that("hash_pin gera hash valido", {
  h <- hash_pin("1234")
  expect_true(is.raw(h) || is.character(h))
})

test_that("verificar_pin: PIN correto retorna TRUE", {
  h <- hash_pin("1234")
  expect_true(verificar_pin("1234", h))
})

test_that("verificar_pin: PIN incorreto retorna FALSE", {
  h <- hash_pin("1234")
  expect_false(verificar_pin("9999", h))
})

test_that("hash_pin gera hashes diferentes para o mesmo PIN (salt)", {
  h1 <- hash_pin("1234")
  h2 <- hash_pin("1234")
  expect_false(identical(h1, h2))
  # Mas ambos verificam o mesmo PIN
  expect_true(verificar_pin("1234", h1))
  expect_true(verificar_pin("1234", h2))
})

# ---------------------------------------------------------------------
# Setup inicial
# ---------------------------------------------------------------------

test_that("Sistema vazio precisa setup inicial", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  expect_true(precisa_setup_inicial(con))
  expect_equal(contar_operadores_ativos(con), 0L)
})

test_that("Primeiro operador deve ser admin", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  expect_error(
    criar_operador(con, "Mauro", "1234", "operador"),
    "primeiro operador.*admin"
  )
})

test_that("Primeiro admin criado: sistema deixa de precisar setup", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id <- criar_operador(con, "Mauro", "1234", "admin")
  expect_true(is.integer(id))
  expect_false(precisa_setup_inicial(con))
  expect_equal(contar_admins_ativos(con), 1L)
})

# ---------------------------------------------------------------------
# Criacao de operadores
# ---------------------------------------------------------------------

test_that("Nome duplicado da erro", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  criar_operador(con, "Mauro", "1234", "admin")
  expect_error(
    criar_operador(con, "Mauro", "5678", "operador"),
    "Ja existe operador"
  )
})

test_that("Criacao registra audit_log INSERT", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id <- criar_operador(con, "Mauro", "1234", "admin")
  audit <- DBI::dbGetQuery(con,
                           "SELECT acao, entidade_tabela, entidade_id FROM audit_log
     WHERE entidade_tabela = 'operadores';")
  expect_equal(nrow(audit), 1L)
  expect_equal(audit$acao[1], "INSERT")
  expect_equal(audit$entidade_id[1], id)
})

# ---------------------------------------------------------------------
# Lockout
# ---------------------------------------------------------------------

test_that("esta_bloqueado: timestamp futuro retorna TRUE", {
  futuro <- format(Sys.time() + 600,
                   "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  expect_true(esta_bloqueado(futuro))
})

test_that("esta_bloqueado: timestamp passado retorna FALSE", {
  passado <- format(Sys.time() - 600,
                    "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  expect_false(esta_bloqueado(passado))
})

test_that("esta_bloqueado: NULL, NA, string vazia retornam FALSE", {
  expect_false(esta_bloqueado(NULL))
  expect_false(esta_bloqueado(NA_character_))
  expect_false(esta_bloqueado(""))
})

# ---------------------------------------------------------------------
# Login orquestrado
# ---------------------------------------------------------------------

test_that("autenticar: sucesso com PIN correto", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  criar_operador(con, "Mauro", "1234", "admin")
  res <- autenticar(con, "Mauro", "1234")
  expect_true(res$sucesso)
  expect_equal(res$motivo, "ok")
  expect_equal(res$operador$nome[1], "Mauro")
})

test_that("autenticar: PIN incorreto", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  criar_operador(con, "Mauro", "1234", "admin")
  res <- autenticar(con, "Mauro", "9999")
  expect_false(res$sucesso)
  expect_equal(res$motivo, "pin_incorreto")
})

test_that("autenticar: usuario inexistente nao loga", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  res <- autenticar(con, "Fantasma", "1234")
  expect_false(res$sucesso)
  expect_equal(res$motivo, "usuario_nao_encontrado")
})

test_that("autenticar: 5 tentativas falhas bloqueiam conta", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  criar_operador(con, "Mauro", "1234", "admin")
  
  for (i in 1:PIN_MAX_TENTATIVAS) {
    autenticar(con, "Mauro", "9999")
  }
  
  # Mesmo com PIN correto, deve estar bloqueado
  res <- autenticar(con, "Mauro", "1234")
  expect_false(res$sucesso)
  expect_equal(res$motivo, "bloqueado")
})

test_that("autenticar: login com sucesso zera tentativas_falhas", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  criar_operador(con, "Mauro", "1234", "admin")
  
  # 2 falhas
  autenticar(con, "Mauro", "9999")
  autenticar(con, "Mauro", "9999")
  
  # Sucesso
  autenticar(con, "Mauro", "1234")
  
  op <- buscar_operador_por_nome(con, "Mauro")
  expect_equal(as.integer(op$tentativas_falhas[1]), 0L)
})

# ---------------------------------------------------------------------
# Mudanca de PIN
# ---------------------------------------------------------------------

test_that("mudar_pin: auto-mudanca exige PIN atual correto", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id <- criar_operador(con, "Mauro", "1234", "admin")
  
  expect_error(
    mudar_pin(con, id, "5678", solicitante_id = id, pin_atual = "9999"),
    "PIN atual incorreto"
  )
  
  mudar_pin(con, id, "5678", solicitante_id = id, pin_atual = "1234")
  res <- autenticar(con, "Mauro", "5678")
  expect_true(res$sucesso)
})

test_that("mudar_pin: admin reseta PIN de outro sem PIN atual", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin1", "1111", "admin")
  id_op <- criar_operador(con, "Operador1", "2222", "operador",
                          criado_por_id = id_admin)
  
  mudar_pin(con, id_op, "9999", solicitante_id = id_admin)
  res <- autenticar(con, "Operador1", "9999")
  expect_true(res$sucesso)
})

test_that("mudar_pin: nao-admin nao pode resetar PIN de outro", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin1", "1111", "admin")
  id_sup <- criar_operador(con, "Sup1", "2222", "supervisor",
                           criado_por_id = id_admin)
  id_op <- criar_operador(con, "Op1", "3333", "operador",
                          criado_por_id = id_admin)
  
  expect_error(
    mudar_pin(con, id_op, "9999", solicitante_id = id_sup),
    "Apenas admins"
  )
})

# ---------------------------------------------------------------------
# Mudanca de papel
# ---------------------------------------------------------------------

test_that("mudar_papel: admin pode promover operador", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin1", "1111", "admin")
  id_op <- criar_operador(con, "Op1", "2222", "operador",
                          criado_por_id = id_admin)
  
  mudar_papel(con, id_op, "supervisor", solicitante_id = id_admin)
  op <- buscar_operador_por_nome(con, "Op1")
  expect_equal(op$papel[1], "supervisor")
})

test_that("mudar_papel: ultimo admin nao pode ser rebaixado", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin1", "1111", "admin")
  expect_error(
    mudar_papel(con, id_admin, "operador", solicitante_id = id_admin),
    "ultimo admin"
  )
})

test_that("mudar_papel: com 2 admins, um pode ser rebaixado", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id1 <- criar_operador(con, "Admin1", "1111", "admin")
  id2 <- criar_operador(con, "Admin2", "2222", "admin",
                        criado_por_id = id1)
  
  mudar_papel(con, id2, "operador", solicitante_id = id1)
  op <- buscar_operador_por_nome(con, "Admin2")
  expect_equal(op$papel[1], "operador")
})

# ---------------------------------------------------------------------
# Desativacao
# ---------------------------------------------------------------------

test_that("desativar_operador: ultimo admin protegido", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin1", "1111", "admin")
  expect_error(
    desativar_operador(con, id_admin, solicitante_id = id_admin),
    "ultimo admin"
  )
})

test_that("desativar_operador: nao-admin nao pode desativar", {
  con <- .setup_banco_auth()
  on.exit(DBI::dbDisconnect(con))
  id_admin <- criar_operador(con, "Admin1", "1111", "admin")
  id_sup <- criar_operador(con, "Sup1", "2222", "supervisor",
                           criado_por_id = id_admin)
  id_op <- criar_operador(con, "Op1", "3333", "operador",
                          criado_por_id = id_admin)
  expect_error(
    desativar_operador(con, id_op, solicitante_id = id_sup),
    "Apenas admins"
  )
})