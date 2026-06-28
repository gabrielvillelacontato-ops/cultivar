# =====================================================================
# CultivaR — Script de criacao do banco SQLite local (Dia 1)
# =====================================================================
# Roda UMA VEZ por maquina de desenvolvimento. Cria o arquivo
# inst/extdata/cultivar.sqlite e aplica todas as migrations pendentes.
#
# Idempotente: pode ser rodado de novo sem efeito colateral.
#
# Pre-requisito: rodar a partir da raiz do projeto cultivaR
#   (RStudio abre o projeto e o WD ja fica correto).
# =====================================================================

devtools::load_all()

con <- db_connect(DB_PATH_DEV)

tryCatch({
  db_apply_migrations(con, MIGRATIONS_DIR, verbose = TRUE)
  
  # ---- Verificacoes ----
  tabelas_esperadas <- c(
    "audit_log", "categorias_componente", "categorias_meio", "componentes",
    "meio_componentes", "meios", "operadores", "pops",
    "preparo_componentes_usados", "preparos", "reagentes_lotes",
    "schema_version", "tenants", "workflow_etapas", "workflows"
  )
  tabelas <- sort(DBI::dbListTables(con))
  faltando <- setdiff(tabelas_esperadas, tabelas)
  if (length(faltando) > 0L) {
    stop("Tabelas faltando apos migration: ", paste(faltando, collapse = ", "))
  }
  
  cat("\nTabelas no banco (", length(tabelas), "):\n", sep = "")
  cat(paste0("  - ", tabelas, collapse = "\n"), "\n", sep = "")
  
  versoes <- DBI::dbGetQuery(
    con,
    "SELECT version, applied_at FROM schema_version ORDER BY version;"
  )
  cat("\nMigrations aplicadas:\n")
  print(versoes, row.names = FALSE)
  
  tenants <- DBI::dbGetQuery(con, "SELECT id, nome FROM tenants;")
  if (nrow(tenants) == 0L) {
    stop("Tenant default nao foi inserido. Verifique a migration.")
  }
  cat("\nTenants cadastrados:\n")
  print(tenants, row.names = FALSE)
  
  # Verifica PRAGMAs em vigor
  fk <- DBI::dbGetQuery(con, "PRAGMA foreign_keys;")$foreign_keys
  jm <- DBI::dbGetQuery(con, "PRAGMA journal_mode;")$journal_mode
  cat("\nPRAGMA foreign_keys = ", fk, " (esperado 1)\n", sep = "")
  cat("PRAGMA journal_mode = ", jm, " (esperado wal)\n", sep = "")
  
}, finally = {
  db_disconnect(con)
})

cat("\nBanco pronto em: ", normalizePath(DB_PATH_DEV, mustWork = FALSE), "\n", sep = "")