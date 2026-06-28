# =====================================================================
# CultivaR — Seed do catalogo validado (Dia 2)
# =====================================================================
# Popula o banco com:
#   - 12 categorias de componente
#   - 3 categorias de meio
#   - 50 componentes quimicos (MW e CAS verificados)
#   - 11 meios validados (MS, B5, e pipeline S&R 2001)
#   - 1 workflow (Transformacao Algodao S&R 2001) com 8 etapas
#
# Idempotente: pode ser rodado de novo sem efeito colateral.
# =====================================================================

devtools::load_all()

con <- db_connect(DB_PATH_DEV)

tryCatch({
  cat("=== Seed do catalogo CultivaR ===\n\n")
  
  n_cc <- seed_categorias_componente(con)
  cat("Categorias de componente inseridas: ", n_cc, "\n", sep = "")
  
  n_cm <- seed_categorias_meio(con)
  cat("Categorias de meio inseridas:       ", n_cm, "\n", sep = "")
  
  n_co <- seed_componentes(con)
  cat("Componentes quimicos inseridos:     ", n_co, "\n", sep = "")
  
  n_me <- seed_meios_validados(con)
  cat("Meios validados inseridos:          ", n_me, "\n", sep = "")
  
  n_wf <- seed_workflow_algodao(con)
  cat("Workflows inseridos:                ", n_wf, "\n\n", sep = "")
  
  # Resumo final
  cat("=== Estado do catalogo apos seed ===\n")
  total_cc <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM categorias_componente;")$n
  total_cm <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM categorias_meio;")$n
  total_co <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM componentes;")$n
  total_me <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM meios;")$n
  total_mc <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM meio_componentes;")$n
  total_wf <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM workflows;")$n
  total_we <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM workflow_etapas;")$n
  
  cat("Total categorias_componente: ", total_cc, "\n", sep = "")
  cat("Total categorias_meio:       ", total_cm, "\n", sep = "")
  cat("Total componentes:           ", total_co, "\n", sep = "")
  cat("Total meios:                 ", total_me, "\n", sep = "")
  cat("Total meio_componentes:      ", total_mc, "\n", sep = "")
  cat("Total workflows:             ", total_wf, "\n", sep = "")
  cat("Total workflow_etapas:       ", total_we, "\n\n", sep = "")
  
  # Lista os meios com flag de incerteza para revisao
  flagged <- DBI::dbGetQuery(
    con,
    "SELECT codigo_curto, nome, nota_incerteza FROM meios WHERE flag_incerteza = 1;"
  )
  if (nrow(flagged) > 0L) {
    cat("=== Meios com flag de incerteza ===\n")
    for (i in seq_len(nrow(flagged))) {
      cat("[", flagged$codigo_curto[i], "] ", flagged$nome[i], "\n",
          "  -> ", flagged$nota_incerteza[i], "\n\n", sep = "")
    }
  }
  
}, finally = {
  db_disconnect(con)
})

cat("Seed concluido.\n")