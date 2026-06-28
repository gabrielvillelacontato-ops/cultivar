# =====================================================================
# CultivaR — Aplica migration 002 (correcoes pos-revisao Codex)
# =====================================================================
# Idempotente: rodar de novo nao duplica registros nem reverte mudancas.
# =====================================================================

devtools::load_all()

con <- db_connect(DB_PATH_DEV)

tryCatch({
  cat("=== Aplicando migration 002 ===\n\n")
  db_apply_migrations(con, MIGRATIONS_DIR, verbose = TRUE)
  
  # ---- Verificacoes ----
  cat("\n=== Verificacoes pos-migration ===\n")
  
  # 1. Coluna bloqueado_preparo existe
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(meios);")$name
  if (!"bloqueado_preparo" %in% cols) {
    stop("Coluna meios.bloqueado_preparo nao foi criada.")
  }
  cat("[OK] Coluna meios.bloqueado_preparo criada.\n")
  
  # 2. PIA esta bloqueada
  pia <- DBI::dbGetQuery(
    con,
    "SELECT codigo_curto, bloqueado_preparo, flag_incerteza
     FROM meios WHERE tenant_id = 1 AND codigo_curto = 'PIA';"
  )
  if (nrow(pia) == 0L || pia$bloqueado_preparo[1] != 1L) {
    stop("PIA nao esta bloqueada para preparo.")
  }
  cat("[OK] PIA bloqueada para preparo.\n")
  
  # 3. Componente Fe-Na-EDTA existe
  fe <- DBI::dbGetQuery(
    con,
    "SELECT nome, massa_molar, cas FROM componentes
     WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA';"
  )
  if (nrow(fe) == 0L) stop("Componente Fe-Na-EDTA nao foi inserido.")
  cat("[OK] Fe-Na-EDTA cadastrado: MW = ", fe$massa_molar[1],
      ", CAS = ", fe$cas[1], "\n", sep = "")
  
  # 4. Na2EDTA renomeado
  edta <- DBI::dbGetQuery(
    con,
    "SELECT nome FROM componentes WHERE tenant_id = 1
       AND nome IN ('Na2EDTA','Na2EDTA.2H2O');"
  )
  if (!"Na2EDTA.2H2O" %in% edta$nome) {
    stop("Na2EDTA nao foi renomeado para Na2EDTA.2H2O.")
  }
  cat("[OK] Na2EDTA renomeado para Na2EDTA.2H2O.\n")
  
  # 5. Meios usando Fe-Na-EDTA
  fe_id <- DBI::dbGetQuery(
    con, "SELECT id FROM componentes WHERE tenant_id = 1 AND nome = 'Fe-Na-EDTA';"
  )$id
  meios_com_fe <- DBI::dbGetQuery(
    con,
    "SELECT m.codigo_curto, mc.concentracao_mg_l
     FROM meio_componentes mc
     JOIN meios m ON m.id = mc.meio_id
     WHERE mc.componente_id = ?
     ORDER BY m.codigo_curto;",
    params = list(fe_id)
  )
  cat("[OK] Meios usando Fe-Na-EDTA:\n")
  print(meios_com_fe, row.names = FALSE)
  
  # 6. Nenhum meio MS/B5 ainda usa FeSO4.7H2O ou Na2EDTA.2H2O
  meios_fe_antigo <- DBI::dbGetQuery(
    con,
    "SELECT m.codigo_curto, c.nome AS componente
     FROM meio_componentes mc
     JOIN meios m ON m.id = mc.meio_id
     JOIN componentes c ON c.id = mc.componente_id
     WHERE c.nome IN ('FeSO4.7H2O','Na2EDTA.2H2O')
       AND m.codigo_curto IN ('MSO','MSR','B5','MS0','P1AS','P1S','P7M','MSBOK','EG3','MS3');"
  )
  if (nrow(meios_fe_antigo) > 0L) {
    cat("[ATENCAO] Meios ainda referenciando Fe antigo:\n")
    print(meios_fe_antigo, row.names = FALSE)
  } else {
    cat("[OK] Nenhum meio MS/B5 referencia FeSO4.7H2O ou Na2EDTA.2H2O.\n")
  }
  
  # 7. B5 usa CuSO4.5H2O e nao mais CuSO4 anidro
  b5_cu <- DBI::dbGetQuery(
    con,
    "SELECT c.nome, mc.concentracao_mg_l
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = (SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'B5')
       AND c.nome LIKE 'CuSO4%';"
  )
  cat("[OK] B5 - Cu source:\n")
  print(b5_cu, row.names = FALSE)
  
  # 8. PIA MES corrigido
  pia_mes <- DBI::dbGetQuery(
    con,
    "SELECT mc.concentracao_mg_l
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = (SELECT id FROM meios WHERE tenant_id = 1 AND codigo_curto = 'PIA')
       AND c.nome = 'MES';"
  )
  cat("[OK] PIA MES = ", pia_mes$concentracao_mg_l[1], " mg/L (esperado 1464.3)\n", sep = "")
  
  # 9. Schema version
  versoes <- DBI::dbGetQuery(
    con, "SELECT version, applied_at FROM schema_version ORDER BY version;"
  )
  cat("\n=== Migrations aplicadas ===\n")
  print(versoes, row.names = FALSE)
  
}, finally = {
  db_disconnect(con)
})

cat("\nMigration 002 aplicada com sucesso.\n")