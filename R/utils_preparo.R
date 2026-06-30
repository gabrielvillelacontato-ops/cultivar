#' Utilitarios de gestao de preparos de meios
#'
#' Funcoes puras (recebem conexao DBI, retornam dados ou levantam erro).
#' Toda escrita registra no audit_log na mesma transacao.
#'
#' Conceito chave: ao iniciar um preparo, a composicao atual do meio e
#' COPIADA (snapshot) para preparo_componentes_usados. Esse snapshot e
#' imutavel — se o meio for editado depois, o preparo continua referenciando
#' a composicao que existia no momento da criacao. ALCOA+ pleno.
#'
#' Schema usado: preparo_componentes_usados tem concentracao_alvo_mg_l,
#' massa_teorica_mg, massa_pesada_mg, observacao (unica). desvio_percentual
#' e calculado em runtime (nao armazenado). ordem fica por pcu.id (insercao).
#'
#' @noRd

# ---------------------------------------------------------------------
# Constantes
# ---------------------------------------------------------------------

.TOLERANCIA_PESAGEM_PCT <- 5
.STATUS_VALIDOS <- c("rascunho", "em_preparo", "concluido", "descartado")
.STATUS_EDITAVEIS <- c("rascunho", "em_preparo")

# ---------------------------------------------------------------------
# Iniciar preparo
# ---------------------------------------------------------------------

iniciar_preparo <- function(con, meio_id, volume_ml, operador_id,
                            tenant_id = TENANT_DEFAULT_ID) {
  meio_id <- as.integer(meio_id)
  operador_id <- as.integer(operador_id)
  
  if (!is.numeric(volume_ml) || length(volume_ml) != 1L ||
      is.na(volume_ml) || volume_ml <= 0) {
    stop("volume_ml deve ser numerico > 0.", call. = FALSE)
  }
  volume_ml <- as.numeric(volume_ml)
  
  op <- DBI::dbGetQuery(
    con,
    "SELECT id, ativo, deleted_at FROM operadores
     WHERE id = ? AND tenant_id = ?;",
    params = list(operador_id, tenant_id)
  )
  if (nrow(op) == 0L) stop("Operador nao encontrado.", call. = FALSE)
  if (op$ativo[1] != 1L || !is.na(op$deleted_at[1])) {
    stop("Operador inativo ou arquivado.", call. = FALSE)
  }
  
  meio <- DBI::dbGetQuery(
    con,
    "SELECT id, codigo_curto, bloqueado_preparo, nota_incerteza, deleted_at
     FROM meios WHERE id = ? AND tenant_id = ?;",
    params = list(meio_id, tenant_id)
  )
  if (nrow(meio) == 0L) {
    stop("Meio nao encontrado (id=", meio_id, ").", call. = FALSE)
  }
  if (!is.na(meio$deleted_at[1])) {
    stop("Meio '", meio$codigo_curto[1], "' foi arquivado.", call. = FALSE)
  }
  if (meio$bloqueado_preparo[1] == 1L) {
    motivo <- if (is.na(meio$nota_incerteza[1])) "sem motivo registrado"
    else meio$nota_incerteza[1]
    stop("Meio '", meio$codigo_curto[1],
         "' esta bloqueado para preparo. Motivo: ", motivo,
         call. = FALSE)
  }
  
  comp <- DBI::dbGetQuery(
    con,
    "SELECT mc.componente_id, c.nome,
            mc.concentracao_mg_l, mc.observacao, mc.ordem_exibicao
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = ?
     ORDER BY mc.ordem_exibicao, c.nome;",
    params = list(meio_id)
  )
  if (nrow(comp) == 0L) {
    stop("Meio '", meio$codigo_curto[1],
         "' nao tem componentes cadastrados.", call. = FALSE)
  }
  
  data_hoje <- format(Sys.Date(), "%Y-%m-%d")
  lote <- gerar_lote_interno(con, meio$codigo_curto[1], data_hoje, tenant_id)
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "INSERT INTO preparos
         (tenant_id, meio_id, operador_id, lote_interno,
          volume_final_ml, status)
       VALUES (?, ?, ?, ?, ?, 'rascunho');",
      params = list(tenant_id, meio_id, operador_id, lote, volume_ml)
    )
    preparo_id <- DBI::dbGetQuery(
      con,
      "SELECT id FROM preparos WHERE lote_interno = ? AND tenant_id = ?;",
      params = list(lote, tenant_id)
    )$id[1]
    
    # Snapshot dos componentes
    for (i in seq_len(nrow(comp))) {
      massa_teorica <- calcular_massa(comp$concentracao_mg_l[i], volume_ml)
      obs_inicial <- if (is.na(comp$observacao[i])) NA_character_
      else comp$observacao[i]
      DBI::dbExecute(
        con,
        "INSERT INTO preparo_componentes_usados
           (preparo_id, componente_id, concentracao_alvo_mg_l,
            massa_teorica_mg, observacao)
         VALUES (?, ?, ?, ?, ?);",
        params = list(preparo_id, comp$componente_id[i],
                      comp$concentracao_mg_l[i], massa_teorica, obs_inicial)
      )
    }
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          valores_depois)
       VALUES (?, ?, 'preparos', ?, 'INSERT', ?);",
      params = list(
        tenant_id, operador_id, as.integer(preparo_id),
        sprintf('{"lote":"%s","meio_id":%d,"volume_ml":%s,"status":"rascunho"}',
                lote, meio_id, volume_ml)
      )
    )
    DBI::dbCommit(con)
    as.integer(preparo_id)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao iniciar preparo: ", conditionMessage(e), call. = FALSE)
  })
}

# ---------------------------------------------------------------------
# Carregar preparo
# ---------------------------------------------------------------------

carregar_preparo <- function(con, preparo_id, tenant_id = TENANT_DEFAULT_ID) {
  preparo_id <- as.integer(preparo_id)
  
  preparo <- DBI::dbGetQuery(
    con,
    "SELECT p.id, p.tenant_id, p.meio_id, p.operador_id, p.lote_interno,
            p.volume_final_ml, p.status, p.ph_medido, p.observacoes,
            p.iniciado_em, p.concluido_em, p.motivo_descarte,
            m.codigo_curto AS meio_codigo, m.nome AS meio_nome,
            m.ph_alvo, m.flag_incerteza, m.nota_incerteza,
            o.nome AS operador_nome
     FROM preparos p
     JOIN meios m ON m.id = p.meio_id
     JOIN operadores o ON o.id = p.operador_id
     WHERE p.id = ? AND p.tenant_id = ?;",
    params = list(preparo_id, tenant_id)
  )
  if (nrow(preparo) == 0L) {
    stop("Preparo nao encontrado (id=", preparo_id, ").", call. = FALSE)
  }
  
  # desvio_percentual calculado em runtime via SQL
  componentes <- DBI::dbGetQuery(
    con,
    "SELECT pcu.id AS pcu_id,
            pcu.componente_id, c.nome, c.formula, c.cas, c.massa_molar,
            pcu.concentracao_alvo_mg_l, pcu.massa_teorica_mg,
            pcu.massa_pesada_mg,
            CASE
              WHEN pcu.massa_pesada_mg IS NULL THEN NULL
              ELSE CAST(ABS(pcu.massa_pesada_mg - pcu.massa_teorica_mg) AS REAL)
                   / pcu.massa_teorica_mg * 100.0
            END AS desvio_percentual,
            pcu.observacao
     FROM preparo_componentes_usados pcu
     JOIN componentes c ON c.id = pcu.componente_id
     WHERE pcu.preparo_id = ?
     ORDER BY pcu.id;",
    params = list(preparo_id)
  )
  
  list(preparo = preparo, componentes = componentes)
}

# ---------------------------------------------------------------------
# Listar rascunhos do operador
# ---------------------------------------------------------------------

listar_rascunhos_operador <- function(con, operador_id,
                                      tenant_id = TENANT_DEFAULT_ID) {
  operador_id <- as.integer(operador_id)
  DBI::dbGetQuery(
    con,
    "SELECT p.id, p.lote_interno, p.volume_final_ml, p.iniciado_em,
            p.status,
            m.codigo_curto AS meio_codigo, m.nome AS meio_nome,
            (SELECT COUNT(*) FROM preparo_componentes_usados pcu
             WHERE pcu.preparo_id = p.id
               AND pcu.massa_pesada_mg IS NOT NULL) AS n_pesados,
            (SELECT COUNT(*) FROM preparo_componentes_usados pcu
             WHERE pcu.preparo_id = p.id) AS n_total
     FROM preparos p
     JOIN meios m ON m.id = p.meio_id
     WHERE p.tenant_id = ? AND p.operador_id = ?
       AND p.status IN ('rascunho','em_preparo')
     ORDER BY p.iniciado_em DESC;",
    params = list(tenant_id, operador_id)
  )
}

# ---------------------------------------------------------------------
# Salvar pesagem
# ---------------------------------------------------------------------

salvar_pesagem <- function(con, preparo_id, componente_id,
                           massa_real_mg, operador_id,
                           observacao = NA_character_,
                           tenant_id = TENANT_DEFAULT_ID) {
  preparo_id <- as.integer(preparo_id)
  componente_id <- as.integer(componente_id)
  operador_id <- as.integer(operador_id)
  
  if (!is.numeric(massa_real_mg) || length(massa_real_mg) != 1L ||
      is.na(massa_real_mg) || massa_real_mg <= 0) {
    stop("massa_real_mg deve ser numerico > 0.", call. = FALSE)
  }
  massa_real_mg <- as.numeric(massa_real_mg)
  
  preparo <- DBI::dbGetQuery(
    con,
    "SELECT id, operador_id, status FROM preparos
     WHERE id = ? AND tenant_id = ?;",
    params = list(preparo_id, tenant_id)
  )
  if (nrow(preparo) == 0L) stop("Preparo nao encontrado.", call. = FALSE)
  if (preparo$operador_id[1] != operador_id) {
    stop("Apenas o operador dono do preparo pode registrar pesagens. ",
         "Preparo pertence ao operador id=", preparo$operador_id[1],
         ".", call. = FALSE)
  }
  if (!preparo$status[1] %in% .STATUS_EDITAVEIS) {
    stop("Preparo no status '", preparo$status[1],
         "' nao pode ser editado.", call. = FALSE)
  }
  
  pcu <- DBI::dbGetQuery(
    con,
    "SELECT id, massa_teorica_mg FROM preparo_componentes_usados
     WHERE preparo_id = ? AND componente_id = ?;",
    params = list(preparo_id, componente_id)
  )
  if (nrow(pcu) == 0L) {
    stop("Componente nao faz parte deste preparo.", call. = FALSE)
  }
  massa_teorica <- pcu$massa_teorica_mg[1]
  
  desvio <- abs(massa_real_mg - massa_teorica) / massa_teorica * 100
  classificacao <- if (desvio <= .TOLERANCIA_PESAGEM_PCT) {
    "ok"
  } else if (desvio <= .TOLERANCIA_PESAGEM_PCT * 2) {
    "atencao"
  } else {
    "fora_tolerancia"
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE preparo_componentes_usados
         SET massa_pesada_mg = ?, observacao = ?
       WHERE id = ?;",
      params = list(massa_real_mg,
                    if (is.na(observacao)) NA_character_ else observacao,
                    pcu$id[1])
    )
    
    if (preparo$status[1] == "rascunho") {
      DBI::dbExecute(
        con,
        "UPDATE preparos SET status = 'em_preparo' WHERE id = ?;",
        params = list(preparo_id)
      )
    }
    
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto, valores_depois)
       VALUES (?, ?, 'preparos', ?, 'UPDATE', ?, ?);",
      params = list(
        tenant_id, operador_id, preparo_id,
        sprintf("Pesagem componente_id=%d", componente_id),
        sprintf('{"componente_id":%d,"massa_pesada_mg":%s,"desvio_pct":%.2f,"classificacao":"%s"}',
                componente_id, massa_real_mg, desvio, classificacao)
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao salvar pesagem: ", conditionMessage(e), call. = FALSE)
  })
  
  list(
    massa_real_mg = massa_real_mg,
    desvio_percentual = desvio,
    classificacao = classificacao
  )
}

# ---------------------------------------------------------------------
# Salvar pH e observacoes
# ---------------------------------------------------------------------

salvar_ph_observacoes <- function(con, preparo_id, ph_medido = NA_real_,
                                  observacoes = NA_character_,
                                  operador_id,
                                  tenant_id = TENANT_DEFAULT_ID) {
  preparo_id <- as.integer(preparo_id)
  operador_id <- as.integer(operador_id)
  
  if (!is.na(ph_medido)) {
    if (!is.numeric(ph_medido) || length(ph_medido) != 1L ||
        ph_medido < 0 || ph_medido > 14) {
      stop("ph_medido deve estar entre 0 e 14 (ou NA).", call. = FALSE)
    }
    ph_medido <- as.numeric(ph_medido)
  }
  
  preparo <- DBI::dbGetQuery(
    con,
    "SELECT id, operador_id, status FROM preparos
     WHERE id = ? AND tenant_id = ?;",
    params = list(preparo_id, tenant_id)
  )
  if (nrow(preparo) == 0L) stop("Preparo nao encontrado.", call. = FALSE)
  if (preparo$operador_id[1] != operador_id) {
    stop("Apenas o operador dono pode editar.", call. = FALSE)
  }
  if (!preparo$status[1] %in% .STATUS_EDITAVEIS) {
    stop("Preparo no status '", preparo$status[1],
         "' nao pode ser editado.", call. = FALSE)
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE preparos SET ph_medido = ?, observacoes = ? WHERE id = ?;",
      params = list(
        if (is.na(ph_medido)) NA_real_ else ph_medido,
        if (is.na(observacoes)) NA_character_ else observacoes,
        preparo_id
      )
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto, valores_depois)
       VALUES (?, ?, 'preparos', ?, 'UPDATE', 'pH/observacoes', ?);",
      params = list(
        tenant_id, operador_id, preparo_id,
        sprintf('{"ph_medido":%s,"observacoes":%s}',
                if (is.na(ph_medido)) "null" else as.character(ph_medido),
                if (is.na(observacoes)) "null"
                else sprintf('"%s"', gsub('"', '\\\\"', observacoes)))
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao salvar pH/observacoes: ", conditionMessage(e),
         call. = FALSE)
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------
# Concluir preparo
# ---------------------------------------------------------------------

concluir_preparo <- function(con, preparo_id, operador_id,
                             tenant_id = TENANT_DEFAULT_ID) {
  preparo_id <- as.integer(preparo_id)
  operador_id <- as.integer(operador_id)
  
  preparo <- DBI::dbGetQuery(
    con,
    "SELECT id, operador_id, status, lote_interno FROM preparos
     WHERE id = ? AND tenant_id = ?;",
    params = list(preparo_id, tenant_id)
  )
  if (nrow(preparo) == 0L) stop("Preparo nao encontrado.", call. = FALSE)
  if (preparo$operador_id[1] != operador_id) {
    stop("Apenas o operador dono pode concluir.", call. = FALSE)
  }
  if (!preparo$status[1] %in% .STATUS_EDITAVEIS) {
    stop("Preparo no status '", preparo$status[1],
         "' nao pode ser concluido.", call. = FALSE)
  }
  
  faltantes <- DBI::dbGetQuery(
    con,
    "SELECT c.nome FROM preparo_componentes_usados pcu
     JOIN componentes c ON c.id = pcu.componente_id
     WHERE pcu.preparo_id = ? AND pcu.massa_pesada_mg IS NULL;",
    params = list(preparo_id)
  )
  if (nrow(faltantes) > 0L) {
    stop("Nao e possivel concluir: ", nrow(faltantes),
         " componente(s) ainda nao pesado(s): ",
         paste(faltantes$nome, collapse = ", "), ".", call. = FALSE)
  }
  
  # desvio_percentual calculado em runtime
  resumo <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n_total,
            SUM(CASE
                  WHEN CAST(ABS(massa_pesada_mg - massa_teorica_mg) AS REAL)
                       / massa_teorica_mg * 100.0 > ?
                  THEN 1 ELSE 0
                END) AS n_fora
     FROM preparo_componentes_usados WHERE preparo_id = ?;",
    params = list(.TOLERANCIA_PESAGEM_PCT * 2, preparo_id)
  )
  
  agora <- .now_utc()
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE preparos SET status = 'concluido', concluido_em = ?
       WHERE id = ?;",
      params = list(agora, preparo_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto, valores_depois)
       VALUES (?, ?, 'preparos', ?, 'UPDATE', 'Preparo concluido', ?);",
      params = list(
        tenant_id, operador_id, preparo_id,
        sprintf('{"status":"concluido","n_total":%d,"n_fora_tolerancia":%d,"lote":"%s"}',
                resumo$n_total[1], resumo$n_fora[1], preparo$lote_interno[1])
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao concluir preparo: ", conditionMessage(e), call. = FALSE)
  })
  
  list(
    lote_interno = preparo$lote_interno[1],
    n_componentes = as.integer(resumo$n_total[1]),
    n_fora_tolerancia = as.integer(resumo$n_fora[1])
  )
}

# ---------------------------------------------------------------------
# Descartar preparo
# ---------------------------------------------------------------------

descartar_preparo <- function(con, preparo_id, motivo, solicitante_id,
                              tenant_id = TENANT_DEFAULT_ID) {
  preparo_id <- as.integer(preparo_id)
  solicitante_id <- as.integer(solicitante_id)
  
  if (is.null(motivo) || length(motivo) != 1L || is.na(motivo) ||
      !is.character(motivo) || !nzchar(trimws(motivo))) {
    stop("Motivo do descarte e obrigatorio.", call. = FALSE)
  }
  motivo <- trimws(motivo)
  
  preparo <- DBI::dbGetQuery(
    con,
    "SELECT id, operador_id, status FROM preparos
     WHERE id = ? AND tenant_id = ?;",
    params = list(preparo_id, tenant_id)
  )
  if (nrow(preparo) == 0L) stop("Preparo nao encontrado.", call. = FALSE)
  if (preparo$status[1] == "descartado") {
    stop("Preparo ja esta descartado.", call. = FALSE)
  }
  
  solicitante <- DBI::dbGetQuery(
    con, "SELECT papel FROM operadores WHERE id = ?;",
    params = list(solicitante_id)
  )
  if (nrow(solicitante) == 0L) {
    stop("Solicitante nao encontrado.", call. = FALSE)
  }
  papel <- solicitante$papel[1]
  
  if (papel == "operador") {
    if (preparo$operador_id[1] != solicitante_id) {
      stop("Operador so pode descartar proprio preparo.", call. = FALSE)
    }
    if (!preparo$status[1] %in% .STATUS_EDITAVEIS) {
      stop("Operador so pode descartar preparo em rascunho ou em_preparo. ",
           "Status atual: '", preparo$status[1],
           "'. Procure um supervisor.", call. = FALSE)
    }
  }
  
  DBI::dbBegin(con)
  tryCatch({
    DBI::dbExecute(
      con,
      "UPDATE preparos SET status = 'descartado', motivo_descarte = ?
       WHERE id = ?;",
      params = list(motivo, preparo_id)
    )
    DBI::dbExecute(
      con,
      "INSERT INTO audit_log
         (tenant_id, operador_id, entidade_tabela, entidade_id, acao,
          contexto, valores_depois)
       VALUES (?, ?, 'preparos', ?, 'DELETE', ?, ?);",
      params = list(
        tenant_id, solicitante_id, preparo_id,
        sprintf("Descarte: %s", motivo),
        sprintf('{"status":"descartado","motivo":"%s"}',
                gsub('"', '\\\\"', motivo))
      )
    )
    DBI::dbCommit(con)
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Falha ao descartar: ", conditionMessage(e), call. = FALSE)
  })
  invisible(NULL)
}

# ---------------------------------------------------------------------
# Listagem (supervisor+)
# ---------------------------------------------------------------------

listar_preparos <- function(con, filtro_status = NULL,
                            filtro_operador_id = NULL,
                            limite = 200L,
                            tenant_id = TENANT_DEFAULT_ID) {
  where_clauses <- c("p.tenant_id = ?")
  params <- list(tenant_id)
  
  if (!is.null(filtro_status) && length(filtro_status) > 0L) {
    placeholders <- paste(rep("?", length(filtro_status)), collapse = ",")
    where_clauses <- c(where_clauses,
                       paste0("p.status IN (", placeholders, ")"))
    params <- c(params, as.list(filtro_status))
  }
  
  if (!is.null(filtro_operador_id) && !is.na(filtro_operador_id)) {
    where_clauses <- c(where_clauses, "p.operador_id = ?")
    params <- c(params, list(as.integer(filtro_operador_id)))
  }
  
  sql <- paste0(
    "SELECT p.id, p.lote_interno, p.volume_final_ml, p.status,
            p.ph_medido, p.iniciado_em, p.concluido_em, p.motivo_descarte,
            m.codigo_curto AS meio_codigo, m.nome AS meio_nome,
            o.nome AS operador_nome
     FROM preparos p
     JOIN meios m ON m.id = p.meio_id
     JOIN operadores o ON o.id = p.operador_id
     WHERE ", paste(where_clauses, collapse = " AND "),
    " ORDER BY p.iniciado_em DESC LIMIT ?;"
  )
  DBI::dbGetQuery(con, sql, params = c(params, list(as.integer(limite))))
}