#' Modulo Shiny: wizard de preparo de meios
#'
#' Wizard sequencial com 5 estados:
#'   "rascunhos" -> lista rascunhos + botao iniciar novo
#'   "iniciar"   -> escolhe meio + volume
#'   "pesar"     -> revisa snapshot + registra massa real de cada componente
#'   "finalizar" -> pH medido + observacoes + concluir
#'   "resumo"    -> lote gerado + estatisticas
#'
#' Descarte disponivel em pesar/finalizar (modal com motivo obrigatorio).
#' Rascunho persistente: sair e voltar retoma no estado correto (pesar).
#'
#' Helpers %||%, ui_fmt_br e ui_fmt_mg_l vem de R/utils_ui.R.
#'
#' IMPORTANTE: a tolerancia de pesagem (.PREP_TOLERANCIA_PCT) deve
#' permanecer sincronizada com a constante .TOLERANCIA_PESAGEM_PCT em
#' R/utils_preparo.R. Se uma mudar, a outra tambem tem que mudar.
#'
#' @noRd

# ---------------------------------------------------------------------
# Classificacao da pesagem
# Espelha .TOLERANCIA_PESAGEM_PCT do utils_preparo.R.
# ---------------------------------------------------------------------

.PREP_TOLERANCIA_PCT <- 5

#' @noRd
.classificar_pesagem <- function(massa_teorica, massa_pesada) {
  if (is.null(massa_pesada) || is.na(massa_pesada) ||
      is.null(massa_teorica) || is.na(massa_teorica) ||
      massa_teorica <= 0) return(NA_character_)
  desvio <- abs(massa_pesada - massa_teorica) / massa_teorica * 100
  if (desvio <= .PREP_TOLERANCIA_PCT)          "ok"
  else if (desvio <= .PREP_TOLERANCIA_PCT * 2) "atencao"
  else                                          "fora_tolerancia"
}

#' @noRd
.badge_pesagem <- function(classificacao) {
  if (is.null(classificacao) || is.na(classificacao)) {
    return(shiny::span(class = "badge bg-secondary", "aguardando"))
  }
  switch(
    classificacao,
    "ok" = shiny::span(class = "badge bg-success", "ok"),
    "atencao" = shiny::span(class = "badge bg-warning text-dark",
                            "atencao"),
    "fora_tolerancia" = shiny::span(class = "badge bg-danger",
                                    "fora tolerancia"),
    shiny::span(class = "badge bg-secondary", classificacao)
  )
}

# ---------------------------------------------------------------------
# UI publica
# ---------------------------------------------------------------------

#' @noRd
mod_preparo_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "cultivar-preparo-wrap",
    shiny::uiOutput(ns("wizard"))
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: tela de rascunhos
# ---------------------------------------------------------------------

#' @noRd
.prep_render_rascunhos <- function(ns, rascunhos_df) {
  cabecalho <- shiny::div(
    style = "margin-bottom: 16px;",
    shiny::h3("Preparos"),
    shiny::actionButton(
      ns("btn_iniciar_novo"),
      label = "Iniciar novo preparo",
      class = "btn-primary"
    )
  )
  
  if (is.null(rascunhos_df) || nrow(rascunhos_df) == 0L) {
    return(shiny::tagList(
      cabecalho,
      shiny::div(
        style = "margin-top: 24px; padding: 40px; text-align: center; color: #888;",
        shiny::tags$p(
          shiny::tags$strong("Sem preparos em andamento."),
          shiny::br(),
          "Inicie um novo preparo para comecar."
        )
      )
    ))
  }
  
  linhas <- lapply(seq_len(nrow(rascunhos_df)), function(i) {
    r <- rascunhos_df[i, , drop = FALSE]
    progresso_txt <- sprintf("%d de %d componentes pesados",
                             r$n_pesados[1], r$n_total[1])
    pct <- if (r$n_total[1] > 0L) {
      round(r$n_pesados[1] / r$n_total[1] * 100)
    } else 0L
    
    shiny::div(
      class = "cultivar-card",
      style = "padding: 12px 16px; margin-bottom: 8px;",
      shiny::fluidRow(
        shiny::column(
          width = 9L,
          shiny::div(
            shiny::tags$strong(r$lote_interno[1]),
            " ",
            shiny::span(
              class = if (r$status[1] == "rascunho") "badge bg-secondary"
              else "badge bg-info text-dark",
              r$status[1]
            )
          ),
          shiny::div(
            style = "color: #555; font-size: 13px; margin-top: 4px;",
            r$meio_codigo[1], " - ", r$meio_nome[1],
            " | Volume: ", r$volume_final_ml[1], " mL"
          ),
          shiny::div(
            style = "color: #888; font-size: 12px; margin-top: 4px;",
            progresso_txt, " (", pct, "%)"
          )
        ),
        shiny::column(
          width = 3L,
          style = "text-align: right; padding-top: 8px;",
          shiny::actionButton(
            ns(paste0("btn_retomar_", r$id[1])),
            label = "Retomar",
            class = "btn-outline-primary btn-sm"
          )
        )
      )
    )
  })
  
  shiny::tagList(
    cabecalho,
    shiny::h5("Em andamento", style = "margin-top: 16px;"),
    do.call(shiny::tagList, linhas)
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: iniciar novo preparo
# ---------------------------------------------------------------------

#' @noRd
.prep_render_iniciar <- function(ns, meios_disponiveis) {
  choices <- if (is.null(meios_disponiveis) ||
                 nrow(meios_disponiveis) == 0L) {
    character(0)
  } else {
    ids <- as.character(meios_disponiveis$id)
    names(ids) <- paste0(meios_disponiveis$codigo_curto, " - ",
                         meios_disponiveis$nome)
    ids
  }
  
  shiny::div(
    class = "cultivar-card",
    style = "padding: 24px; max-width: 640px;",
    shiny::h3("Iniciar novo preparo"),
    shiny::div(
      style = "margin-top: 16px;",
      shiny::selectInput(
        ns("iniciar_meio"),
        label = "Meio",
        choices = choices,
        selected = character(0)
      )
    ),
    shiny::div(
      shiny::numericInput(
        ns("iniciar_volume"),
        label = "Volume final (mL)",
        value = 1000, min = 1, step = 50
      ),
      shiny::uiOutput(ns("err_iniciar_volume"))
    ),
    shiny::div(
      style = "margin-top: 24px;",
      shiny::actionButton(ns("btn_confirmar_iniciar"),
                          "Iniciar preparo",
                          class = "btn-primary"),
      " ",
      shiny::actionButton(ns("btn_voltar_rascunhos"),
                          "Cancelar")
    )
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: tela de pesagem
# ---------------------------------------------------------------------

#' @noRd
.prep_render_pesar <- function(ns, preparo, componentes) {
  # Cabecalho
  header <- shiny::div(
    style = "margin-bottom: 16px;",
    shiny::h3("Pesagem"),
    shiny::div(
      style = "color: #555;",
      shiny::tags$strong(preparo$lote_interno[1]), " | ",
      preparo$meio_codigo[1], " - ", preparo$meio_nome[1], " | ",
      "Volume: ", preparo$volume_final_ml[1], " mL"
    )
  )
  
  # Progresso
  n_pesados <- sum(!is.na(componentes$massa_pesada_mg))
  n_total <- nrow(componentes)
  pct <- if (n_total > 0L) round(n_pesados / n_total * 100) else 0L
  progresso <- shiny::div(
    style = "margin-bottom: 20px;",
    shiny::div(
      style = "display: flex; justify-content: space-between; margin-bottom: 4px;",
      shiny::span(sprintf("%d de %d componentes pesados",
                          n_pesados, n_total)),
      shiny::span(paste0(pct, "%"))
    ),
    shiny::div(
      class = "progress",
      style = "height: 10px;",
      shiny::div(
        class = "progress-bar",
        role = "progressbar",
        style = sprintf("width: %d%%; background-color: #6c5ce7;", pct)
      )
    )
  )
  
  # Cards de componentes
  linhas_comp <- lapply(seq_len(nrow(componentes)), function(i) {
    c_ <- componentes[i, , drop = FALSE]
    cid <- c_$componente_id[1]
    ja_pesado <- !is.na(c_$massa_pesada_mg[1])
    classificacao <- if (ja_pesado) {
      .classificar_pesagem(c_$massa_teorica_mg[1], c_$massa_pesada_mg[1])
    } else NA_character_
    
    input_id <- paste0("pesar_massa_", cid)
    btn_id <- paste0("pesar_btn_", cid)
    
    borda_cor <- if (ja_pesado) {
      switch(classificacao,
             "ok" = "#198754",
             "atencao" = "#ffc107",
             "fora_tolerancia" = "#dc3545",
             "#ddd")
    } else "#ddd"
    
    shiny::div(
      class = "cultivar-card",
      style = sprintf(
        "padding: 12px 16px; margin-bottom: 8px; border-left: 4px solid %s;",
        borda_cor),
      shiny::fluidRow(
        shiny::column(
          width = 5L,
          shiny::div(
            shiny::tags$strong(c_$nome[1]),
            if (!is.na(c_$formula[1]) && c_$formula[1] != c_$nome[1]) {
              shiny::span(style = "color: #888; margin-left: 6px;",
                          "(", c_$formula[1], ")")
            }
          ),
          shiny::div(
            style = "color: #666; font-size: 13px; margin-top: 2px;",
            "Alvo: ", ui_fmt_mg_l(c_$massa_teorica_mg[1]), " mg"
          )
        ),
        shiny::column(
          width = 4L,
          shiny::numericInput(
            ns(input_id),
            label = NULL,
            value = if (ja_pesado) c_$massa_pesada_mg[1] else NA_real_,
            min = 0.001, step = 0.001
          )
        ),
        shiny::column(
          width = 3L,
          style = "text-align: right; padding-top: 8px;",
          if (ja_pesado) {
            shiny::div(
              .badge_pesagem(classificacao),
              shiny::br(),
              shiny::span(
                style = "font-size: 11px; color: #888;",
                "desvio: ",
                ui_fmt_br(
                  abs(c_$massa_pesada_mg[1] - c_$massa_teorica_mg[1]) /
                    c_$massa_teorica_mg[1] * 100, 1L),
                "%"
              )
            )
          } else {
            shiny::actionButton(
              ns(btn_id),
              label = "Registrar",
              class = "btn-outline-primary btn-sm"
            )
          }
        )
      )
    )
  })
  
  # Lembrete de pH alvo
  ph_ref <- if (!is.na(preparo$ph_alvo[1])) {
    shiny::div(
      style = "margin-top: 16px; padding: 12px; background: #f0f0ff; border-radius: 4px; font-size: 13px;",
      shiny::tags$strong("Lembrete: "),
      "pH alvo deste meio e ",
      ui_fmt_br(preparo$ph_alvo[1], 2L),
      ". Voce vai ajustar isso no proximo passo."
    )
  } else NULL
  
  # Acoes - botao avancar so aparece funcional quando todos pesados
  todos_pesados <- n_pesados == n_total && n_total > 0L
  botao_avancar <- if (todos_pesados) {
    shiny::actionButton(
      ns("btn_ir_finalizar"),
      "Avancar para pH e observacoes",
      class = "btn-primary"
    )
  } else {
    # Fallback HTML puro: nativamente desabilitado
    shiny::tags$button(
      type = "button",
      class = "btn btn-secondary",
      disabled = "disabled",
      style = "opacity: 0.6; cursor: not-allowed;",
      "Pese todos os componentes para avancar"
    )
  }
  
  acoes <- shiny::div(
    style = "margin-top: 24px;",
    botao_avancar, " ",
    shiny::actionButton(ns("btn_voltar_rascunhos"), "Salvar e sair"),
    " ",
    shiny::actionButton(ns("btn_descartar"), "Descartar preparo",
                        class = "btn-outline-danger")
  )
  
  shiny::tagList(header, progresso,
                 do.call(shiny::tagList, linhas_comp),
                 ph_ref, acoes)
}

# ---------------------------------------------------------------------
# Sub-renderizador: finalizar (pH e observacoes)
# ---------------------------------------------------------------------

#' @noRd
.prep_render_finalizar <- function(ns, preparo) {
  header <- shiny::div(
    style = "margin-bottom: 16px;",
    shiny::h3("pH e observacoes"),
    shiny::div(
      style = "color: #555;",
      shiny::tags$strong(preparo$lote_interno[1]), " | ",
      preparo$meio_codigo[1], " - ", preparo$meio_nome[1]
    )
  )
  
  ph_info <- if (!is.na(preparo$ph_alvo[1])) {
    shiny::div(
      style = "color: #666; font-size: 13px; margin-top: 4px;",
      "pH alvo: ", ui_fmt_br(preparo$ph_alvo[1], 2L)
    )
  } else {
    shiny::div(
      style = "color: #888; font-size: 12px; font-style: italic; margin-top: 4px;",
      "Este meio nao tem pH alvo cadastrado."
    )
  }
  
  shiny::div(
    class = "cultivar-card",
    style = "padding: 24px; max-width: 720px;",
    header,
    shiny::div(
      style = "margin-top: 16px;",
      shiny::numericInput(
        ns("final_ph"),
        label = "pH medido (0-14)",
        value = preparo$ph_medido[1],
        min = 0, max = 14, step = 0.01
      ),
      ph_info,
      shiny::uiOutput(ns("err_final_ph"))
    ),
    shiny::div(
      style = "margin-top: 16px;",
      shiny::textAreaInput(
        ns("final_obs"),
        label = "Observacoes",
        value = if (is.na(preparo$observacoes[1])) "" else preparo$observacoes[1],
        rows = 3,
        placeholder = "Ajustes de pH, incidentes, notas de rastreabilidade..."
      )
    ),
    shiny::div(
      style = "margin-top: 24px;",
      shiny::actionButton(ns("btn_concluir"),
                          "Concluir preparo",
                          class = "btn-primary"),
      " ",
      shiny::actionButton(ns("btn_voltar_pesar"),
                          "Voltar para pesagens"),
      " ",
      shiny::actionButton(ns("btn_descartar"), "Descartar",
                          class = "btn-outline-danger")
    )
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: resumo final
# ---------------------------------------------------------------------

#' @noRd
.prep_render_resumo <- function(ns, resumo) {
  if (is.null(resumo) || !is.list(resumo)) {
    return(shiny::div(
      class = "alert alert-warning",
      "Erro ao carregar resumo do preparo."
    ))
  }
  
  n_fora <- resumo$n_fora_tolerancia %||% 0L
  cor_status <- if (n_fora > 0L) "#dc3545" else "#198754"
  msg_status <- if (n_fora > 0L) {
    sprintf("Preparo concluido com %d componente(s) fora de tolerancia.",
            n_fora)
  } else {
    "Preparo concluido dentro da tolerancia."
  }
  
  shiny::div(
    class = "cultivar-card",
    style = sprintf(
      "padding: 24px; max-width: 640px; border-left: 4px solid %s;",
      cor_status),
    shiny::h3("Preparo concluido"),
    shiny::div(
      style = "margin-top: 16px;",
      shiny::tags$strong("Lote interno:"),
      shiny::br(),
      shiny::span(style = "font-size: 20px; font-family: monospace;",
                  resumo$lote_interno %||% "-")
    ),
    shiny::div(
      style = sprintf("margin-top: 16px; color: %s;", cor_status),
      msg_status
    ),
    shiny::div(
      style = "margin-top: 8px; color: #666;",
      sprintf("Total de componentes registrados: %d",
              resumo$n_componentes %||% 0L)
    ),
    shiny::div(
      style = "margin-top: 24px;",
      shiny::actionButton(ns("btn_iniciar_novo"),
                          "Iniciar outro preparo",
                          class = "btn-primary"),
      " ",
      shiny::actionButton(ns("btn_voltar_rascunhos"),
                          "Ver preparos em andamento")
    )
  )
}

# =====================================================================
# SERVER
# =====================================================================

#' @noRd
mod_preparo_server <- function(id, con, sessao_auth) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------------
    # Estado
    # -----------------------------------------------------------------
    
    state <- shiny::reactiveVal("rascunhos")
    preparo_id_atual <- shiny::reactiveVal(NULL)
    resumo_ultimo <- shiny::reactiveVal(NULL)
    reload_trigger <- shiny::reactiveVal(0L)
    msg_wizard <- shiny::reactiveVal(NULL)
    
    # Environment do server para guardar observers dinamicos
    server_env <- environment()
    server_env$observers_pesagem <- list()
    server_env$observers_retomar <- list()
    
    # Helper: transicao de estado com limpezas apropriadas
    .ir_para <- function(novo_state) {
      if (novo_state %in% c("rascunhos", "iniciar", "resumo")) {
        preparo_id_atual(NULL)
      }
      msg_wizard(NULL)
      state(novo_state)
    }
    
    # Helper: id do operador logado (NA se sem sessao)
    .op_id <- function() {
      s <- sessao_auth()
      if (is.null(s)) return(NA_integer_)
      as.integer(s$id[1])
    }
    
    # -----------------------------------------------------------------
    # Reactives derivados
    # -----------------------------------------------------------------
    
    rascunhos_lista <- shiny::reactive({
      reload_trigger()
      op <- sessao_auth()
      if (is.null(op)) return(NULL)
      tryCatch(
        listar_rascunhos_operador(con, as.integer(op$id[1])),
        error = function(e) {
          msg_wizard(list(tipo = "danger",
                          texto = paste("Erro ao listar rascunhos:",
                                        conditionMessage(e))))
          NULL
        }
      )
    })
    
    meios_disponiveis <- shiny::reactive({
      reload_trigger()
      df <- tryCatch(
        buscar_meios(con, termo = "", incluir_arquivados = FALSE),
        error = function(e) NULL
      )
      if (is.null(df) || nrow(df) == 0L) return(df)
      df[df$bloqueado_preparo == 0L, , drop = FALSE]
    })
    
    preparo_detalhe <- shiny::reactive({
      pid <- preparo_id_atual()
      if (is.null(pid)) return(NULL)
      reload_trigger()
      tryCatch(
        carregar_preparo(con, as.integer(pid)),
        error = function(e) {
          msg_wizard(list(
            tipo = "danger",
            texto = paste("Preparo indisponivel:", conditionMessage(e))
          ))
          preparo_id_atual(NULL)
          state("rascunhos")
          NULL
        }
      )
    })
    
    componente_ids_atuais <- shiny::reactive({
      det <- preparo_detalhe()
      if (is.null(det) || is.null(det$componentes) ||
          nrow(det$componentes) == 0L) {
        return(integer(0))
      }
      as.integer(det$componentes$componente_id)
    })
    
    # -----------------------------------------------------------------
    # Acao: registrar pesagem (chamada por observer dinamico)
    # -----------------------------------------------------------------
    
    .registrar_pesagem <- function(cid) {
      pid <- preparo_id_atual()
      op_id <- .op_id()
      if (is.null(pid) || is.na(op_id)) return(invisible(NULL))
      
      input_id <- paste0("pesar_massa_", cid)
      valor <- input[[input_id]]
      
      if (is.null(valor) || is.na(valor)) {
        msg_wizard(list(
          tipo = "warning",
          texto = "Digite a massa antes de registrar."
        ))
        return(invisible(NULL))
      }
      
      tryCatch({
        salvar_pesagem(con,
                       preparo_id = as.integer(pid),
                       componente_id = as.integer(cid),
                       massa_real_mg = as.numeric(valor),
                       operador_id = op_id)
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        msg_wizard(list(
          tipo = "danger",
          texto = paste("Erro ao registrar:", conditionMessage(e))
        ))
      })
    }
    
    # -----------------------------------------------------------------
    # Observers dinamicos: botoes "Registrar" de cada componente
    # -----------------------------------------------------------------
    
    shiny::observe({
      ids <- componente_ids_atuais()
      
      # Destruir observers antigos
      old <- server_env$observers_pesagem
      for (nm in names(old)) {
        try(old[[nm]]$destroy(), silent = TRUE)
      }
      server_env$observers_pesagem <- list()
      
      if (length(ids) == 0L) return()
      
      # Criar novos
      novos <- list()
      for (cid in ids) {
        local({
          cid_local <- cid
          obs_btn <- shiny::observeEvent(
            input[[paste0("pesar_btn_", cid_local)]],
            {
              .registrar_pesagem(cid_local)
            },
            ignoreInit = TRUE,
            ignoreNULL = TRUE
          )
          novos[[as.character(cid_local)]] <<- obs_btn
        })
      }
      server_env$observers_pesagem <- novos
    })
    
    # -----------------------------------------------------------------
    # Observers dinamicos: botoes "Retomar" da lista de rascunhos
    # -----------------------------------------------------------------
    
    shiny::observe({
      rasc <- rascunhos_lista()
      
      # Destruir antigos
      old <- server_env$observers_retomar
      for (nm in names(old)) {
        try(old[[nm]]$destroy(), silent = TRUE)
      }
      server_env$observers_retomar <- list()
      
      if (is.null(rasc) || nrow(rasc) == 0L) return()
      
      novos <- list()
      for (i in seq_len(nrow(rasc))) {
        local({
          pid_local <- as.integer(rasc$id[i])
          obs <- shiny::observeEvent(
            input[[paste0("btn_retomar_", pid_local)]],
            {
              info <- tryCatch(
                DBI::dbGetQuery(
                  con,
                  "SELECT status FROM preparos WHERE id = ?;",
                  params = list(pid_local)
                ),
                error = function(e) NULL
              )
              if (is.null(info) || nrow(info) == 0L) {
                msg_wizard(list(tipo = "danger",
                                texto = "Preparo nao encontrado."))
                reload_trigger(reload_trigger() + 1L)
                return()
              }
              if (!info$status[1] %in% c("rascunho", "em_preparo")) {
                msg_wizard(list(
                  tipo = "warning",
                  texto = sprintf("Preparo ja esta '%s'. Recarregando...",
                                  info$status[1])
                ))
                reload_trigger(reload_trigger() + 1L)
                return()
              }
              preparo_id_atual(pid_local)
              msg_wizard(NULL)
              state("pesar")
            },
            ignoreInit = TRUE,
            ignoreNULL = TRUE
          )
          novos[[as.character(pid_local)]] <<- obs
        })
      }
      server_env$observers_retomar <- novos
    })
    
    # -----------------------------------------------------------------
    # Observers estaticos: navegacao
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_iniciar_novo, {
      .ir_para("iniciar")
    })
    
    shiny::observeEvent(input$btn_voltar_rascunhos, {
      .ir_para("rascunhos")
    })
    
    shiny::observeEvent(input$btn_voltar_pesar, {
      msg_wizard(NULL)
      state("pesar")
    })
    
    shiny::observeEvent(input$btn_ir_finalizar, {
      msg_wizard(NULL)
      state("finalizar")
    })
    
    # -----------------------------------------------------------------
    # Iniciar novo preparo
    # -----------------------------------------------------------------
    
    output$err_iniciar_volume <- shiny::renderUI({
      v <- input$iniciar_volume
      if (is.null(v) || is.na(v)) return(NULL)
      if (v <= 0) {
        return(shiny::div(class = "text-danger small",
                          "Volume deve ser maior que zero."))
      }
      if (v > 1e6) {
        return(shiny::div(class = "text-danger small",
                          "Volume excessivamente alto."))
      }
      NULL
    })
    
    shiny::observeEvent(input$btn_confirmar_iniciar, {
      op_id <- .op_id()
      if (is.na(op_id)) {
        msg_wizard(list(tipo = "danger",
                        texto = "Sessao invalida. Faca login novamente."))
        return()
      }
      
      meio_sel <- input$iniciar_meio
      if (is.null(meio_sel) || !nzchar(meio_sel)) {
        msg_wizard(list(tipo = "warning",
                        texto = "Selecione um meio."))
        return()
      }
      meio_id <- suppressWarnings(as.integer(meio_sel))
      if (is.na(meio_id)) {
        msg_wizard(list(tipo = "danger",
                        texto = "ID de meio invalido."))
        return()
      }
      
      volume <- input$iniciar_volume
      if (is.null(volume) || is.na(volume) || volume <= 0) {
        msg_wizard(list(tipo = "warning",
                        texto = "Informe um volume valido."))
        return()
      }
      
      tryCatch({
        novo_pid <- iniciar_preparo(con,
                                    meio_id = meio_id,
                                    volume_ml = as.numeric(volume),
                                    operador_id = op_id)
        preparo_id_atual(as.integer(novo_pid))
        reload_trigger(reload_trigger() + 1L)
        msg_wizard(NULL)
        state("pesar")
      }, error = function(e) {
        msg_wizard(list(
          tipo = "danger",
          texto = paste("Erro ao iniciar preparo:", conditionMessage(e))
        ))
      })
    })
    
    # -----------------------------------------------------------------
    # Finalizar (pH + observacoes + concluir)
    # -----------------------------------------------------------------
    
    output$err_final_ph <- shiny::renderUI({
      ph <- input$final_ph
      if (is.null(ph) || is.na(ph)) return(NULL)
      if (ph < 0 || ph > 14) {
        return(shiny::div(class = "text-danger small",
                          "pH deve estar entre 0 e 14."))
      }
      NULL
    })
    
    shiny::observeEvent(input$btn_concluir, {
      pid <- preparo_id_atual()
      op_id <- .op_id()
      if (is.null(pid) || is.na(op_id)) {
        msg_wizard(list(tipo = "danger",
                        texto = "Sessao ou preparo invalido."))
        return()
      }
      
      ph <- input$final_ph
      if (!is.null(ph) && !is.na(ph)) {
        if (ph < 0 || ph > 14) {
          msg_wizard(list(tipo = "warning",
                          texto = "pH invalido. Corrija antes de concluir."))
          return()
        }
      }
      obs <- input$final_obs
      if (is.null(obs) || !nzchar(trimws(obs))) obs <- NA_character_
      
      tryCatch({
        salvar_ph_observacoes(
          con,
          preparo_id = as.integer(pid),
          ph_medido = if (is.null(ph) || is.na(ph)) NA_real_ else as.numeric(ph),
          observacoes = if (is.na(obs)) NA_character_ else obs,
          operador_id = op_id
        )
        res <- concluir_preparo(con,
                                preparo_id = as.integer(pid),
                                operador_id = op_id)
        resumo_ultimo(res)
        reload_trigger(reload_trigger() + 1L)
        preparo_id_atual(NULL)
        msg_wizard(NULL)
        state("resumo")
      }, error = function(e) {
        msg_wizard(list(
          tipo = "danger",
          texto = paste("Erro ao concluir:", conditionMessage(e))
        ))
      })
    })
    
    # -----------------------------------------------------------------
    # Descartar preparo
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_descartar, {
      if (is.null(preparo_id_atual())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Descartar preparo",
        shiny::div(
          "Esta acao descarta o preparo em andamento. ",
          "O registro ficara marcado como 'descartado' com o motivo abaixo."
        ),
        shiny::textAreaInput(
          ns("descarte_motivo"),
          label = "Motivo do descarte (obrigatorio)",
          rows = 3,
          placeholder = "Ex: erro de pesagem em componente critico"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_descartar_confirmar"),
                              "Descartar",
                              class = "btn-danger")
        ),
        easyClose = FALSE
      ))
    })
    
    shiny::observeEvent(input$btn_descartar_confirmar, {
      pid <- preparo_id_atual()
      op_id <- .op_id()
      motivo <- input$descarte_motivo
      
      if (is.null(pid) || is.na(op_id)) {
        shiny::removeModal()
        return()
      }
      if (is.null(motivo) || !nzchar(trimws(motivo))) {
        shiny::removeModal()
        msg_wizard(list(
          tipo = "warning",
          texto = "Descarte cancelado: motivo obrigatorio. Tente novamente."
        ))
        return()
      }
      
      shiny::removeModal()
      tryCatch({
        descartar_preparo(con,
                          preparo_id = as.integer(pid),
                          motivo = trimws(motivo),
                          solicitante_id = op_id)
        reload_trigger(reload_trigger() + 1L)
        preparo_id_atual(NULL)
        msg_wizard(list(tipo = "success",
                        texto = "Preparo descartado."))
        state("rascunhos")
      }, error = function(e) {
        msg_wizard(list(
          tipo = "danger",
          texto = paste("Erro ao descartar:", conditionMessage(e))
        ))
      })
    })
    
    # -----------------------------------------------------------------
    # Renderizacao principal do wizard
    # -----------------------------------------------------------------
    
    output$wizard <- shiny::renderUI({
      st <- state()
      msg <- msg_wizard()
      
      alerta <- if (!is.null(msg)) {
        ui_alerta(mensagem = msg$texto, tipo = msg$tipo)
      } else NULL
      
      conteudo <- switch(
        st,
        "rascunhos" = .prep_render_rascunhos(ns, rascunhos_lista()),
        "iniciar" = .prep_render_iniciar(ns, meios_disponiveis()),
        "pesar" = {
          det <- preparo_detalhe()
          if (is.null(det)) {
            shiny::div("Carregando preparo...")
          } else {
            .prep_render_pesar(ns, det$preparo, det$componentes)
          }
        },
        "finalizar" = {
          det <- preparo_detalhe()
          if (is.null(det)) {
            shiny::div("Carregando preparo...")
          } else {
            .prep_render_finalizar(ns, det$preparo)
          }
        },
        "resumo" = .prep_render_resumo(ns, resumo_ultimo()),
        shiny::div("Estado desconhecido: ", st)
      )
      
      shiny::tagList(alerta, conteudo)
    })
    
    # TODO v0.2: JavaScript custom para Enter no input disparar
    # "Registrar" do componente. Requer Shiny.setInputValue no cliente.
    
  })
}