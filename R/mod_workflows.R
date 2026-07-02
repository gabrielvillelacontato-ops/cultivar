#' Modulo Shiny: gestao de workflows e etapas
#'
#' Layout em coluna unica (largura total):
#'   - Tabela de workflows no topo
#'   - Ao selecionar um workflow, expande abaixo:
#'     - Metadata do workflow (nome, referencia, DOI, descricao)
#'     - Lista de etapas com drag-drop (via sortable::rank_list)
#'
#' Permissoes:
#'   - operador: apenas visualiza
#'   - supervisor: cria/edita workflow, adiciona/edita/remove/reordena etapas
#'   - admin: tudo + arquiva/restaura workflow
#'
#' Reordenacao: drag-drop via sortable. Cada card de etapa carrega um
#' label codificado "id|texto" para permitir mapear a nova ordem de volta
#' aos IDs no server.
#'
#' Feedback: showNotification (top-right, transient).
#'
#' @noRd

# ---------------------------------------------------------------------
# Constantes internas
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Label do rank_list: valor visual + nome invisivel (id da etapa)
#
# sortable::rank_list aceita vetor nomeado onde:
#   - o valor eh o texto exibido ao usuario
#   - o nome eh capturado no input$xxx (invisivel na UI)
# ---------------------------------------------------------------------

#' Texto visivel da etapa no rank_list e na lista estatica
#' @noRd
.wf_label_visual <- function(ordem, nome_etapa, meio_codigo, duracao) {
  dur_txt <- if (is.null(duracao) || is.na(duracao) ||
                 !nzchar(as.character(duracao))) {
    ""
  } else {
    paste0(" - ", duracao)
  }
  sprintf("%d. %s (%s)%s", ordem, nome_etapa, meio_codigo, dur_txt)
}

# ---------------------------------------------------------------------
# Validadores de formulario
# ---------------------------------------------------------------------

#' @noRd
.err_wf_nome <- function(nome) {
  if (is.null(nome) || is.na(nome) ||
      !nzchar(trimws(as.character(nome)))) {
    return("Nome obrigatorio.")
  }
  if (nchar(trimws(as.character(nome))) > 200L) {
    return("Ate 200 caracteres.")
  }
  NULL
}

#' @noRd
.err_etapa_nome <- function(nome) {
  if (is.null(nome) || is.na(nome) ||
      !nzchar(trimws(as.character(nome)))) {
    return("Nome da etapa obrigatorio.")
  }
  NULL
}

#' @noRd
.err_etapa_meio <- function(meio_id) {
  if (is.null(meio_id) || is.na(meio_id) ||
      !nzchar(as.character(meio_id))) {
    return("Selecione um meio.")
  }
  NULL
}

# ---------------------------------------------------------------------
# UI publica
# ---------------------------------------------------------------------

#' @noRd
mod_workflows_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::div(
    class = "cultivar-workflows-wrap",
    shiny::uiOutput(ns("main"))
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: lista de workflows (topo)
# ---------------------------------------------------------------------

#' @noRd
.wf_render_lista <- function(ns, pode_criar) {
  cabecalho <- shiny::div(
    class = "cultivar-card",
    style = "padding: 16px; margin-bottom: 12px;",
    shiny::fluidRow(
      shiny::column(
        width = 6L,
        shiny::textInput(
          ns("termo_busca"),
          label = NULL,
          placeholder = "Buscar por nome ou referencia..."
        )
      ),
      shiny::column(
        width = 3L,
        shiny::checkboxInput(
          ns("mostrar_arquivados"),
          label = "Mostrar arquivados",
          value = FALSE
        )
      ),
      shiny::column(
        width = 3L,
        style = "text-align: right;",
        if (isTRUE(pode_criar)) {
          shiny::actionButton(
            ns("btn_novo_workflow"),
            label = "+ Novo workflow",
            class = "btn-primary",
            style = "margin-top: 4px;"
          )
        } else NULL
      )
    )
  )
  
  tabela <- shiny::div(
    class = "cultivar-card",
    style = "padding: 8px;",
    DT::dataTableOutput(ns("tabela"))
  )
  
  shiny::tagList(cabecalho, tabela)
}

# ---------------------------------------------------------------------
# Sub-renderizador: detalhe do workflow selecionado
# ---------------------------------------------------------------------

#' @noRd
.wf_render_detalhe <- function(ns, workflow, etapas, pode_editar,
                               pode_arquivar) {
  arquivado <- !is.na(workflow$deleted_at[1])
  
  # Botoes contextuais
  botoes <- shiny::tagList()
  if (pode_editar && !arquivado) {
    botoes <- shiny::tagList(
      botoes,
      shiny::actionButton(ns("btn_editar_workflow"), "Editar",
                          class = "btn-primary btn-sm"),
      " "
    )
  }
  if (pode_arquivar) {
    if (arquivado) {
      botoes <- shiny::tagList(
        botoes,
        shiny::actionButton(ns("btn_restaurar_workflow"), "Restaurar",
                            class = "btn-success btn-sm")
      )
    } else {
      botoes <- shiny::tagList(
        botoes,
        shiny::actionButton(ns("btn_arquivar_workflow"), "Arquivar",
                            class = "btn-outline-danger btn-sm")
      )
    }
  }
  
  header <- shiny::div(
    class = "cultivar-card",
    style = "padding: 20px; margin-bottom: 12px;",
    shiny::fluidRow(
      shiny::column(
        width = 8L,
        shiny::actionLink(ns("btn_voltar_lista"),
                          label = shiny::HTML("&laquo; Voltar")),
        shiny::h3(workflow$nome[1], style = "margin-top: 8px;"),
        if (arquivado) shiny::div(
          class = "alert alert-warning",
          style = "padding: 6px 10px; font-size: 12px; display: inline-block;",
          "Este workflow esta arquivado. Restaure para editar."
        ) else NULL
      ),
      shiny::column(
        width = 4L,
        style = "text-align: right; padding-top: 30px;",
        botoes
      )
    ),
    shiny::tags$dl(
      style = "margin-top: 12px;",
      if (!is.na(workflow$referencia[1])) shiny::tagList(
        shiny::tags$dt("Referencia"),
        shiny::tags$dd(workflow$referencia[1])
      ),
      if (!is.na(workflow$doi[1])) shiny::tagList(
        shiny::tags$dt("DOI"),
        shiny::tags$dd(workflow$doi[1])
      ),
      if (!is.na(workflow$descricao[1])) shiny::tagList(
        shiny::tags$dt("Descricao"),
        shiny::tags$dd(workflow$descricao[1])
      )
    )
  )
  
  # Bloco de etapas
  etapas_ui <- .wf_render_etapas(ns, etapas, pode_editar, arquivado)
  
  shiny::tagList(header, etapas_ui)
}

# ---------------------------------------------------------------------
# Sub-renderizador: lista de etapas (drag-drop via sortable)
# ---------------------------------------------------------------------

#' @noRd
#' @noRd
.wf_render_etapas <- function(ns, etapas, pode_editar, arquivado) {
  cabecalho <- shiny::fluidRow(
    shiny::column(
      width = 8L,
      shiny::h4("Etapas", style = "margin: 0;")
    ),
    shiny::column(
      width = 4L,
      style = "text-align: right;",
      if (pode_editar && !arquivado) {
        shiny::actionButton(
          ns("btn_adicionar_etapa"),
          label = "+ Adicionar etapa",
          class = "btn-primary btn-sm"
        )
      } else NULL
    )
  )
  
  if (is.null(etapas) || nrow(etapas) == 0L) {
    conteudo <- shiny::div(
      style = "text-align: center; padding: 40px; color: #888;",
      shiny::tags$p(
        shiny::tags$strong("Sem etapas cadastradas."),
        shiny::br(),
        if (pode_editar && !arquivado) {
          "Adicione a primeira etapa usando o botao acima."
        } else {
          "Nenhuma etapa foi cadastrada neste workflow."
        }
      )
    )
    return(shiny::div(
      class = "cultivar-card",
      style = "padding: 20px;",
      cabecalho,
      shiny::div(style = "margin-top: 16px;", conteudo)
    ))
  }
  
  n_total <- nrow(etapas)
  
  # Renderiza cada etapa como card com botoes de reorder + editar/remover
  cards <- lapply(seq_len(n_total), function(i) {
    e <- etapas[i, , drop = FALSE]
    dur <- if ("duracao" %in% names(etapas)) e$duracao[1] else NA
    texto_visual <- .wf_label_visual(
      ordem       = e$ordem[1],
      nome_etapa  = e$nome_etapa[1],
      meio_codigo = e$meio_codigo[1],
      duracao     = dur
    )
    
    eh_primeira <- (i == 1L)
    eh_ultima   <- (i == n_total)
    
    # Botoes de reorder (setas up/down)
    btn_up <- if (pode_editar && !arquivado) {
      if (eh_primeira) {
        shiny::tags$button(
          type = "button",
          class = "btn btn-outline-secondary btn-sm",
          disabled = "disabled",
          style = "opacity: 0.4; cursor: not-allowed;",
          shiny::HTML("&#9650;")
        )
      } else {
        shiny::actionButton(
          ns(paste0("btn_etapa_up_", e$id[1])),
          label = shiny::HTML("&#9650;"),
          class = "btn-outline-primary btn-sm",
          title = "Mover para cima"
        )
      }
    } else NULL
    
    btn_down <- if (pode_editar && !arquivado) {
      if (eh_ultima) {
        shiny::tags$button(
          type = "button",
          class = "btn btn-outline-secondary btn-sm",
          disabled = "disabled",
          style = "opacity: 0.4; cursor: not-allowed;",
          shiny::HTML("&#9660;")
        )
      } else {
        shiny::actionButton(
          ns(paste0("btn_etapa_down_", e$id[1])),
          label = shiny::HTML("&#9660;"),
          class = "btn-outline-primary btn-sm",
          title = "Mover para baixo"
        )
      }
    } else NULL
    
    # Botoes Editar/Remover ao lado dos de reorder
    btn_editar <- if (pode_editar && !arquivado) {
      shiny::actionButton(
        ns(paste0("btn_editar_etapa_", e$id[1])),
        label = "Editar",
        class = "btn-outline-primary btn-sm"
      )
    } else NULL
    
    btn_remover <- if (pode_editar && !arquivado) {
      shiny::actionButton(
        ns(paste0("btn_remover_etapa_", e$id[1])),
        label = "Remover",
        class = "btn-outline-danger btn-sm"
      )
    } else NULL
    
    shiny::div(
      class = "cv-etapa-card",
      style = "padding: 12px 14px; border: 1px solid #ddd;
               border-radius: 4px; margin-bottom: 6px;
               background: #fafafa;
               display: flex; align-items: center; gap: 8px;",
      # Setas
      if (pode_editar && !arquivado) {
        shiny::div(
          style = "display: flex; gap: 4px;",
          btn_up, btn_down
        )
      } else NULL,
      # Texto da etapa
      shiny::div(
        style = "flex: 1;",
        texto_visual
      ),
      # Botoes de acao
      if (pode_editar && !arquivado) {
        shiny::div(
          style = "display: flex; gap: 4px;",
          btn_editar, btn_remover
        )
      } else NULL
    )
  })
  
  shiny::div(
    class = "cultivar-card",
    style = "padding: 20px;",
    cabecalho,
    shiny::div(
      style = "margin-top: 16px;",
      do.call(shiny::tagList, cards)
    )
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: formulario de workflow (criar/editar) - em modal
# ---------------------------------------------------------------------

#' @noRd
.wf_form_workflow <- function(ns, workflow = NULL) {
  eh_edicao <- !is.null(workflow)
  sufixo <- if (eh_edicao) "_wfedit" else "_wfnovo"
  
  val_nome <- if (eh_edicao) workflow$nome[1] else ""
  val_ref <- if (eh_edicao && !is.na(workflow$referencia[1])) {
    workflow$referencia[1]
  } else ""
  val_doi <- if (eh_edicao && !is.na(workflow$doi[1])) {
    workflow$doi[1]
  } else ""
  val_desc <- if (eh_edicao && !is.na(workflow$descricao[1])) {
    workflow$descricao[1]
  } else ""
  
  shiny::tagList(
    shiny::textInput(
      ns(paste0("nome_workflow", sufixo)),
      label = "Nome*",
      value = val_nome,
      placeholder = "Ex: Transformacao de algodao via Agrobacterium"
    ),
    shiny::uiOutput(ns(paste0("err_nome_workflow", sufixo))),
    
    shiny::textInput(
      ns(paste0("referencia_workflow", sufixo)),
      label = "Referencia bibliografica",
      value = val_ref,
      placeholder = "Ex: Sunilkumar & Rathore 2001"
    ),
    
    shiny::textInput(
      ns(paste0("doi_workflow", sufixo)),
      label = "DOI",
      value = val_doi,
      placeholder = "Ex: 10.1023/A:1010651218398"
    ),
    
    shiny::textAreaInput(
      ns(paste0("descricao_workflow", sufixo)),
      label = "Descricao",
      value = val_desc,
      rows = 4,
      placeholder = "Descreva brevemente o pipeline..."
    )
  )
}

# ---------------------------------------------------------------------
# Sub-renderizador: formulario de etapa (criar/editar) - em modal
# ---------------------------------------------------------------------

#' @noRd
.wf_form_etapa <- function(ns, meios_choices, etapa = NULL) {
  eh_edicao <- !is.null(etapa)
  sufixo <- if (eh_edicao) "_etedit" else "_etnovo"
  
  val_meio <- if (eh_edicao) as.character(etapa$meio_id[1]) else NULL
  val_nome <- if (eh_edicao) etapa$nome_etapa[1] else ""
  val_dur <- if (eh_edicao && !is.na(etapa$duracao[1])) {
    etapa$duracao[1]
  } else ""
  val_cond <- if (eh_edicao && !is.na(etapa$condicoes[1])) {
    etapa$condicoes[1]
  } else ""
  val_obs <- if (eh_edicao && !is.na(etapa$observacoes[1])) {
    etapa$observacoes[1]
  } else ""
  
  shiny::tagList(
    shiny::selectInput(
      ns(paste0("meio", sufixo)),
      label = "Meio*",
      choices = c("Selecione..." = "", meios_choices),
      selected = val_meio
    ),
    shiny::uiOutput(ns(paste0("err_meio", sufixo))),
    
    shiny::textInput(
      ns(paste0("nome_etapa", sufixo)),
      label = "Nome da etapa*",
      value = val_nome,
      placeholder = "Ex: Co-cultivo com Agrobacterium"
    ),
    shiny::uiOutput(ns(paste0("err_nome_etapa", sufixo))),
    
    shiny::textInput(
      ns(paste0("duracao", sufixo)),
      label = "Duracao",
      value = val_dur,
      placeholder = "Ex: 3 dias, 48h, 2 semanas"
    ),

    shiny::textInput(
      ns(paste0("condicoes", sufixo)),
      label = "Condicoes",
      value = val_cond,
      placeholder = "Ex: 22C escuro, 25C fotoperiodo 16h"
    ),

    shiny::textAreaInput(
      ns(paste0("observacoes", sufixo)),
      label = "Observacoes",
      value = val_obs,
      rows = 3
    )
  )
}
# =====================================================================
# SERVER
# =====================================================================

#' @noRd
mod_workflows_server <- function(id, con, sessao_auth) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------------
    # Estado
    # -----------------------------------------------------------------
    
    state <- shiny::reactiveVal("lista")
    workflow_id_atual <- shiny::reactiveVal(NULL)
    etapa_id_edicao <- shiny::reactiveVal(NULL)
    etapa_id_remocao <- shiny::reactiveVal(NULL)
    reload_trigger <- shiny::reactiveVal(0L)
    
    server_env <- environment()
    server_env$observers_etapa_editar <- list()
    server_env$observers_etapa_remover <- list()
    
    # -----------------------------------------------------------------
    # Permissoes derivadas da sessao
    # -----------------------------------------------------------------
    
    papel_atual <- shiny::reactive({
      s <- sessao_auth()
      if (is.null(s)) NA_character_ else s$papel[1]
    })
    pode_editar <- shiny::reactive(
      isTRUE(papel_atual() %in% c("supervisor", "admin"))
    )
    pode_arquivar <- shiny::reactive(
      isTRUE(papel_atual() == "admin")
    )
    op_atual_id <- shiny::reactive({
      s <- sessao_auth()
      if (is.null(s)) NA_integer_ else as.integer(s$id[1])
    })
    
    # -----------------------------------------------------------------
    # Helper: notificacao
    # -----------------------------------------------------------------
    
    .notify <- function(texto, tipo = "message") {
      shiny::showNotification(texto, type = tipo, duration = 4)
    }
    
    # -----------------------------------------------------------------
    # Reactives de dados
    # -----------------------------------------------------------------
    
    workflows_lista <- shiny::reactive({
      reload_trigger()
      termo <- input$termo_busca %||% ""
      incluir_arq <- isTRUE(input$mostrar_arquivados)
      
      tryCatch(
        buscar_workflows(con,
                         termo = termo,
                         incluir_arquivados = incluir_arq),
        error = function(e) {
          .notify(paste("Erro ao listar workflows:",
                        conditionMessage(e)),
                  tipo = "error")
          NULL
        }
      )
    })
    
    workflow_detalhe <- shiny::reactive({
      wid <- workflow_id_atual()
      if (is.null(wid)) return(NULL)
      reload_trigger()
      tryCatch(
        detalhe_workflow(con, as.integer(wid)),
        error = function(e) {
          .notify(paste("Workflow indisponivel:",
                        conditionMessage(e)),
                  tipo = "error")
          workflow_id_atual(NULL)
          state("lista")
          NULL
        }
      )
    })
    
    meios_choices <- shiny::reactive({
      reload_trigger()
      df <- tryCatch(
        buscar_meios(con, termo = "", incluir_arquivados = FALSE),
        error = function(e) NULL
      )
      if (is.null(df) || nrow(df) == 0L) return(character(0))
      df <- df[df$bloqueado_preparo == 0L, , drop = FALSE]
      if (nrow(df) == 0L) return(character(0))
      ids <- as.character(df$id)
      names(ids) <- paste0(df$codigo_curto, " - ", df$nome)
      ids
    })
    
    etapas_ids_atuais <- shiny::reactive({
      det <- workflow_detalhe()
      if (is.null(det) || nrow(det$etapas) == 0L) return(integer(0))
      as.integer(det$etapas$id)
    })
    
    # -----------------------------------------------------------------
    # Render principal (state machine)
    # -----------------------------------------------------------------
    
    output$main <- shiny::renderUI({
      st <- state()
      if (st == "detalhe") {
        det <- workflow_detalhe()
        if (is.null(det)) {
          return(shiny::div("Carregando..."))
        }
        .wf_render_detalhe(
          ns,
          workflow = det$workflow,
          etapas = det$etapas,
          pode_editar = pode_editar(),
          pode_arquivar = pode_arquivar()
        )
      } else {
        .wf_render_lista(ns, pode_criar = pode_editar())
      }
    })
    
    # -----------------------------------------------------------------
    # Tabela de workflows
    # -----------------------------------------------------------------
    
    output$tabela <- DT::renderDataTable({
      df <- workflows_lista()
      if (is.null(df) || nrow(df) == 0L) {
        return(DT::datatable(
          data.frame(Aviso = "Nenhum workflow encontrado."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      
      df_view <- data.frame(
        Nome = df$nome,
        Referencia = ifelse(is.na(df$referencia), "-", df$referencia),
        `N etapas` = df$n_etapas,
        Status = ifelse(is.na(df$deleted_at), "Ativo", "Arquivado"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        df_view,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          dom = "ft",
          language = list(
            search = "Filtrar:",
            zeroRecords = "Nenhum registro encontrado",
            info = "_TOTAL_ workflow(s)",
            infoEmpty = "0 workflows",
            infoFiltered = "(filtrados de _MAX_)"
          )
        )
      )
    })
    
    # -----------------------------------------------------------------
    # Navegacao lista <-> detalhe
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$tabela_rows_selected, {
      idx <- input$tabela_rows_selected
      df <- workflows_lista()
      if (is.null(idx) || length(idx) == 0L || is.null(df) ||
          nrow(df) == 0L || idx > nrow(df)) {
        return()
      }
      workflow_id_atual(as.integer(df$id[idx]))
      state("detalhe")
    }, ignoreNULL = FALSE)
    
    shiny::observeEvent(input$btn_voltar_lista, {
      workflow_id_atual(NULL)
      state("lista")
    })
    
    # -----------------------------------------------------------------
    # Criar workflow (modal)
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_novo_workflow, {
      if (!isTRUE(pode_editar())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Novo workflow",
        .wf_form_workflow(ns, workflow = NULL),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_salvar_workflow_novo"),
                              "Criar",
                              class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE
      ))
    })
    
    output$err_nome_workflow_wfnovo <- shiny::renderUI({
      e <- .err_wf_nome(input$nome_workflow_wfnovo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    shiny::observeEvent(input$btn_salvar_workflow_novo, {
      if (!isTRUE(pode_editar())) return()
      
      nome <- input$nome_workflow_wfnovo
      erro <- .err_wf_nome(nome)
      if (!is.null(erro)) {
        .notify(paste("Corrija:", erro), tipo = "warning")
        return()
      }
      
      nz <- function(x) {
        if (is.null(x) || is.na(x) ||
            !nzchar(trimws(as.character(x)))) {
          NA_character_
        } else {
          trimws(as.character(x))
        }
      }
      
      tryCatch({
        novo_id <- criar_workflow(
          con,
          nome = trimws(nome),
          referencia = nz(input$referencia_workflow_wfnovo),
          doi = nz(input$doi_workflow_wfnovo),
          descricao = nz(input$descricao_workflow_wfnovo),
          criado_por_id = op_atual_id()
        )
        shiny::removeModal()
        .notify("Workflow criado.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
        workflow_id_atual(as.integer(novo_id))
        state("detalhe")
      }, error = function(e) {
        .notify(paste("Erro ao criar workflow:", conditionMessage(e)),
                tipo = "error")
      })
    })
    
    # -----------------------------------------------------------------
    # Editar workflow (modal)
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_editar_workflow, {
      if (!isTRUE(pode_editar())) return()
      det <- workflow_detalhe()
      if (is.null(det)) return()
      
      shiny::showModal(shiny::modalDialog(
        title = "Editar workflow",
        .wf_form_workflow(ns, workflow = det$workflow),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_salvar_workflow_edicao"),
                              "Salvar",
                              class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE
      ))
    })
    
    output$err_nome_workflow_wfedit <- shiny::renderUI({
      e <- .err_wf_nome(input$nome_workflow_wfedit)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    shiny::observeEvent(input$btn_salvar_workflow_edicao, {
      if (!isTRUE(pode_editar())) return()
      wid <- workflow_id_atual()
      if (is.null(wid)) return()
      
      nome <- input$nome_workflow_wfedit
      erro <- .err_wf_nome(nome)
      if (!is.null(erro)) {
        .notify(paste("Corrija:", erro), tipo = "warning")
        return()
      }
      
      nz <- function(x) {
        if (is.null(x) || is.na(x) ||
            !nzchar(trimws(as.character(x)))) {
          NA_character_
        } else {
          trimws(as.character(x))
        }
      }
      
      campos <- list(
        nome = trimws(nome),
        referencia = nz(input$referencia_workflow_wfedit),
        doi = nz(input$doi_workflow_wfedit),
        descricao = nz(input$descricao_workflow_wfedit)
      )
      
      tryCatch({
        atualizar_workflow(con, wid,
                           campos = campos,
                           solicitante_id = op_atual_id())
        shiny::removeModal()
        .notify("Workflow atualizado.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        .notify(paste("Erro ao atualizar:", conditionMessage(e)),
                tipo = "error")
      })
    })
    
    # -----------------------------------------------------------------
    # Arquivar workflow (com confirmacao)
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_arquivar_workflow, {
      if (!isTRUE(pode_arquivar())) return()
      if (is.null(workflow_id_atual())) return()
      
      shiny::showModal(shiny::modalDialog(
        title = "Confirmar arquivamento",
        shiny::div(
          "Este workflow ficara indisponivel para edicao ate ser ",
          "restaurado. Historico de preparos que o referenciam ",
          "sera preservado."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_arquivar_workflow_confirmar"),
                              "Arquivar",
                              class = "btn-danger")
        )
      ))
    })
    
    shiny::observeEvent(input$btn_arquivar_workflow_confirmar, {
      shiny::removeModal()
      wid <- workflow_id_atual()
      if (is.null(wid)) return()
      
      tryCatch({
        arquivar_workflow(con, wid, solicitante_id = op_atual_id())
        .notify("Workflow arquivado.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        .notify(paste("Erro:", conditionMessage(e)), tipo = "error")
      })
    })
    
    # -----------------------------------------------------------------
    # Restaurar workflow (sem confirmacao)
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_restaurar_workflow, {
      if (!isTRUE(pode_arquivar())) return()
      wid <- workflow_id_atual()
      if (is.null(wid)) return()
      
      tryCatch({
        restaurar_workflow(con, wid, solicitante_id = op_atual_id())
        .notify("Workflow restaurado.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        .notify(paste("Erro:", conditionMessage(e)), tipo = "error")
      })
    })
    
    # -----------------------------------------------------------------
    # Adicionar etapa (modal)
    # -----------------------------------------------------------------
    
    shiny::observeEvent(input$btn_adicionar_etapa, {
      if (!isTRUE(pode_editar())) return()
      wid <- workflow_id_atual()
      if (is.null(wid)) return()
      
      choices <- meios_choices()
      if (length(choices) == 0L) {
        .notify("Nenhum meio disponivel. Cadastre um meio antes.",
                tipo = "warning")
        return()
      }
      
      shiny::showModal(shiny::modalDialog(
        title = "Adicionar etapa",
        .wf_form_etapa(ns, meios_choices = choices, etapa = NULL),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_salvar_etapa_novo"),
                              "Adicionar",
                              class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE
      ))
    })
    
    output$err_meio_etnovo <- shiny::renderUI({
      e <- .err_etapa_meio(input$meio_etnovo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_nome_etapa_etnovo <- shiny::renderUI({
      e <- .err_etapa_nome(input$nome_etapa_etnovo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    shiny::observeEvent(input$btn_salvar_etapa_novo, {
      if (!isTRUE(pode_editar())) return()
      wid <- workflow_id_atual()
      if (is.null(wid)) return()
      
      erros <- c(
        .err_etapa_meio(input$meio_etnovo),
        .err_etapa_nome(input$nome_etapa_etnovo)
      )
      if (length(erros) > 0L) {
        .notify(paste("Corrija:", paste(erros, collapse = " ")),
                tipo = "warning")
        return()
      }
      
      nz <- function(x) {
        if (is.null(x) || is.na(x) ||
            !nzchar(trimws(as.character(x)))) NA_character_
        else trimws(as.character(x))
      }
      
      tryCatch({
        adicionar_etapa(
          con,
          workflow_id = as.integer(wid),
          meio_id = as.integer(input$meio_etnovo),
          nome_etapa = trimws(input$nome_etapa_etnovo),
          duracao = nz(input$duracao_etnovo),
          condicoes = nz(input$condicoes_etnovo),
          observacoes = nz(input$observacoes_etnovo),
          solicitante_id = op_atual_id()
        )
        shiny::removeModal()
        .notify("Etapa adicionada.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        .notify(paste("Erro ao adicionar etapa:", conditionMessage(e)),
                tipo = "error")
      })
    })
    
    # -----------------------------------------------------------------
    # Editar etapa (modal) - id vem do observer dinamico
    # -----------------------------------------------------------------
    
    output$err_meio_etedit <- shiny::renderUI({
      e <- .err_etapa_meio(input$meio_etedit)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_nome_etapa_etedit <- shiny::renderUI({
      e <- .err_etapa_nome(input$nome_etapa_etedit)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    .abrir_modal_editar_etapa <- function(etapa_id) {
      det <- workflow_detalhe()
      if (is.null(det)) return()
      etapa_row <- det$etapas[det$etapas$id == etapa_id, , drop = FALSE]
      if (nrow(etapa_row) == 0L) return()
      
      etapa_id_edicao(as.integer(etapa_id))
      
      shiny::showModal(shiny::modalDialog(
        title = paste0("Editar etapa ", etapa_row$ordem[1]),
        .wf_form_etapa(ns, meios_choices = meios_choices(),
                       etapa = etapa_row),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_salvar_etapa_edicao"),
                              "Salvar",
                              class = "btn-primary")
        ),
        size = "m",
        easyClose = FALSE
      ))
    }
    
    shiny::observeEvent(input$btn_salvar_etapa_edicao, {
      if (!isTRUE(pode_editar())) return()
      eid <- etapa_id_edicao()
      if (is.null(eid)) return()
      
      erros <- c(
        .err_etapa_meio(input$meio_etedit),
        .err_etapa_nome(input$nome_etapa_etedit)
      )
      if (length(erros) > 0L) {
        .notify(paste("Corrija:", paste(erros, collapse = " ")),
                tipo = "warning")
        return()
      }
      
      nz <- function(x) {
        if (is.null(x) || is.na(x) ||
            !nzchar(trimws(as.character(x)))) NA_character_
        else trimws(as.character(x))
      }
      
      campos <- list(
        meio_id = as.integer(input$meio_etedit),
        nome_etapa = trimws(input$nome_etapa_etedit),
        duracao = nz(input$duracao_etedit),
        condicoes = nz(input$condicoes_etedit),
        observacoes = nz(input$observacoes_etedit)
      )
      
      tryCatch({
        atualizar_etapa(con, eid,
                        campos = campos,
                        solicitante_id = op_atual_id())
        shiny::removeModal()
        .notify("Etapa atualizada.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
        etapa_id_edicao(NULL)
      }, error = function(e) {
        .notify(paste("Erro ao atualizar etapa:", conditionMessage(e)),
                tipo = "error")
      })
    })
    
    # -----------------------------------------------------------------
    # Remover etapa (modal com confirmacao)
    # -----------------------------------------------------------------
    
    .abrir_modal_remover_etapa <- function(etapa_id) {
      det <- workflow_detalhe()
      if (is.null(det)) return()
      etapa_row <- det$etapas[det$etapas$id == etapa_id, , drop = FALSE]
      if (nrow(etapa_row) == 0L) return()
      
      etapa_id_remocao(as.integer(etapa_id))
      
      shiny::showModal(shiny::modalDialog(
        title = "Remover etapa",
        shiny::div(
          "Remover a etapa ",
          shiny::tags$strong(sprintf("%d. %s",
                                     etapa_row$ordem[1],
                                     etapa_row$nome_etapa[1])),
          "? As etapas posteriores serao renumeradas automaticamente."
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("btn_remover_etapa_confirmar"),
                              "Remover",
                              class = "btn-danger")
        )
      ))
    }
    
    shiny::observeEvent(input$btn_remover_etapa_confirmar, {
      shiny::removeModal()
      eid <- etapa_id_remocao()
      if (is.null(eid)) return()
      
      tryCatch({
        remover_etapa(con, eid, solicitante_id = op_atual_id())
        .notify("Etapa removida.", tipo = "message")
        reload_trigger(reload_trigger() + 1L)
        etapa_id_remocao(NULL)
      }, error = function(e) {
        .notify(paste("Erro ao remover etapa:", conditionMessage(e)),
                tipo = "error")
      })
    })
    # -----------------------------------------------------------------
    # Sincroniza rank_list quando os dados mudam
    #
    # Como sortable::rank_list mantem estado interno do widget,
    # precisamos usar update_rank_list para refletir mudancas do
    # banco (adicionar/remover/reordenar/editar etapa) no widget
    # sem re-renderizar completamente.
    # -----------------------------------------------------------------
    
    # v0.2: sincronizar rank_list via update_rank_list (ver notas_roadmap.md)
    # -----------------------------------------------------------------
    # v0.2: reordenar_etapas via drag-drop (sortable::rank_list)
    #
    # A implementacao inicial usava input$etapas_ordem gerado pelo
    # rank_list. Ver dev/notas_roadmap.md para bugs conhecidos que
    # impediram a integracao no MVP. Preservado no codigo para
    # retomada futura sem perder o trabalho de arquitetura.
    # -----------------------------------------------------------------
    
    # -----------------------------------------------------------------
    # Botoes individuais por etapa (Editar/Remover)
    # Renderiza uma linha por etapa + observers dinamicos
    # -----------------------------------------------------------------
    
    # v0.2: botoes por etapa fora dos cards - substituido por botoes
    # dentro do card no renderer .wf_render_etapas
    
    # Guarda de observers dinamicos para setas de reorder
    server_env$observers_etapa_up <- list()
    server_env$observers_etapa_down <- list()
    
    # Helper: aplicar reordenacao (mover etapa uma posicao)
    .mover_etapa <- function(etapa_id, direcao) {
      # direcao: -1 (up) ou +1 (down)
      det <- workflow_detalhe()
      if (is.null(det) || nrow(det$etapas) == 0L) return()
      
      ids_atuais <- as.integer(det$etapas$id)
      pos_atual <- which(ids_atuais == as.integer(etapa_id))
      if (length(pos_atual) == 0L) return()
      
      pos_nova <- pos_atual + direcao
      if (pos_nova < 1L || pos_nova > length(ids_atuais)) return()
      
      # Troca posicoes na sequencia
      ids_novos <- ids_atuais
      ids_novos[pos_atual] <- ids_atuais[pos_nova]
      ids_novos[pos_nova] <- ids_atuais[pos_atual]
      
      tryCatch({
        reordenar_etapas(con,
                         workflow_id = as.integer(workflow_id_atual()),
                         nova_ordem = ids_novos,
                         solicitante_id = op_atual_id())
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        .notify(paste("Erro ao reordenar:", conditionMessage(e)),
                tipo = "error")
      })
    }
    
    # Observers dinamicos: editar, remover, up, down (um por etapa)
    shiny::observe({
      ids <- etapas_ids_atuais()
      
      # Destruir antigos
      for (nm in names(server_env$observers_etapa_editar)) {
        try(server_env$observers_etapa_editar[[nm]]$destroy(),
            silent = TRUE)
      }
      for (nm in names(server_env$observers_etapa_remover)) {
        try(server_env$observers_etapa_remover[[nm]]$destroy(),
            silent = TRUE)
      }
      for (nm in names(server_env$observers_etapa_up)) {
        try(server_env$observers_etapa_up[[nm]]$destroy(),
            silent = TRUE)
      }
      for (nm in names(server_env$observers_etapa_down)) {
        try(server_env$observers_etapa_down[[nm]]$destroy(),
            silent = TRUE)
      }
      server_env$observers_etapa_editar <- list()
      server_env$observers_etapa_remover <- list()
      server_env$observers_etapa_up <- list()
      server_env$observers_etapa_down <- list()
      
      if (length(ids) == 0L) return()
      
      novos_editar <- list()
      novos_remover <- list()
      novos_up <- list()
      novos_down <- list()
      
      for (eid in ids) {
        local({
          eid_local <- eid
          
          obs_e <- shiny::observeEvent(
            input[[paste0("btn_editar_etapa_", eid_local)]],
            { .abrir_modal_editar_etapa(eid_local) },
            ignoreInit = TRUE, ignoreNULL = TRUE
          )
          novos_editar[[as.character(eid_local)]] <<- obs_e
          
          obs_r <- shiny::observeEvent(
            input[[paste0("btn_remover_etapa_", eid_local)]],
            { .abrir_modal_remover_etapa(eid_local) },
            ignoreInit = TRUE, ignoreNULL = TRUE
          )
          novos_remover[[as.character(eid_local)]] <<- obs_r
          
          obs_u <- shiny::observeEvent(
            input[[paste0("btn_etapa_up_", eid_local)]],
            { .mover_etapa(eid_local, -1L) },
            ignoreInit = TRUE, ignoreNULL = TRUE
          )
          novos_up[[as.character(eid_local)]] <<- obs_u
          
          obs_d <- shiny::observeEvent(
            input[[paste0("btn_etapa_down_", eid_local)]],
            { .mover_etapa(eid_local, 1L) },
            ignoreInit = TRUE, ignoreNULL = TRUE
          )
          novos_down[[as.character(eid_local)]] <<- obs_d
        })
      }
      server_env$observers_etapa_editar <- novos_editar
      server_env$observers_etapa_remover <- novos_remover
      server_env$observers_etapa_up <- novos_up
      server_env$observers_etapa_down <- novos_down
    })
  })
}