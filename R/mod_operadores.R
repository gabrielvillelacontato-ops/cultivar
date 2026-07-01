#' Modulo Shiny: gestao de operadores
#'
#' Layout em duas colunas: lista (esquerda) + painel de detalhes/edicao (direita).
#'
#' Acesso: exclusivo de admin. Se solicitante nao for admin, painel
#' fica desabilitado.
#'
#' Acoes disponiveis:
#'   - Listar operadores (ativos por default; toggle mostra inativos)
#'   - Criar novo operador
#'   - Mudar papel de operador (operador/supervisor/admin)
#'   - Desativar operador (ativo = 0)
#'   - Reativar operador (ativo = 1)
#'
#' Protecoes de UI:
#'   - Admin logado nao pode se desativar
#'   - Admin logado nao pode mudar proprio papel
#'   - Backend ja bloqueia "ultimo admin" para desativacao e mudanca
#'
#' @noRd

# Pipe operator helper
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------
# Validadores puros
# ---------------------------------------------------------------------

#' @noRd
.err_op_nome <- function(nome) {
  if (is.null(nome) || is.na(nome) || !nzchar(trimws(as.character(nome)))) {
    return("Nome obrigatorio.")
  }
  if (nchar(trimws(as.character(nome))) > 100L) {
    return("Ate 100 caracteres.")
  }
  NULL
}

#' @noRd
.err_op_pin <- function(pin) {
  if (is.null(pin) || is.na(pin) || !nzchar(as.character(pin))) {
    return("PIN obrigatorio.")
  }
  if (!grepl("^[0-9]{4}$", as.character(pin))) {
    return("PIN deve ter 4 digitos numericos.")
  }
  NULL
}

#' @noRd
.err_op_pin_conf <- function(pin, pin_conf) {
  if (is.null(pin_conf) || !nzchar(as.character(pin_conf %||% ""))) {
    return(NULL)  # so cobra confirmacao quando algo digitado
  }
  if (!identical(as.character(pin %||% ""), as.character(pin_conf))) {
    return("Os PINs nao conferem.")
  }
  NULL
}

#' @noRd
.err_op_email <- function(email) {
  # Email opcional. Se preenchido, precisa parecer email.
  if (is.null(email) || is.na(email) ||
      !nzchar(trimws(as.character(email)))) {
    return(NULL)
  }
  em <- trimws(as.character(email))
  if (!grepl("^[^@\\s]+@[^@\\s]+\\.[^@\\s]+$", em)) {
    return("Email invalido.")
  }
  NULL
}

# ---------------------------------------------------------------------
# UI publica
# ---------------------------------------------------------------------

#' @noRd
mod_operadores_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 8L,
      shiny::div(
        class = "cultivar-card",
        style = "padding: 16px; margin-bottom: 12px;",
        shiny::fluidRow(
          shiny::column(
            width = 6L,
            shiny::textInput(
              ns("termo_busca"),
              label = NULL,
              placeholder = "Buscar por nome..."
            )
          ),
          shiny::column(
            width = 6L,
            shiny::checkboxInput(
              ns("mostrar_inativos"),
              label = "Mostrar inativos",
              value = FALSE
            )
          )
        ),
        shiny::uiOutput(ns("ui_btn_novo"))
      ),
      shiny::div(
        class = "cultivar-card",
        style = "padding: 8px;",
        DT::dataTableOutput(ns("tabela"))
      )
    ),
    shiny::column(
      width = 4L,
      shiny::div(
        class = "cultivar-card",
        style = "padding: 16px; min-height: 400px;",
        shiny::uiOutput(ns("painel"))
      )
    )
  )
}

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------

#' @noRd
mod_operadores_server <- function(id, con, sessao_auth) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Estado ----
    selected_id <- shiny::reactiveVal(NULL)
    panel_state <- shiny::reactiveVal("vazio")
    reload_trigger <- shiny::reactiveVal(0L)
    mensagem_painel <- shiny::reactiveVal(NULL)
    
    # ---- Permissoes ----
    papel_atual <- shiny::reactive({
      s <- sessao_auth()
      if (is.null(s)) NA_character_ else s$papel[1]
    })
    e_admin <- shiny::reactive(isTRUE(papel_atual() == "admin"))
    op_atual_id <- shiny::reactive({
      s <- sessao_auth()
      if (is.null(s)) NA_integer_ else as.integer(s$id[1])
    })
    
    # ---- Lista de operadores ----
    operadores_filtrados <- shiny::reactive({
      reload_trigger()
      incluir_arq <- isTRUE(input$mostrar_inativos)
      termo <- input$termo_busca %||% ""
      
      df <- tryCatch(
        listar_operadores(con, incluir_arquivados = incluir_arq),
        error = function(e) {
          mensagem_painel(list(
            tipo = "danger",
            texto = paste("Erro ao listar operadores:",
                          conditionMessage(e))
          ))
          NULL
        }
      )
      if (is.null(df) || nrow(df) == 0L) return(df)
      
      # Filtro por status: se nao mostrar inativos, so ativo = 1
      if (!incluir_arq) {
        df <- df[df$ativo == 1L, , drop = FALSE]
      }
      
      # Filtro por termo (nome)
      if (nzchar(trimws(termo))) {
        pat <- tolower(trimws(termo))
        df <- df[grepl(pat, tolower(df$nome), fixed = TRUE), ,
                 drop = FALSE]
      }
      df
    })
    
    output$ui_btn_novo <- shiny::renderUI({
      if (!isTRUE(e_admin())) return(NULL)
      shiny::actionButton(
        ns("btn_novo"),
        label = "+ Novo operador",
        class = "btn-primary",
        style = "margin-top: 8px;"
      )
    })
    
    output$tabela <- DT::renderDataTable({
      df <- operadores_filtrados()
      if (is.null(df) || nrow(df) == 0L) {
        return(DT::datatable(
          data.frame(Aviso = "Nenhum operador encontrado."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      
      df_view <- data.frame(
        Nome = df$nome,
        Papel = df$papel,
        Email = ifelse(is.na(df$email), "-", df$email),
        Status = ifelse(df$ativo == 1L, "Ativo", "Inativo"),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        df_view,
        selection = "single",
        rownames = FALSE,
        options = list(
          pageLength = 50,
          dom = "ft",
          language = list(
            search = "Filtrar:",
            zeroRecords = "Nenhum registro encontrado",
            info = "_TOTAL_ operador(es)",
            infoEmpty = "0 operadores",
            infoFiltered = "(filtrados de _MAX_)"
          )
        )
      )
    })
    
    # ---- Selecao ----
    shiny::observeEvent(input$tabela_rows_selected, {
      idx <- input$tabela_rows_selected
      df <- operadores_filtrados()
      if (is.null(idx) || length(idx) == 0L || is.null(df) ||
          nrow(df) == 0L || idx > nrow(df)) {
        return()
      }
      selected_id(as.integer(df$id[idx]))
      panel_state("visualizando")
      mensagem_painel(NULL)
    }, ignoreNULL = FALSE)
    
    # ---- Botoes de estado ----
    shiny::observeEvent(input$btn_novo, {
      if (!isTRUE(e_admin())) return()
      selected_id(NULL)
      panel_state("criando")
      mensagem_painel(NULL)
    })
    
    shiny::observeEvent(input$btn_editar, {
      if (!isTRUE(e_admin()) || is.null(selected_id())) return()
      panel_state("editando")
      mensagem_painel(NULL)
    })
    
    shiny::observeEvent(input$btn_cancelar, {
      if (is.null(selected_id())) {
        panel_state("vazio")
      } else {
        panel_state("visualizando")
      }
      mensagem_painel(NULL)
    })
    
    # ---- Desativar (com confirmacao) ----
    shiny::observeEvent(input$btn_desativar, {
      if (!isTRUE(e_admin()) || is.null(selected_id())) return()
      
      # Proibe auto-desativacao (UI, redundante com bom senso)
      if (identical(selected_id(), op_atual_id())) {
        mensagem_painel(list(
          tipo = "warning",
          texto = "Voce nao pode desativar a si mesmo."
        ))
        return()
      }
      
      shiny::showModal(shiny::modalDialog(
        title = "Confirmar desativacao",
        "Este operador nao podera mais fazer login ate ser reativado.",
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(
            ns("btn_desativar_confirmar"),
            "Desativar",
            class = "btn-danger"
          )
        )
      ))
    })
    
    shiny::observeEvent(input$btn_desativar_confirmar, {
      shiny::removeModal()
      tryCatch({
        desativar_operador(con, selected_id(), op_atual_id())
        mensagem_painel(list(tipo = "success",
                             texto = "Operador desativado."))
        reload_trigger(reload_trigger() + 1L)
        panel_state("visualizando")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Reativar ----
    shiny::observeEvent(input$btn_reativar, {
      if (!isTRUE(e_admin()) || is.null(selected_id())) return()
      tryCatch({
        reativar_operador(con, selected_id(), op_atual_id())
        mensagem_painel(list(tipo = "success",
                             texto = "Operador reativado."))
        reload_trigger(reload_trigger() + 1L)
        panel_state("visualizando")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Salvar edicao ----
    shiny::observeEvent(input$btn_salvar_edicao, {
      if (!isTRUE(e_admin()) || is.null(selected_id())) return()
      
      novo_papel <- input$papel_edit
      
      # Proibe auto-rebaixamento
      if (identical(selected_id(), op_atual_id()) &&
          !identical(novo_papel, "admin")) {
        mensagem_painel(list(
          tipo = "warning",
          texto = "Voce nao pode rebaixar a si mesmo."
        ))
        return()
      }
      
      if (!novo_papel %in% c("operador", "supervisor", "admin")) {
        mensagem_painel(list(
          tipo = "danger",
          texto = "Papel invalido."
        ))
        return()
      }
      
      tryCatch({
        mudar_papel(con, selected_id(),
                    papel_novo = novo_papel,
                    solicitante_id = op_atual_id())
        mensagem_painel(list(tipo = "success",
                             texto = "Papel atualizado."))
        reload_trigger(reload_trigger() + 1L)
        panel_state("visualizando")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Validacao reativa (criacao) ----
    output$err_nome_novo <- shiny::renderUI({
      e <- .err_op_nome(input$nome_novo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_email_novo <- shiny::renderUI({
      e <- .err_op_email(input$email_novo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_pin_novo <- shiny::renderUI({
      e <- .err_op_pin(input$pin_novo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_pin_conf_novo <- shiny::renderUI({
      e <- .err_op_pin_conf(input$pin_novo, input$pin_conf_novo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    # ---- Salvar novo ----
    shiny::observeEvent(input$btn_salvar_novo, {
      if (!isTRUE(e_admin())) return()
      
      nome <- input$nome_novo
      email <- input$email_novo
      pin <- input$pin_novo
      pin_conf <- input$pin_conf_novo
      papel_novo_op <- input$papel_novo %||% "operador"
      
      erros <- c(
        .err_op_nome(nome),
        .err_op_pin(pin),
        .err_op_pin_conf(pin, pin_conf),
        .err_op_email(email)
      )
      if (!nzchar(as.character(pin_conf %||% ""))) {
        erros <- c(erros, "Confirmacao de PIN obrigatoria.")
      }
      if (length(erros) > 0L) {
        mensagem_painel(list(
          tipo = "warning",
          texto = paste("Corrija os campos:",
                        paste(erros, collapse = " "))
        ))
        return()
      }
      
      email_final <- if (is.null(email) || is.na(email) ||
                         !nzchar(trimws(as.character(email)))) {
        NA_character_
      } else {
        trimws(as.character(email))
      }
      
      tryCatch({
        novo_id <- criar_operador(
          con,
          nome = trimws(nome),
          pin = pin,
          papel = papel_novo_op,
          email = email_final,
          criado_por_id = op_atual_id()
        )
        mensagem_painel(list(tipo = "success",
                             texto = "Operador criado."))
        reload_trigger(reload_trigger() + 1L)
        selected_id(as.integer(novo_id))
        panel_state("visualizando")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro ao criar operador:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Renderiza painel ----
    output$painel <- shiny::renderUI({
      state <- panel_state()
      msg <- mensagem_painel()
      
      # Sem admin -> so mensagem
      if (!isTRUE(e_admin())) {
        return(shiny::div(
          class = "alert alert-warning",
          "Apenas administradores podem gerenciar operadores."
        ))
      }
      
      conteudo <- switch(
        state,
        "vazio" = .op_painel_vazio(),
        "visualizando" = .op_painel_visualizando(
          ns, selected_id(), con, op_atual_id()
        ),
        "editando" = .op_painel_editando(
          ns, selected_id(), con, op_atual_id()
        ),
        "criando" = .op_painel_criando(ns),
        .op_painel_vazio()
      )
      
      alerta <- NULL
      if (!is.null(msg)) {
        alerta <- ui_alerta(mensagem = msg$texto, tipo = msg$tipo)
      }
      
      shiny::tagList(alerta, conteudo)
    })
  })
}

# ---------------------------------------------------------------------
# Sub-renderizadores
# ---------------------------------------------------------------------

#' @noRd
.op_painel_vazio <- function() {
  shiny::div(
    style = "color: #888; text-align: center; padding: 60px 20px;",
    shiny::tags$p(
      shiny::tags$strong("Selecione um operador"),
      shiny::br(),
      "para ver detalhes ou usar +Novo para criar."
    )
  )
}

#' @noRd
.op_painel_visualizando <- function(ns, op_id, con, solicitante_id) {
  if (is.null(op_id)) return(.op_painel_vazio())
  
  df <- tryCatch(
    listar_operadores(con, incluir_arquivados = TRUE),
    error = function(e) NULL
  )
  if (is.null(df)) {
    return(shiny::div("Erro ao carregar operador."))
  }
  linha <- df[df$id == op_id, , drop = FALSE]
  if (nrow(linha) == 0L) {
    return(shiny::div("Operador nao encontrado."))
  }
  
  ativo <- linha$ativo[1] == 1L
  eh_proprio <- identical(as.integer(op_id),
                          as.integer(solicitante_id %||% -1L))
  
  badges <- shiny::tagList(
    ui_badge_papel(linha$papel[1]), " ",
    if (ativo) {
      ui_badge(texto = "Ativo", tipo = "success")
    } else {
      ui_badge(texto = "Inativo", tipo = "secondary")
    },
    if (eh_proprio) {
      shiny::tagList(" ", ui_badge(texto = "Voce", tipo = "info"))
    } else NULL
  )
  
  # Botoes contextuais
  botoes <- shiny::tagList()
  
  # Editar sempre permitido (mas server bloqueia auto-rebaixamento)
  botoes <- shiny::tagList(
    botoes,
    shiny::actionButton(ns("btn_editar"), "Editar papel",
                        class = "btn-primary"),
    " "
  )
  
  if (ativo) {
    if (!eh_proprio) {
      botoes <- shiny::tagList(
        botoes,
        shiny::actionButton(ns("btn_desativar"), "Desativar",
                            class = "btn-outline-danger")
      )
    }
  } else {
    botoes <- shiny::tagList(
      botoes,
      shiny::actionButton(ns("btn_reativar"), "Reativar",
                          class = "btn-success")
    )
  }
  
  shiny::tagList(
    shiny::h4(linha$nome[1]),
    shiny::div(badges),
    ui_vspace(),
    shiny::tags$dl(
      if (!is.na(linha$email[1])) shiny::tagList(
        shiny::tags$dt("Email"),
        shiny::tags$dd(linha$email[1])
      ),
      shiny::tags$dt("Papel"),
      shiny::tags$dd(linha$papel[1]),
      shiny::tags$dt("Criado em"),
      shiny::tags$dd(ui_format_timestamp(linha$criado_em[1])),
      if (!is.na(linha$bloqueado_ate[1])) shiny::tagList(
        shiny::tags$dt("Bloqueado ate"),
        shiny::tags$dd(ui_format_timestamp(linha$bloqueado_ate[1]))
      )
    ),
    ui_vspace(),
    botoes
  )
}

#' @noRd
.op_painel_editando <- function(ns, op_id, con, solicitante_id) {
  df <- tryCatch(
    listar_operadores(con, incluir_arquivados = TRUE),
    error = function(e) NULL
  )
  if (is.null(df)) return(shiny::div("Erro ao carregar operador."))
  linha <- df[df$id == op_id, , drop = FALSE]
  if (nrow(linha) == 0L) {
    return(shiny::div("Operador nao encontrado."))
  }
  
  eh_proprio <- identical(as.integer(op_id),
                          as.integer(solicitante_id %||% -1L))
  
  aviso_proprio <- if (eh_proprio) {
    shiny::div(
      class = "alert alert-warning",
      style = "font-size: 12px; padding: 8px;",
      "Voce nao pode rebaixar o proprio papel."
    )
  } else NULL
  
  shiny::tagList(
    shiny::h4("Editar papel"),
    shiny::div(
      style = "color: #666; margin-bottom: 12px;",
      "Operador: ", shiny::tags$strong(linha$nome[1])
    ),
    aviso_proprio,
    shiny::selectInput(
      ns("papel_edit"),
      "Papel",
      choices = c("operador" = "operador",
                  "supervisor" = "supervisor",
                  "admin" = "admin"),
      selected = linha$papel[1]
    ),
    ui_vspace(),
    shiny::actionButton(ns("btn_salvar_edicao"), "Salvar",
                        class = "btn-primary"),
    " ",
    shiny::actionButton(ns("btn_cancelar"), "Cancelar")
  )
}

#' @noRd
.op_painel_criando <- function(ns) {
  shiny::tagList(
    shiny::h4("Novo operador"),
    
    shiny::textInput(ns("nome_novo"), "Nome*",
                     placeholder = "Ex: Ana Silva"),
    shiny::uiOutput(ns("err_nome_novo")),
    
    shiny::textInput(ns("email_novo"), "Email",
                     placeholder = "opcional"),
    shiny::uiOutput(ns("err_email_novo")),
    
    shiny::selectInput(
      ns("papel_novo"), "Papel*",
      choices = c("operador" = "operador",
                  "supervisor" = "supervisor",
                  "admin" = "admin"),
      selected = "operador"
    ),
    
    shiny::passwordInput(ns("pin_novo"), "PIN (4 digitos)*"),
    shiny::uiOutput(ns("err_pin_novo")),
    
    shiny::passwordInput(ns("pin_conf_novo"), "Confirme o PIN*"),
    shiny::uiOutput(ns("err_pin_conf_novo")),
    
    ui_vspace(),
    shiny::actionButton(ns("btn_salvar_novo"), "Criar",
                        class = "btn-primary"),
    " ",
    shiny::actionButton(ns("btn_cancelar"), "Cancelar")
  )
}