#' Server principal do aplicativo CultivaR
#'
#' Orquestra autenticacao, timeout e renderizacao condicional da UI
#' principal vs tela de auth.
#'
#' @param input,output,session Shiny.
#' @noRd
app_server <- function(input, output, session) {
  
  # -------------------------------------------------------------------
  # Conexao com o banco
  # -------------------------------------------------------------------
  # Obtem caminho passado por run_app() via golem_opts
  db_path <- golem::get_golem_options("db_path") %||% DB_PATH_DEV
  
  if (!file.exists(db_path)) {
    stop("Banco nao encontrado em '", db_path,
         "'. Execute os scripts de dev/ para criar e popular.",
         call. = FALSE)
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  })
  
  # -------------------------------------------------------------------
  # Modulo de autenticacao
  # -------------------------------------------------------------------
  auth <- mod_auth_server("auth", con = con)
  
  # -------------------------------------------------------------------
  # Timeout de inatividade (30 min)
  # -------------------------------------------------------------------
  ultima_atividade <- shiny::reactiveVal(Sys.time())
  
  # Atualiza ultima_atividade a cada interacao
  shiny::observe({
    # Depende de qualquer input mudar
    shiny::reactiveValuesToList(input)
    ultima_atividade(Sys.time())
  })
  
  # Verifica a cada 60s
  shiny::observe({
    shiny::invalidateLater(60 * 1000, session)
    op <- auth$operador()
    if (is.null(op)) return()  # so vale para sessao logada
    
    minutos <- as.numeric(difftime(Sys.time(), ultima_atividade(),
                                   units = "mins"))
    if (minutos >= 30) {
      shiny::showModal(shiny::modalDialog(
        title = "Sessao expirada",
        "Sua sessao expirou por inatividade. Faca login novamente.",
        footer = shiny::modalButton("OK"),
        easyClose = FALSE
      ))
      auth$logout()
    } else if (minutos >= 25) {
      shiny::showModal(shiny::modalDialog(
        title = "Sessao prestes a expirar",
        sprintf("Sua sessao expira em %d minutos. ",
                30 - floor(minutos)),
        "Clique em qualquer lugar para continuar.",
        footer = shiny::modalButton("Continuar trabalhando"),
        easyClose = TRUE
      ))
    }
  })
  
  # -------------------------------------------------------------------
  # Renderizacao condicional: auth vs app principal
  # -------------------------------------------------------------------
  output$cv_root <- shiny::renderUI({
    op <- auth$operador()
    if (is.null(op)) {
      # Nao logado: renderiza a tela de auth (setup ou login)
      return(mod_auth_ui("auth"))
    }
    # Logado: renderiza app principal
    .ui_app_principal(op)
  })
  
  # -------------------------------------------------------------------
  # Logout
  # -------------------------------------------------------------------
  shiny::observeEvent(input$cv_logout, {
    auth$logout()
  })
}

# ---------------------------------------------------------------------
# UI do app principal (apos login)
# ---------------------------------------------------------------------

#' Renderiza o layout principal pos-login
#'
#' @param op data.frame com 1 linha: operador logado.
#' @noRd
.ui_app_principal <- function(op) {
  papel <- op$papel[1]
  
  bslib::page_navbar(
    title = htmltools::tags$span(
      htmltools::tags$strong("CultivaR")
    ),
    id = "cv_nav",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "flatly",
      primary = "#6c5ce7"
    ),
    fillable = FALSE,
    
    # Sidebar compartilhada
    sidebar = bslib::sidebar(
      width = 260,
      open = "always",
      
      # Bloco do usuario
      htmltools::tags$div(
        class = "cv-user-strip",
        htmltools::tags$div(
          htmltools::tags$strong(op$nome[1]),
          " ",
          ui_badge_papel(papel)
        ),
        htmltools::tags$div(
          class = "mt-2",
          shiny::actionLink("cv_logout", "Sair",
                            icon = shiny::icon("right-from-bracket"))
        )
      )
    ),
    
    # Painel: Meios
    bslib::nav_panel(
      title = "Catalogo de Meios",
      icon = shiny::icon("flask"),
      .placeholder_em_breve(
        "Catalogo de Meios",
        "Listagem, busca e edicao de meios de cultura. ",
        "Sera entregue na proxima etapa do desenvolvimento."
      )
    ),
    
    # Painel: Preparos (futuro)
    bslib::nav_panel(
      title = "Preparos",
      icon = shiny::icon("vial"),
      .placeholder_em_breve(
        "Preparos",
        "Wizard passo a passo para registrar preparo de lotes. ",
        "Disponivel no Dia 5."
      )
    ),
    
    # Painel: Workflows (futuro)
    bslib::nav_panel(
      title = "Workflows",
      icon = shiny::icon("diagram-project"),
      .placeholder_em_breve(
        "Workflows",
        "Definicao de pipelines como o S&R 2001 para transformacao genetica. ",
        "Disponivel no Dia 6."
      )
    ),
    
    # Painel: Auditoria (futuro, apenas supervisor/admin)
    if (papel %in% c("supervisor", "admin")) {
      bslib::nav_panel(
        title = "Auditoria",
        icon = shiny::icon("clipboard-list"),
        .placeholder_em_breve(
          "Auditoria",
          "Timeline visual de todas as acoes no sistema (ALCOA+). ",
          "Disponivel no Dia 6."
        )
      )
    } else NULL,
    
    # Painel: Operadores (apenas admin)
    if (papel == "admin") {
      bslib::nav_panel(
        title = "Operadores",
        icon = shiny::icon("users"),
        .placeholder_em_breve(
          "Operadores",
          "Cadastro, edicao de papeis e desativacao de operadores. ",
          "Disponivel no Dia 6."
        )
      )
    } else NULL
  )
}

#' Placeholder de modulo em desenvolvimento
#' @noRd
.placeholder_em_breve <- function(titulo, ...) {
  htmltools::tags$div(
    ui_breadcrumb(c("CultivaR", titulo)),
    htmltools::tags$div(
      class = "cv-empty-state",
      htmltools::tags$div(
        class = "cv-empty-state-icon",
        bsicons::bs_icon("tools")
      ),
      htmltools::tags$h3(titulo),
      htmltools::tags$p(...)
    )
  )
}