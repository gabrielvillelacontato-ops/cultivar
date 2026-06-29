#' Modulo Shiny de autenticacao (setup inicial + login)
#'
#' Responsavel por:
#'   1. Detectar se o sistema precisa de setup inicial (zero operadores
#'      ativos) e mostrar formulario "Criar primeiro administrador".
#'   2. Mostrar tela de login (nome + PIN) com tratamento de lockout.
#'   3. Expor um reactive com o operador logado (NULL se nao logado).
#'
#' O modulo NAO renderiza a interface do app principal — apenas decide
#' entre "setup", "login" e "app pronto". A camada acima (app_ui/server)
#' usa o reactive exposto para condicionalmente renderizar o resto.
#'
#' Uso tipico:
#'   # ui.R
#'   mod_auth_ui("auth")
#'
#'   # server.R
#'   operador_atual <- mod_auth_server("auth", con = pool)
#'   observe({
#'     if (is.null(operador_atual())) {
#'       # mostra UI de auth
#'     } else {
#'       # mostra app
#'     }
#'   })
#'
#' @noRd

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------

#' UI do modulo de autenticacao
#'
#' Renderiza um container que sera preenchido reativamente pelo server
#' com a tela apropriada (setup inicial ou login).
#'
#' @param id Namespace do modulo.
#' @noRd
mod_auth_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("tela_auth"))
}

# ---------------------------------------------------------------------
# UI helpers internos (gerados pelo server conforme estado)
# ---------------------------------------------------------------------

#' Renderiza a tela de setup inicial (criar primeiro admin)
#' @noRd
.ui_setup_inicial <- function(ns) {
  ui_auth_card(
    titulo = "Bem-vindo ao CultivaR",
    htmltools::tags$p(
      class = "cv-auth-subtitle",
      "Este sistema ainda nao tem nenhum administrador cadastrado. ",
      "Crie o primeiro administrador para comecar."
    ),
    shiny::textInput(
      ns("setup_nome"),
      label = "Seu nome",
      placeholder = "Ex: Maria Silva",
      width = "100%"
    ),
    shiny::passwordInput(
      ns("setup_pin"),
      label = "PIN (4 digitos)",
      placeholder = "____",
      width = "100%"
    ),
    shiny::passwordInput(
      ns("setup_pin_conf"),
      label = "Confirme o PIN",
      placeholder = "____",
      width = "100%"
    ),
    ui_vspace(0.5),
    shiny::actionButton(
      ns("setup_criar"),
      label = "Criar administrador",
      class = "btn btn-primary btn-lg w-100",
      icon = shiny::icon("user-plus")
    ),
    ui_vspace(0.75),
    shiny::uiOutput(ns("setup_alerta"))
  )
}

#' Renderiza a tela de login
#' @noRd
.ui_login <- function(ns) {
  ui_auth_card(
    titulo = "CultivaR",
    htmltools::tags$p(
      class = "cv-auth-subtitle",
      "Identifique-se para acessar o sistema."
    ),
    shiny::textInput(
      ns("login_nome"),
      label = "Nome",
      placeholder = "Seu nome",
      width = "100%"
    ),
    shiny::passwordInput(
      ns("login_pin"),
      label = "PIN",
      placeholder = "____",
      width = "100%"
    ),
    ui_vspace(0.5),
    shiny::actionButton(
      ns("login_entrar"),
      label = "Entrar",
      class = "btn btn-primary btn-lg w-100",
      icon = shiny::icon("right-to-bracket")
    ),
    ui_vspace(0.75),
    shiny::uiOutput(ns("login_alerta"))
  )
}

# ---------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------

#' Server do modulo de autenticacao
#'
#' @param id Namespace.
#' @param con Conexao DBI (ou pool) com o banco.
#' @param tenant_id Inteiro. Default TENANT_DEFAULT_ID.
#' @return Reactive: data.frame com 1 linha (operador logado) ou NULL.
#' @noRd
mod_auth_server <- function(id, con, tenant_id = TENANT_DEFAULT_ID) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Estado reativo
    operador_logado <- shiny::reactiveVal(NULL)
    msg_setup       <- shiny::reactiveVal(NULL)
    msg_login       <- shiny::reactiveVal(NULL)
    
    # Forca re-checagem do estado de setup (apos criacao do primeiro admin)
    setup_trigger <- shiny::reactiveVal(0L)
    
    precisa_setup <- shiny::reactive({
      setup_trigger()  # dependencia explicita
      precisa_setup_inicial(con, tenant_id)
    })
    
    # -----------------------------------------------------------------
    # Roteamento de tela
    # -----------------------------------------------------------------
    output$tela_auth <- shiny::renderUI({
      if (!is.null(operador_logado())) {
        # Operador logado: este modulo nao desenha nada, app principal
        # toma conta da tela.
        return(NULL)
      }
      if (precisa_setup()) {
        .ui_setup_inicial(ns)
      } else {
        .ui_login(ns)
      }
    })
    
    # -----------------------------------------------------------------
    # Acao: criar primeiro administrador (setup inicial)
    # -----------------------------------------------------------------
    shiny::observeEvent(input$setup_criar, {
      nome <- input$setup_nome
      pin  <- input$setup_pin
      conf <- input$setup_pin_conf
      
      # Validacao client-side rapida (mensagens amigaveis)
      if (is.null(nome) || !nzchar(trimws(nome %||% ""))) {
        msg_setup(ui_alerta("Informe seu nome.", "danger"))
        return()
      }
      if (is.null(pin) || !nzchar(pin)) {
        msg_setup(ui_alerta("Informe o PIN de 4 digitos.", "danger"))
        return()
      }
      if (!grepl("^[0-9]{4}$", pin)) {
        msg_setup(ui_alerta("O PIN deve ter exatamente 4 digitos numericos.",
                            "danger"))
        return()
      }
      if (!identical(pin, conf)) {
        msg_setup(ui_alerta("Os PINs nao conferem. Digite novamente.",
                            "danger"))
        return()
      }
      
      # Tentar criar
      resultado <- tryCatch({
        novo_id <- criar_operador(
          con, nome = nome, pin = pin, papel = "admin",
          tenant_id = tenant_id
        )
        list(ok = TRUE, id = novo_id)
      }, error = function(e) {
        list(ok = FALSE, msg = conditionMessage(e))
      })
      
      if (resultado$ok) {
        # Auto-login do recem-criado admin
        op <- buscar_operador_por_nome(con, trimws(nome), tenant_id)
        registrar_login_sucesso(con, op$id[1], tenant_id)
        op_atualizado <- buscar_operador_por_nome(con, trimws(nome), tenant_id)
        operador_logado(op_atualizado)
        msg_setup(NULL)
        setup_trigger(setup_trigger() + 1L)
      } else {
        msg_setup(ui_alerta(resultado$msg, "danger"))
      }
    })
    
    output$setup_alerta <- shiny::renderUI({ msg_setup() })
    
    # -----------------------------------------------------------------
    # Acao: login
    # -----------------------------------------------------------------
    shiny::observeEvent(input$login_entrar, {
      nome <- input$login_nome
      pin  <- input$login_pin
      
      if (is.null(nome) || !nzchar(trimws(nome %||% ""))) {
        msg_login(ui_alerta("Informe seu nome.", "danger"))
        return()
      }
      if (is.null(pin) || !grepl("^[0-9]{4}$", pin %||% "")) {
        msg_login(ui_alerta("O PIN deve ter 4 digitos numericos.", "danger"))
        return()
      }
      
      res <- tryCatch(
        autenticar(con, nome = nome, pin = pin, tenant_id = tenant_id),
        error = function(e) list(sucesso = FALSE, motivo = "erro_interno",
                                 mensagem = conditionMessage(e))
      )
      
      if (isTRUE(res$sucesso)) {
        operador_logado(res$operador)
        msg_login(NULL)
        return()
      }
      
      msg <- switch(
        res$motivo,
        "usuario_nao_encontrado" = "Nome ou PIN incorretos.",
        "pin_incorreto"          = "Nome ou PIN incorretos.",
        "arquivado"  = "Esta conta foi arquivada. Procure um administrador.",
        "inativo"    = "Esta conta esta desativada. Procure um administrador.",
        "bloqueado"  = sprintf(
          "Conta bloqueada por excesso de tentativas. Tente novamente apos %s.",
          ui_format_timestamp(res$bloqueado_ate %||% "")
        ),
        "erro_interno" = sprintf("Erro interno: %s",
                                 res$mensagem %||% "desconhecido"),
        "Nao foi possivel autenticar."
      )
      msg_login(ui_alerta(msg, "danger"))
    })
    
    output$login_alerta <- shiny::renderUI({ msg_login() })
    
    # -----------------------------------------------------------------
    # Funcao publica: logout
    # -----------------------------------------------------------------
    logout <- function() {
      op <- operador_logado()
      if (!is.null(op) && nrow(op) > 0L) {
        tryCatch(
          registrar_logout(con, op$id[1], tenant_id),
          error = function(e) NULL
        )
      }
      operador_logado(NULL)
      msg_login(NULL)
      msg_setup(NULL)
    }
    
    # Retorno: lista com reactive do operador + funcao de logout
    list(
      operador = shiny::reactive(operador_logado()),
      logout = logout
    )
  })
}

# ---------------------------------------------------------------------
# Helper interno: operador NULL-coalesce
# ---------------------------------------------------------------------

#' Operador `%||%` (null coalescing)
#' @noRd
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a