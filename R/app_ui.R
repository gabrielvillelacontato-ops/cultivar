#' UI principal do aplicativo CultivaR
#'
#' Renderiza apenas um container reativo: o conteudo e definido pelo
#' app_server com base no estado de autenticacao.
#'
#' @param request URL request (golem default).
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Recursos externos (CSS, JS, favicon)
    golem_add_external_resources(),
    
    # Container raiz: alterna entre tela de auth e app principal
    shiny::uiOutput("cv_root")
  )
}

#' Recursos externos carregados em todas as paginas
#'
#' Inclui dependencias bslib (Bootstrap 5), CSS customizado do CultivaR,
#' e tags meta padrao do golem.
#'
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath("www", system.file("app/www", package = "cultivaR"))
  
  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = system.file("app/www", package = "cultivaR"),
      app_title = "CultivaR"
    ),
    shiny::tags$link(rel = "stylesheet", type = "text/css",
                     href = "www/cultivar.css")
  )
}