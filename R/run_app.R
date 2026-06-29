#' Roda o aplicativo CultivaR
#'
#' @param db_path Caminho para o arquivo SQLite. Default: DB_PATH_DEV
#'   (resolvido relativo ao diretorio do projeto).
#' @param ... Argumentos passados para shiny::shinyApp().
#' @export
run_app <- function(db_path = NULL, ...) {
  caminho <- db_path %||% system.file("extdata/cultivar.sqlite",
                                      package = "cultivaR")
  if (!nzchar(caminho) || !file.exists(caminho)) {
    # Fallback para desenvolvimento (load_all com pasta nao instalada)
    caminho <- file.path(DB_PATH_DEV)
  }
  
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = NULL,
      options = list()
    ),
    golem_opts = list(db_path = caminho)
  )
}