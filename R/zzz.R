#' Hook de inicializacao do pacote
#'
#' Chamado automaticamente quando o pacote e carregado (via library
#' ou devtools::load_all). Aqui inicializamos integracoes que precisam
#' de opt-in explicito.
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  # sortable: opt-in para funcionar dentro de Shiny modules.
  # Sem isso, rank_list dentro de moduleServer nao emite input$xxx
  # corretamente para o namespace.
  if (requireNamespace("sortable", quietly = TRUE)) {
    try(sortable::enable_modules(), silent = TRUE)
  }
  invisible()
}