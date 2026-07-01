#' Utilitarios de UI
#'
#' Helpers visuais reutilizaveis: badges, breadcrumbs, formatacao de
#' timestamps, alertas. Toda funcao retorna htmltools::tag (ou similar)
#' para uso direto em UIs Shiny/bslib.
#'
#' @noRd

# ---------------------------------------------------------------------
# Badges semanticos
# ---------------------------------------------------------------------

#' Renderiza um badge de papel de operador com cor por nivel hierarquico
#'
#' admin -> vermelho, supervisor -> azul, operador -> cinza
#'
#' @param papel String: 'admin', 'supervisor' ou 'operador'.
#' @return htmltools::tag
#' @noRd
ui_badge_papel <- function(papel) {
  if (is.null(papel) || is.na(papel)) {
    return(htmltools::tags$span(class = "badge bg-secondary", "?"))
  }
  cls <- switch(
    papel,
    "admin"      = "badge bg-danger",
    "supervisor" = "badge bg-primary",
    "operador"   = "badge bg-secondary",
    "badge bg-secondary"
  )
  htmltools::tags$span(class = cls, papel)
}

#' Badge de incerteza cientifica em um meio
#'
#' Usado quando meios.flag_incerteza == 1
#' @noRd
ui_badge_incerteza <- function(visivel = TRUE) {
  if (!isTRUE(visivel)) return(NULL)
  htmltools::tags$span(
    class = "badge bg-warning text-dark",
    title = "Este meio tem incerteza documentada em sua composicao.",
    bsicons::bs_icon("exclamation-triangle-fill"),
    " incerteza"
  )
}

#' Badge de meio bloqueado para preparo
#'
#' Usado quando meios.bloqueado_preparo == 1
#' @noRd
ui_badge_bloqueado <- function(visivel = TRUE) {
  if (!isTRUE(visivel)) return(NULL)
  htmltools::tags$span(
    class = "badge bg-dark",
    title = "Este meio esta bloqueado para preparo.",
    bsicons::bs_icon("lock-fill"),
    " bloqueado"
  )
}

#' Badge generico de status
#'
#' @param texto String visivel no badge.
#' @param tipo 'success', 'warning', 'danger', 'info', 'primary', 'secondary'.
#' @noRd
ui_badge <- function(texto, tipo = "secondary") {
  htmltools::tags$span(class = paste0("badge bg-", tipo), texto)
}

# ---------------------------------------------------------------------
# Breadcrumb (caminho de navegacao)
# ---------------------------------------------------------------------

#' Constroi um breadcrumb simples
#'
#' @param itens Vetor character com os niveis do caminho. Ex:
#'   c("Catalogo", "MSR", "Composicao"). O ultimo e marcado como ativo.
#' @return htmltools::tag (nav.breadcrumb estilo Bootstrap 5)
#' @noRd
ui_breadcrumb <- function(itens) {
  if (length(itens) == 0L) return(NULL)
  itens_li <- lapply(seq_along(itens), function(i) {
    if (i == length(itens)) {
      htmltools::tags$li(
        class = "breadcrumb-item active",
        `aria-current` = "page",
        itens[i]
      )
    } else {
      htmltools::tags$li(class = "breadcrumb-item", itens[i])
    }
  })
  htmltools::tags$nav(
    `aria-label` = "breadcrumb",
    htmltools::tags$ol(
      class = "breadcrumb",
      itens_li
    )
  )
}

# ---------------------------------------------------------------------
# Formatacao de timestamps
# ---------------------------------------------------------------------

#' Formata ISO 8601 UTC para exibicao pt-BR (DD/MM/AAAA HH:MM)
#'
#' Aceita NA ou NULL e retorna string vazia.
#' @noRd
ui_format_timestamp <- function(ts) {
  if (is.null(ts) || length(ts) == 0L || is.na(ts) || !nzchar(ts)) {
    return("")
  }
  parsed <- tryCatch(
    as.POSIXct(ts, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    error = function(e) NA
  )
  if (is.na(parsed)) return(ts)
  format(parsed, "%d/%m/%Y %H:%M", tz = Sys.timezone())
}

# ---------------------------------------------------------------------
# Alertas inline (mensagens de feedback)
# ---------------------------------------------------------------------

#' Renderiza um alerta inline tipo Bootstrap
#'
#' @param mensagem String.
#' @param tipo 'success', 'warning', 'danger', 'info'.
#' @param icone Logical, se TRUE adiciona icone correspondente ao tipo.
#' @noRd
ui_alerta <- function(mensagem, tipo = "info", icone = TRUE) {
  if (is.null(mensagem) || !nzchar(mensagem)) return(NULL)
  cls <- paste0("alert alert-", tipo, " mb-3")
  icone_tag <- if (isTRUE(icone)) {
    nome <- switch(
      tipo,
      "success" = "check-circle-fill",
      "warning" = "exclamation-triangle-fill",
      "danger"  = "x-circle-fill",
      "info"    = "info-circle-fill",
      "info-circle-fill"
    )
    htmltools::tagList(bsicons::bs_icon(nome), " ")
  } else {
    NULL
  }
  htmltools::tags$div(
    class = cls, role = "alert",
    icone_tag, mensagem
  )
}

# ---------------------------------------------------------------------
# Helpers de layout
# ---------------------------------------------------------------------

#' Card centralizado para telas de auth (login, setup inicial)
#'
#' Renderiza um card maximo de ~480px, centralizado vertical e horizontalmente
#' na viewport. Usado nas telas pre-login.
#'
#' @param titulo String titulo do card.
#' @param ... Conteudo do card.
#' @noRd
ui_auth_card <- function(titulo, ...) {
  htmltools::tags$div(
    class = "cv-auth-wrapper",
    htmltools::tags$div(
      class = "cv-auth-card",
      htmltools::tags$h2(class = "cv-auth-title", titulo),
      ...
    )
  )
}

#' Espacador vertical
#' @noRd
ui_vspace <- function(rem = 1) {
  htmltools::tags$div(style = sprintf("height: %srem;", rem))
}
# ---------------------------------------------------------------------
# Helpers globais reutilizados pelos modulos
# ---------------------------------------------------------------------

#' Operador null-coalesce
#'
#' Retorna `b` se `a` for NULL; caso contrario retorna `a`.
#'
#' @noRd
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Formatacao numerica no padrao BR
#'
#' Separador de milhar ponto, decimal virgula. Retorna "-" para NA/NULL.
#'
#' @param x Numeric escalar.
#' @param decimais Numero de casas decimais (default 2).
#' @return String.
#' @noRd
ui_fmt_br <- function(x, decimais = 2L) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("-")
  formatC(as.numeric(x), format = "f", digits = decimais,
          big.mark = ".", decimal.mark = ",")
}

#' Formatacao de concentracao em mg/L com casas decimais adaptativas
#'
#' Regra: >=1000 sem casas, 100-999 uma casa, 0.1-99 duas casas,
#' <0.1 tres casas.
#'
#' TODO v0.2: retornar valor + unidade dinamica (g/L, mg/L, ug/L).
#'
#' @param x Numeric escalar em mg/L.
#' @return String.
#' @noRd
ui_fmt_mg_l <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("-")
  x <- as.numeric(x)
  if (x >= 1000)      ui_fmt_br(x, 0L)
  else if (x >= 100)  ui_fmt_br(x, 1L)
  else if (x >= 1)    ui_fmt_br(x, 2L)
  else if (x >= 0.1)  ui_fmt_br(x, 2L)
  else                ui_fmt_br(x, 3L)
}