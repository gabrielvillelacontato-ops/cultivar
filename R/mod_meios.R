#' Modulo Shiny: gestao de meios de cultura
#'
#' Layout em duas colunas: lista (esquerda) + painel de detalhes/edicao (direita).
#'
#' Permissoes:
#'   - operador: apenas visualiza
#'   - supervisor: visualiza, cria, edita
#'   - admin: tudo (incluindo arquivar/restaurar)
#'
#' Composicao (lista de componentes) e read-only no MVP. Edicao via
#' dev/06_editar_composicao.R em emergencia.
#'
#' Limitacoes alinhadas a atualizar_meio (so aceita: nome, codigo_curto,
#' categoria_id, ph_alvo, flag_incerteza, nota_incerteza, bloqueado_preparo):
#'   - Na edicao, referencia/doi/observacoes/pop_id NAO sao editaveis
#'   - Na criacao, todos os campos sao aceitos (criar_meio aceita tudo)
#'
#' Formatacao BR: separador decimal virgula, milhar ponto.
#' Formatacao inteligente por escala (g/L, mg/L, ug/L) ficou como
#' TODO para v0.2 - ver dev/notas_roadmap.md.
#'
#' @noRd

# Pipe operator helper
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------
# Formatacao numerica BR
# ---------------------------------------------------------------------

#' @noRd
.fmt_br <- function(x, decimais = 2L) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("-")
  formatC(as.numeric(x),
          format = "f",
          digits = decimais,
          big.mark = ".",
          decimal.mark = ",")
}

#' Formatacao inteligente da concentracao em mg/L
#'
#' Regra atual (v0.1 - unidade unica):
#'   >= 1000: sem casas decimais (ex: "20.000")
#'   >= 100:  1 casa decimal    (ex: "150,0")
#'   >= 1:    2 casas decimais  (ex: "5,75")
#'   >= 0.1:  2 casas decimais  (ex: "0,25")
#'   < 0.1:   3 casas decimais  (ex: "0,020")
#'
#' TODO v0.2: retornar valor + unidade dinamica (g/L, mg/L, ug/L).
#'
#' @noRd
.formatar_mg_l <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return("-")
  x <- as.numeric(x)
  if (x >= 1000)      .fmt_br(x, 0L)
  else if (x >= 100)  .fmt_br(x, 1L)
  else if (x >= 1)    .fmt_br(x, 2L)
  else if (x >= 0.1)  .fmt_br(x, 2L)
  else                .fmt_br(x, 3L)
}

# ---------------------------------------------------------------------
# Validadores puros (retornam NULL se OK, string se erro)
# ---------------------------------------------------------------------

#' @noRd
.err_nome <- function(nome) {
  if (is.null(nome) || is.na(nome) || !nzchar(trimws(as.character(nome)))) {
    return("Nome obrigatorio.")
  }
  if (nchar(trimws(as.character(nome))) > 200L) {
    return("Ate 200 caracteres.")
  }
  NULL
}

#' @noRd
.err_codigo <- function(codigo, obrigatorio = TRUE) {
  vazio <- is.null(codigo) || is.na(codigo) ||
    !nzchar(trimws(as.character(codigo)))
  if (vazio) {
    if (obrigatorio) return("Codigo obrigatorio.")
    return(NULL)
  }
  codigo <- trimws(as.character(codigo))
  if (!grepl("^[A-Za-z0-9_]+$", codigo)) {
    return("So aceita letras, numeros e underscore.")
  }
  if (nchar(codigo) > 20L) {
    return("Ate 20 caracteres.")
  }
  NULL
}

#' @noRd
.err_ph <- function(ph) {
  if (is.null(ph) || is.na(ph)) return(NULL)  # opcional
  ph <- as.numeric(ph)
  if (is.na(ph)) return("Valor invalido.")
  if (ph < 0 || ph > 14) return("Deve estar entre 0 e 14.")
  NULL
}

#' @noRd
.err_nota <- function(nota, flag, bloq) {
  if (isTRUE(flag == 1L) || isTRUE(bloq == 1L)) {
    if (is.null(nota) || is.na(nota) ||
        !nzchar(trimws(as.character(nota)))) {
      return("Obrigatoria quando incerteza ou bloqueio ativos.")
    }
  }
  NULL
}

# ---------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------

#' @noRd
mod_meios_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    shiny::column(
      width = 8L,
      shiny::div(
        class = "cultivar-card",
        style = "padding: 16px; margin-bottom: 12px;",
        shiny::fluidRow(
          shiny::column(
            width = 5L,
            shiny::textInput(
              ns("termo_busca"),
              label = NULL,
              placeholder = "Buscar por nome ou codigo..."
            )
          ),
          shiny::column(
            width = 4L,
            shiny::uiOutput(ns("ui_filtro_categoria"))
          ),
          shiny::column(
            width = 3L,
            shiny::checkboxInput(
              ns("incluir_arquivados"),
              label = "Mostrar arquivados",
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
        id = ns("painel_lateral"),
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
mod_meios_server <- function(id, con, sessao_auth) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Estado ----
    selected_id <- shiny::reactiveVal(NULL)
    panel_state <- shiny::reactiveVal("vazio")
    reload_trigger <- shiny::reactiveVal(0L)
    mensagem_painel <- shiny::reactiveVal(NULL)  # so para success/erro do backend
    
    # ---- Permissoes ----
    papel_atual <- shiny::reactive({
      s <- sessao_auth()
      if (is.null(s)) NA_character_ else s$papel[1]
    })
    pode_editar <- shiny::reactive(
      isTRUE(papel_atual() %in% c("supervisor", "admin"))
    )
    pode_arquivar <- shiny::reactive(isTRUE(papel_atual() == "admin"))
    op_atual_id <- shiny::reactive({
      s <- sessao_auth()
      if (is.null(s)) NA_integer_ else as.integer(s$id[1])
    })
    
    # ---- Categorias ----
    categorias <- shiny::reactive({
      shiny::req(con)
      tryCatch(
        listar_categorias_meio(con),
        error = function(e) data.frame(id = integer(0), nome = character(0))
      )
    })
    
    output$ui_filtro_categoria <- shiny::renderUI({
      cats <- categorias()
      choices <- c("Todas" = "")
      if (nrow(cats) > 0L) {
        ids <- as.character(cats$id)
        names(ids) <- cats$nome
        choices <- c(choices, ids)
      }
      shiny::selectInput(
        ns("filtro_categoria"),
        label = NULL,
        choices = choices,
        selected = ""
      )
    })
    
    output$ui_btn_novo <- shiny::renderUI({
      if (!isTRUE(pode_editar())) return(NULL)
      shiny::actionButton(
        ns("btn_novo"),
        label = "+ Novo meio",
        class = "btn-primary",
        style = "margin-top: 8px;"
      )
    })
    
    # ---- Lista ----
    meios_filtrados <- shiny::reactive({
      reload_trigger()
      termo <- input$termo_busca %||% ""
      incluir_arq <- isTRUE(input$incluir_arquivados)
      cat_filtro <- input$filtro_categoria
      
      df <- tryCatch(
        buscar_meios(
          con,
          termo = termo,
          incluir_arquivados = incluir_arq
        ),
        error = function(e) {
          mensagem_painel(list(
            tipo = "danger",
            texto = paste("Erro ao listar meios:", conditionMessage(e))
          ))
          NULL
        }
      )
      if (is.null(df) || nrow(df) == 0L) return(df)
      
      if (!is.null(cat_filtro) && nzchar(cat_filtro)) {
        cid <- suppressWarnings(as.integer(cat_filtro))
        if (!is.na(cid)) {
          df <- df[df$categoria_id == cid, , drop = FALSE]
        }
      }
      df
    })
    
    output$tabela <- DT::renderDataTable({
      df <- meios_filtrados()
      if (is.null(df) || nrow(df) == 0L) {
        return(DT::datatable(
          data.frame(Aviso = "Nenhum meio encontrado."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      
      colunas <- c("codigo_curto", "nome", "categoria_nome")
      colunas <- intersect(colunas, names(df))
      df_view <- df[, colunas, drop = FALSE]
      
      if ("deleted_at" %in% names(df)) {
        df_view$Status <- ifelse(
          is.na(df$deleted_at), "Ativo", "Arquivado"
        )
      }
      
      headers <- c(
        codigo_curto = "Codigo",
        nome = "Nome",
        categoria_nome = "Categoria",
        Status = "Status"
      )
      names(df_view) <- headers[names(df_view)]
      
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
            info = "_TOTAL_ registro(s)",
            infoEmpty = "0 registros",
            infoFiltered = "(filtrados de _MAX_)"
          )
        )
      )
    })
    
    # ---- Selecao ----
    shiny::observeEvent(input$tabela_rows_selected, {
      idx <- input$tabela_rows_selected
      df <- meios_filtrados()
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
      if (!isTRUE(pode_editar())) return()
      selected_id(NULL)
      panel_state("criando")
      mensagem_painel(NULL)
    })
    
    shiny::observeEvent(input$btn_editar, {
      if (!isTRUE(pode_editar()) || is.null(selected_id())) return()
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
    
    # ---- Arquivar ----
    shiny::observeEvent(input$btn_arquivar, {
      if (!isTRUE(pode_arquivar()) || is.null(selected_id())) return()
      shiny::showModal(shiny::modalDialog(
        title = "Confirmar arquivamento",
        "Esta acao arquiva o meio. Voce podera restaura-lo depois.",
        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(
            ns("btn_arquivar_confirmar"),
            "Arquivar",
            class = "btn-danger"
          )
        )
      ))
    })
    
    shiny::observeEvent(input$btn_arquivar_confirmar, {
      shiny::removeModal()
      tryCatch({
        arquivar_meio(con, selected_id(), op_atual_id())
        mensagem_painel(list(tipo = "success", texto = "Meio arquivado."))
        reload_trigger(reload_trigger() + 1L)
        selected_id(NULL)
        panel_state("vazio")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Restaurar ----
    shiny::observeEvent(input$btn_restaurar, {
      if (!isTRUE(pode_arquivar()) || is.null(selected_id())) return()
      tryCatch({
        restaurar_meio(con, selected_id(), op_atual_id())
        mensagem_painel(list(tipo = "success", texto = "Meio restaurado."))
        reload_trigger(reload_trigger() + 1L)
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Erros reativos em tempo real (edicao) ----
    output$err_nome_edit <- shiny::renderUI({
      e <- .err_nome(input$nome_edit)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_codigo_edit <- shiny::renderUI({
      e <- .err_codigo(input$codigo_curto_edit, obrigatorio = FALSE)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_ph_edit <- shiny::renderUI({
      e <- .err_ph(input$ph_alvo_edit)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_nota_edit <- shiny::renderUI({
      flag <- if (isTRUE(input$flag_incerteza_edit)) 1L else 0L
      bloq <- if (isTRUE(input$bloqueado_preparo_edit)) 1L else 0L
      e <- .err_nota(input$nota_incerteza_edit, flag, bloq)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    # ---- Erros reativos em tempo real (criacao) ----
    output$err_nome_novo <- shiny::renderUI({
      e <- .err_nome(input$nome_novo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_codigo_novo <- shiny::renderUI({
      e <- .err_codigo(input$codigo_curto_novo, obrigatorio = TRUE)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_ph_novo <- shiny::renderUI({
      e <- .err_ph(input$ph_alvo_novo)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    output$err_nota_novo <- shiny::renderUI({
      flag <- if (isTRUE(input$flag_incerteza_novo)) 1L else 0L
      bloq <- if (isTRUE(input$bloqueado_preparo_novo)) 1L else 0L
      e <- .err_nota(input$nota_incerteza_novo, flag, bloq)
      if (is.null(e)) NULL else shiny::div(class = "text-danger small", e)
    })
    
    # ---- Verificacao de erros de formulario (para bloquear submit) ----
    .form_tem_erro <- function(sufixo, obrigatorio_codigo) {
      erros <- c(
        .err_nome(input[[paste0("nome", sufixo)]]),
        .err_codigo(input[[paste0("codigo_curto", sufixo)]],
                    obrigatorio = obrigatorio_codigo),
        .err_ph(input[[paste0("ph_alvo", sufixo)]]),
        {
          flag <- if (isTRUE(input[[paste0("flag_incerteza", sufixo)]])) 1L else 0L
          bloq <- if (isTRUE(input[[paste0("bloqueado_preparo", sufixo)]])) 1L else 0L
          .err_nota(input[[paste0("nota_incerteza", sufixo)]], flag, bloq)
        }
      )
      length(erros) > 0L
    }
    
    # ---- Salvar edicao ----
    shiny::observeEvent(input$btn_salvar_edicao, {
      if (!isTRUE(pode_editar()) || is.null(selected_id())) return()
      if (.form_tem_erro("_edit", obrigatorio_codigo = FALSE)) {
        mensagem_painel(list(
          tipo = "warning",
          texto = "Corrija os campos destacados antes de salvar."
        ))
        return()
      }
      campos <- .coletar_campos_form(input, sufixo = "_edit", modo = "edit")
      tryCatch({
        atualizar_meio(con, selected_id(), campos, op_atual_id())
        mensagem_painel(list(tipo = "success", texto = "Meio atualizado."))
        reload_trigger(reload_trigger() + 1L)
        panel_state("visualizando")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Salvar novo ----
    shiny::observeEvent(input$btn_salvar_novo, {
      if (!isTRUE(pode_editar())) return()
      if (.form_tem_erro("_novo", obrigatorio_codigo = TRUE)) {
        mensagem_painel(list(
          tipo = "warning",
          texto = "Corrija os campos destacados antes de criar."
        ))
        return()
      }
      campos <- .coletar_campos_form(input, sufixo = "_novo", modo = "novo")
      tryCatch({
        novo_id <- criar_meio(
          con,
          nome = campos$nome,
          codigo_curto = campos$codigo_curto,
          categoria_id = campos$categoria_id,
          criado_por_id = op_atual_id(),
          referencia = campos$referencia %||% NA_character_,
          doi = campos$doi %||% NA_character_,
          ph_alvo = campos$ph_alvo %||% NA_real_,
          observacoes = campos$observacoes %||% NA_character_,
          flag_incerteza = campos$flag_incerteza %||% 0L,
          nota_incerteza = campos$nota_incerteza %||% NA_character_,
          bloqueado_preparo = campos$bloqueado_preparo %||% 0L
        )
        mensagem_painel(list(tipo = "success", texto = "Meio criado."))
        reload_trigger(reload_trigger() + 1L)
        selected_id(as.integer(novo_id))
        panel_state("visualizando")
      }, error = function(e) {
        mensagem_painel(list(
          tipo = "danger",
          texto = paste("Erro ao criar meio:", conditionMessage(e))
        ))
      })
    })
    
    # ---- Renderiza painel ----
    output$painel <- shiny::renderUI({
      state <- panel_state()
      msg <- mensagem_painel()
      
      conteudo <- switch(
        state,
        "vazio" = .painel_vazio(),
        "visualizando" = .painel_visualizando(
          ns, selected_id(), con, pode_editar(), pode_arquivar()
        ),
        "editando" = .painel_editando(
          ns, selected_id(), con, categorias()
        ),
        "criando" = .painel_criando(
          ns, categorias()
        ),
        .painel_vazio()
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
.painel_vazio <- function() {
  shiny::div(
    style = "color: #888; text-align: center; padding: 60px 20px;",
    shiny::tags$p(
      shiny::tags$strong("Selecione um meio"),
      shiny::br(),
      "para ver os detalhes aqui."
    )
  )
}

#' @noRd
.painel_visualizando <- function(ns, meio_id, con, pode_editar, pode_arquivar) {
  if (is.null(meio_id)) return(.painel_vazio())
  det <- tryCatch(
    detalhe_meio(con, meio_id),
    error = function(e) NULL
  )
  if (is.null(det)) {
    return(shiny::div("Erro ao carregar meio."))
  }
  m <- det$meio
  comp <- det$composicao
  arquivado <- !is.na(m$deleted_at[1])
  
  badges <- shiny::tagList()
  if (isTRUE(m$flag_incerteza[1] == 1L)) {
    badges <- shiny::tagList(badges, ui_badge_incerteza(1L), " ")
  }
  if (isTRUE(m$bloqueado_preparo[1] == 1L)) {
    badges <- shiny::tagList(badges, ui_badge_bloqueado(1L), " ")
  }
  if (arquivado) {
    badges <- shiny::tagList(
      badges,
      ui_badge(texto = "Arquivado", tipo = "secondary"), " "
    )
  }
  
  botoes <- shiny::tagList()
  if (pode_editar && !arquivado) {
    botoes <- shiny::tagList(
      botoes,
      shiny::actionButton(ns("btn_editar"), "Editar",
                          class = "btn-primary"),
      " "
    )
  }
  if (pode_arquivar) {
    if (arquivado) {
      botoes <- shiny::tagList(
        botoes,
        shiny::actionButton(ns("btn_restaurar"), "Restaurar",
                            class = "btn-success")
      )
    } else {
      botoes <- shiny::tagList(
        botoes,
        shiny::actionButton(ns("btn_arquivar"), "Arquivar",
                            class = "btn-outline-danger")
      )
    }
  }
  
  if (!is.null(comp) && nrow(comp) > 0L) {
    linhas_comp <- lapply(seq_len(nrow(comp)), function(i) {
      shiny::tags$tr(
        shiny::tags$td(comp$nome[i]),
        shiny::tags$td(
          style = "text-align: right; font-variant-numeric: tabular-nums;",
          .formatar_mg_l(comp$concentracao_mg_l[i])
        )
      )
    })
    tabela_comp <- shiny::tags$table(
      class = "table table-sm",
      style = "margin-top: 8px;",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th("Componente"),
          shiny::tags$th(style = "text-align: right;", "mg/L")
        )
      ),
      shiny::tags$tbody(linhas_comp)
    )
  } else {
    tabela_comp <- shiny::div(
      style = "color: #888; font-style: italic;",
      "Sem composicao cadastrada."
    )
  }
  
  shiny::tagList(
    shiny::h4(m$nome[1]),
    shiny::div(badges),
    ui_vspace(),
    shiny::tags$dl(
      shiny::tags$dt("Codigo"),
      shiny::tags$dd(m$codigo_curto[1]),
      shiny::tags$dt("Categoria"),
      shiny::tags$dd(m$categoria_nome[1]),
      if (!is.na(m$ph_alvo[1])) shiny::tagList(
        shiny::tags$dt("pH alvo"),
        shiny::tags$dd(.fmt_br(m$ph_alvo[1], 2L))
      ),
      if (!is.na(m$nota_incerteza[1])) shiny::tagList(
        shiny::tags$dt("Nota de incerteza/bloqueio"),
        shiny::tags$dd(m$nota_incerteza[1])
      )
    ),
    shiny::h5("Composicao", style = "margin-top: 16px;"),
    shiny::div(
      style = "font-size: 11px; color: #888; margin-bottom: 4px;",
      "Edicao de composicao via script administrativo."
    ),
    tabela_comp,
    ui_vspace(),
    botoes
  )
}

#' @noRd
.painel_editando <- function(ns, meio_id, con, cats_df) {
  det <- tryCatch(detalhe_meio(con, meio_id), error = function(e) NULL)
  if (is.null(det)) return(shiny::div("Erro ao carregar meio."))
  m <- det$meio
  
  choices_cat <- if (nrow(cats_df) > 0L) {
    ids <- as.character(cats_df$id)
    names(ids) <- cats_df$nome
    ids
  } else {
    character(0)
  }
  
  shiny::tagList(
    shiny::h4("Editar meio"),
    shiny::div(
      class = "alert alert-info",
      style = "font-size: 12px; padding: 8px;",
      "Referencia, DOI e observacoes nao sao editaveis nesta versao."
    ),
    
    shiny::textInput(ns("nome_edit"), "Nome", value = m$nome[1]),
    shiny::uiOutput(ns("err_nome_edit")),
    
    shiny::textInput(ns("codigo_curto_edit"), "Codigo curto",
                     value = m$codigo_curto[1]),
    shiny::uiOutput(ns("err_codigo_edit")),
    
    shiny::selectInput(ns("categoria_id_edit"), "Categoria",
                       choices = choices_cat,
                       selected = as.character(m$categoria_id[1])),
    
    shiny::numericInput(ns("ph_alvo_edit"), "pH alvo (0-14)",
                        value = if (is.na(m$ph_alvo[1])) NA_real_ else m$ph_alvo[1],
                        min = 0, max = 14, step = 0.1),
    shiny::uiOutput(ns("err_ph_edit")),
    
    shiny::checkboxInput(ns("flag_incerteza_edit"), "Marcar incerteza",
                         value = isTRUE(m$flag_incerteza[1] == 1L)),
    shiny::checkboxInput(ns("bloqueado_preparo_edit"), "Bloquear para preparo",
                         value = isTRUE(m$bloqueado_preparo[1] == 1L)),
    shiny::textAreaInput(ns("nota_incerteza_edit"),
                         "Nota (obrigatoria se incerteza ou bloqueio)",
                         value = if (is.na(m$nota_incerteza[1])) "" else m$nota_incerteza[1],
                         rows = 2),
    shiny::uiOutput(ns("err_nota_edit")),
    
    ui_vspace(),
    shiny::actionButton(ns("btn_salvar_edicao"), "Salvar",
                        class = "btn-primary"),
    " ",
    shiny::actionButton(ns("btn_cancelar"), "Cancelar")
  )
}

#' @noRd
.painel_criando <- function(ns, cats_df) {
  choices_cat <- if (nrow(cats_df) > 0L) {
    ids <- as.character(cats_df$id)
    names(ids) <- cats_df$nome
    ids
  } else {
    character(0)
  }
  
  shiny::tagList(
    shiny::h4("Novo meio"),
    
    shiny::textInput(ns("nome_novo"), "Nome*"),
    shiny::uiOutput(ns("err_nome_novo")),
    
    shiny::textInput(ns("codigo_curto_novo"), "Codigo curto*",
                     placeholder = "Ex: MSO, P1AS"),
    shiny::uiOutput(ns("err_codigo_novo")),
    
    shiny::selectInput(ns("categoria_id_novo"), "Categoria*",
                       choices = choices_cat),
    
    shiny::textInput(ns("referencia_novo"), "Referencia"),
    shiny::textInput(ns("doi_novo"), "DOI"),
    shiny::numericInput(ns("ph_alvo_novo"), "pH alvo (0-14)",
                        value = NA_real_, min = 0, max = 14, step = 0.1),
    shiny::uiOutput(ns("err_ph_novo")),
    
    shiny::textAreaInput(ns("observacoes_novo"), "Observacoes", rows = 2),
    
    shiny::checkboxInput(ns("flag_incerteza_novo"), "Marcar incerteza",
                         value = FALSE),
    shiny::checkboxInput(ns("bloqueado_preparo_novo"), "Bloquear para preparo",
                         value = FALSE),
    shiny::textAreaInput(ns("nota_incerteza_novo"),
                         "Nota (obrigatoria se incerteza ou bloqueio)",
                         rows = 2),
    shiny::uiOutput(ns("err_nota_novo")),
    
    ui_vspace(),
    shiny::actionButton(ns("btn_salvar_novo"), "Criar",
                        class = "btn-primary"),
    " ",
    shiny::actionButton(ns("btn_cancelar"), "Cancelar")
  )
}

# ---------------------------------------------------------------------
# Helpers de formulario
# ---------------------------------------------------------------------

#' @noRd
.coletar_campos_form <- function(input, sufixo, modo) {
  nz <- function(x) {
    if (is.null(x) || !nzchar(trimws(as.character(x)))) NA_character_
    else trimws(as.character(x))
  }
  num <- function(x) if (is.null(x) || is.na(x)) NA_real_ else as.numeric(x)
  bool <- function(x) if (isTRUE(x)) 1L else 0L
  
  campos <- list(
    nome = nz(input[[paste0("nome", sufixo)]]),
    codigo_curto = nz(input[[paste0("codigo_curto", sufixo)]]),
    categoria_id = suppressWarnings(as.integer(
      input[[paste0("categoria_id", sufixo)]]
    )),
    ph_alvo = num(input[[paste0("ph_alvo", sufixo)]]),
    flag_incerteza = bool(input[[paste0("flag_incerteza", sufixo)]]),
    nota_incerteza = nz(input[[paste0("nota_incerteza", sufixo)]]),
    bloqueado_preparo = bool(input[[paste0("bloqueado_preparo", sufixo)]])
  )
  
  if (modo == "novo") {
    campos$referencia <- nz(input[[paste0("referencia", sufixo)]])
    campos$doi <- nz(input[[paste0("doi", sufixo)]])
    campos$observacoes <- nz(input[[paste0("observacoes", sufixo)]])
  }
  
  if (modo == "edit") {
    campos <- campos[!vapply(campos, function(x) {
      length(x) == 0L || (length(x) == 1L && is.na(x))
    }, logical(1))]
  }
  
  campos
}