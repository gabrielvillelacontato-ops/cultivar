#' Seed do catalogo validado do CultivaR
#'
#' Funcoes para popular o banco com o catalogo cientifico validado:
#' categorias, componentes, meios e workflow do algodao S&R 2001.
#'
#' Todas as funcoes sao idempotentes (verificam existencia antes de
#' inserir) e podem ser executadas multiplas vezes sem efeito colateral.
#' Retornam contagem de insercoes REAIS (zero em segunda execucao).
#'
#' @noRd

# ---------------------------------------------------------------------
# Helpers internos
# ---------------------------------------------------------------------

#' Retorna ID de um registro existente ou NA_integer_
#' @noRd
.lookup_id <- function(con, tabela, coluna, valor, tenant_id = NULL) {
  if (is.null(tenant_id)) {
    sql <- sprintf("SELECT id FROM %s WHERE %s = ? LIMIT 1;", tabela, coluna)
    res <- DBI::dbGetQuery(con, sql, params = list(valor))
  } else {
    sql <- sprintf("SELECT id FROM %s WHERE %s = ? AND tenant_id = ? LIMIT 1;",
                   tabela, coluna)
    res <- DBI::dbGetQuery(con, sql, params = list(valor, tenant_id))
  }
  if (nrow(res) == 0L) NA_integer_ else as.integer(res$id[1])
}

#' Insere se nao existir
#'
#' @return list(id = integer, inserido = logical). inserido=FALSE se ja existia.
#' @noRd
.insert_if_missing <- function(con, tabela, cols_valores, chave_unica,
                               tenant_id = NULL) {
  id_existente <- .lookup_id(con, tabela, chave_unica$coluna,
                             chave_unica$valor, tenant_id)
  if (!is.na(id_existente)) {
    return(list(id = id_existente, inserido = FALSE))
  }
  
  DBI::dbExecute(
    con,
    sprintf("INSERT INTO %s (%s) VALUES (%s);",
            tabela,
            paste(names(cols_valores), collapse = ", "),
            paste(rep("?", length(cols_valores)), collapse = ", ")),
    params = unname(cols_valores)
  )
  novo_id <- .lookup_id(con, tabela, chave_unica$coluna,
                        chave_unica$valor, tenant_id)
  list(id = novo_id, inserido = TRUE)
}

# ---------------------------------------------------------------------
# Seed 1: categorias de componente
# ---------------------------------------------------------------------

#' Popula categorias de componente
#'
#' @param con Conexao SQLite.
#' @param tenant_id ID do tenant. Default: TENANT_DEFAULT_ID.
#' @return Numero de categorias INSERIDAS nesta chamada (invisivel).
#' @noRd
seed_categorias_componente <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  categorias <- data.frame(
    nome = c("macronutriente", "micronutriente", "vitamina", "hormonio",
             "carbono", "tampao", "solidificante", "antibiotico",
             "indutor", "aminoacido", "extrato_complexo", "outros"),
    ordem_exibicao = 1:12,
    stringsAsFactors = FALSE
  )
  
  n_inseridos <- 0L
  for (i in seq_len(nrow(categorias))) {
    r <- .insert_if_missing(
      con, "categorias_componente",
      list(tenant_id = tenant_id,
           nome = categorias$nome[i],
           ordem_exibicao = categorias$ordem_exibicao[i]),
      list(coluna = "nome", valor = categorias$nome[i]),
      tenant_id = tenant_id
    )
    if (r$inserido) n_inseridos <- n_inseridos + 1L
  }
  invisible(n_inseridos)
}

# ---------------------------------------------------------------------
# Seed 2: categorias de meio
# ---------------------------------------------------------------------

#' Popula categorias de meio
#' @noRd
seed_categorias_meio <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  categorias <- data.frame(
    nome = c("tecido_vegetal", "microbiologia", "transformacao_genetica"),
    ordem_exibicao = 1:3,
    stringsAsFactors = FALSE
  )
  
  n_inseridos <- 0L
  for (i in seq_len(nrow(categorias))) {
    r <- .insert_if_missing(
      con, "categorias_meio",
      list(tenant_id = tenant_id,
           nome = categorias$nome[i],
           ordem_exibicao = categorias$ordem_exibicao[i]),
      list(coluna = "nome", valor = categorias$nome[i]),
      tenant_id = tenant_id
    )
    if (r$inserido) n_inseridos <- n_inseridos + 1L
  }
  invisible(n_inseridos)
}

# ---------------------------------------------------------------------
# Seed 3: componentes (MW e CAS validados)
# ---------------------------------------------------------------------

#' Popula componentes quimicos
#'
#' Pre-requisito: seed_categorias_componente() ja executada.
#' Massas molares verificadas contra IUPAC/PubChem.
#' Extratos complexos (peptona, triptona, etc.) ficam com massa_molar NULL.
#'
#' @noRd
seed_componentes <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  n_cats <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM categorias_componente WHERE tenant_id = ?;",
    params = list(tenant_id)
  )$n
  if (n_cats == 0L) {
    stop("Categorias de componente nao populadas. ",
         "Rode seed_categorias_componente() antes.", call. = FALSE)
  }
  
  nomes_cat <- c("macronutriente", "micronutriente", "vitamina", "hormonio",
                 "carbono", "tampao", "solidificante", "antibiotico",
                 "indutor", "aminoacido", "extrato_complexo", "outros")
  cat_ids <- setNames(
    vapply(nomes_cat, function(n) {
      id <- .lookup_id(con, "categorias_componente", "nome", n, tenant_id)
      if (is.na(id)) stop("Categoria nao encontrada: '", n, "'.", call. = FALSE)
      id
    }, integer(1)),
    nomes_cat
  )
  
  componentes <- list(
    list("NH4NO3",                  "NH4NO3",         cat_ids[["macronutriente"]],    "6484-52-2",   80.043),
    list("KNO3",                    "KNO3",           cat_ids[["macronutriente"]],    "7757-79-1",   101.103),
    list("KCl",                     "KCl",            cat_ids[["macronutriente"]],    "7447-40-7",   74.551),
    list("KH2PO4",                  "KH2PO4",         cat_ids[["macronutriente"]],    "7778-77-0",   136.086),
    list("CaCl2.2H2O",              "CaCl2.2H2O",     cat_ids[["macronutriente"]],    "10035-04-8",  147.014),
    list("Ca(NO3)2.4H2O",           "Ca(NO3)2.4H2O",  cat_ids[["macronutriente"]],    "13477-34-4",  236.149),
    list("MgSO4.7H2O",              "MgSO4.7H2O",     cat_ids[["macronutriente"]],    "10034-99-8",  246.475),
    list("NaH2PO4.H2O",             "NaH2PO4.H2O",    cat_ids[["macronutriente"]],    "10049-21-5",  137.99),
    list("(NH4)2SO4",               "(NH4)2SO4",      cat_ids[["macronutriente"]],    "7783-20-2",   132.14),
    
    list("FeSO4.7H2O",              "FeSO4.7H2O",     cat_ids[["micronutriente"]],    "7782-63-0",   278.014),
    list("Na2EDTA",                 "Na2EDTA",        cat_ids[["micronutriente"]],    "6381-92-6",   372.24),
    list("H3BO3",                   "H3BO3",          cat_ids[["micronutriente"]],    "10043-35-3",  61.83),
    list("MnSO4.4H2O",              "MnSO4.4H2O",     cat_ids[["micronutriente"]],    "10101-50-5",  223.07),
    list("MnSO4.H2O",               "MnSO4.H2O",      cat_ids[["micronutriente"]],    "10034-96-5",  169.02),
    list("ZnSO4.7H2O",              "ZnSO4.7H2O",     cat_ids[["micronutriente"]],    "7446-20-0",   287.56),
    list("KI",                      "KI",             cat_ids[["micronutriente"]],    "7681-11-0",   166.003),
    list("Na2MoO4.2H2O",             "Na2MoO4.2H2O",  cat_ids[["micronutriente"]],    "10102-40-6",  241.95),
    list("CuSO4.5H2O",              "CuSO4.5H2O",     cat_ids[["micronutriente"]],    "7758-99-8",   249.685),
    list("CuSO4 anidro",            "CuSO4",          cat_ids[["micronutriente"]],    "7758-98-7",   159.609),
    list("CoCl2.6H2O",              "CoCl2.6H2O",     cat_ids[["micronutriente"]],    "7791-13-1",   237.93),
    
    list("Tiamina-HCl",             NA_character_,    cat_ids[["vitamina"]],          "67-03-8",     337.27),
    list("Acido nicotinico",        NA_character_,    cat_ids[["vitamina"]],          "59-67-6",     123.11),
    list("Piridoxina-HCl",          NA_character_,    cat_ids[["vitamina"]],          "58-56-0",     205.64),
    list("Glicina",                 NA_character_,    cat_ids[["aminoacido"]],        "56-40-6",     75.07),
    list("Myo-inositol",            NA_character_,    cat_ids[["vitamina"]],          "87-89-8",     180.16),
    
    list("BAP",                     NA_character_,    cat_ids[["hormonio"]],          "1214-39-7",   225.25),
    list("NAA",                     NA_character_,    cat_ids[["hormonio"]],          "86-87-3",     186.21),
    list("2,4-D",                   NA_character_,    cat_ids[["hormonio"]],          "94-75-7",     221.04),
    list("IAA",                     NA_character_,    cat_ids[["hormonio"]],          "87-51-4",     175.18),
    list("2-iP",                    NA_character_,    cat_ids[["hormonio"]],          "2365-40-4",   203.25),
    list("Cinetina",                NA_character_,    cat_ids[["hormonio"]],          "525-79-1",    215.21),
    list("GA3",                     NA_character_,    cat_ids[["hormonio"]],          "77-06-5",     346.37),
    
    list("Sacarose",                "C12H22O11",      cat_ids[["carbono"]],           "57-50-1",     342.30),
    list("Glicose",                 "C6H12O6",        cat_ids[["carbono"]],           "50-99-7",     180.16),
    
    list("MES",                     NA_character_,    cat_ids[["tampao"]],            "4432-31-9",   195.24),
    list("Tampao fosfato de sodio", NA_character_,    cat_ids[["tampao"]],            NA_character_, NA_real_),
    
    list("Agar",                    NA_character_,    cat_ids[["solidificante"]],     NA_character_, NA_real_),
    list("Phytagel",                NA_character_,    cat_ids[["solidificante"]],     NA_character_, NA_real_),
    
    list("Canamicina sulfato",      NA_character_,    cat_ids[["antibiotico"]],       "25389-94-0",  582.58),
    list("Carbenicilina dissodica", NA_character_,    cat_ids[["antibiotico"]],       "4800-94-6",   422.36),
    list("Cefotaxima sodica",       NA_character_,    cat_ids[["antibiotico"]],       "64485-93-4",  477.45),
    list("Rifampicina",             NA_character_,    cat_ids[["antibiotico"]],       "13292-46-1",  822.94),
    
    list("Acetosiringona",          NA_character_,    cat_ids[["indutor"]],           "2478-38-8",   196.20),
    
    list("Triptona",                NA_character_,    cat_ids[["extrato_complexo"]],  NA_character_, NA_real_),
    list("Extrato de levedura",     NA_character_,    cat_ids[["extrato_complexo"]],  NA_character_, NA_real_),
    list("Extrato de carne",        NA_character_,    cat_ids[["extrato_complexo"]],  NA_character_, NA_real_),
    list("Peptona",                 NA_character_,    cat_ids[["extrato_complexo"]],  NA_character_, NA_real_),
    list("Edamin",                  NA_character_,    cat_ids[["extrato_complexo"]],  NA_character_, NA_real_),
    
    list("NaCl",                    "NaCl",           cat_ids[["outros"]],            "7647-14-5",   58.443),
    list("MgCl2.6H2O",              "MgCl2.6H2O",     cat_ids[["outros"]],            "7791-18-6",   203.30)
  )
  
  n_inseridos <- 0L
  for (comp in componentes) {
    nome      <- comp[[1]]
    formula   <- comp[[2]]
    cat_id_v  <- comp[[3]]
    cas       <- comp[[4]]
    mw        <- comp[[5]]
    
    r <- .insert_if_missing(
      con, "componentes",
      list(tenant_id    = tenant_id,
           categoria_id = cat_id_v,
           nome         = nome,
           formula      = formula,
           cas          = cas,
           massa_molar  = mw),
      list(coluna = "nome", valor = nome),
      tenant_id = tenant_id
    )
    if (r$inserido) n_inseridos <- n_inseridos + 1L
  }
  invisible(n_inseridos)
}

# ---------------------------------------------------------------------
# Helper: insere composicao de meio em meio_componentes
# ---------------------------------------------------------------------

#' Insere a composicao de um meio em meio_componentes
#' @noRd
.insert_composicao <- function(con, meio_id, composicao, comp_ids) {
  n_inseridos <- 0L
  for (i in seq_along(composicao)) {
    item <- composicao[[i]]
    nome_comp <- item[["nome"]]
    comp_id   <- comp_ids[[nome_comp]]
    if (is.null(comp_id) || is.na(comp_id)) {
      stop("Componente nao encontrado no catalogo: '", nome_comp,
           "'. Rode seed_componentes() antes.", call. = FALSE)
    }
    
    existe <- DBI::dbGetQuery(
      con,
      "SELECT id FROM meio_componentes WHERE meio_id = ? AND componente_id = ? LIMIT 1;",
      params = list(meio_id, comp_id)
    )
    if (nrow(existe) > 0L) next
    
    DBI::dbExecute(
      con,
      "INSERT INTO meio_componentes
       (meio_id, componente_id, concentracao_mg_l, valor_original,
        unidade_original, ordem_exibicao, observacao)
       VALUES (?, ?, ?, ?, ?, ?, ?);",
      params = list(
        meio_id,
        comp_id,
        item[["conc_mg_l"]],
        if (is.null(item[["valor_original"]])) NA_real_ else item[["valor_original"]],
        if (is.null(item[["unidade_original"]])) NA_character_ else item[["unidade_original"]],
        i,
        if (is.null(item[["observacao"]])) NA_character_ else item[["observacao"]]
      )
    )
    n_inseridos <- n_inseridos + 1L
  }
  n_inseridos
}

# ---------------------------------------------------------------------
# Seed 4: meios validados (bibliografia primaria)
# ---------------------------------------------------------------------

#' Popula os 11 meios validados com bibliografia primaria
#'
#' Pre-requisito: seed_categorias_meio() e seed_componentes() executados.
#'
#' @return Numero de meios INSERIDOS (invisivel).
#' @noRd
seed_meios_validados <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  n_cats_meio <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM categorias_meio WHERE tenant_id = ?;",
    params = list(tenant_id)
  )$n
  if (n_cats_meio == 0L) {
    stop("Categorias de meio nao populadas. Rode seed_categorias_meio() antes.",
         call. = FALSE)
  }
  
  n_comps <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM componentes WHERE tenant_id = ?;",
    params = list(tenant_id)
  )$n
  if (n_comps == 0L) {
    stop("Componentes nao populados. Rode seed_componentes() antes.",
         call. = FALSE)
  }
  
  cat_meio_id <- function(nome) {
    id <- .lookup_id(con, "categorias_meio", "nome", nome, tenant_id)
    if (is.na(id)) stop("Categoria de meio nao encontrada: '", nome, "'.",
                        call. = FALSE)
    id
  }
  cat_meio_ids <- list(
    tecido_vegetal         = cat_meio_id("tecido_vegetal"),
    microbiologia          = cat_meio_id("microbiologia"),
    transformacao_genetica = cat_meio_id("transformacao_genetica")
  )
  
  todos_comps <- DBI::dbGetQuery(
    con,
    "SELECT id, nome FROM componentes WHERE tenant_id = ?;",
    params = list(tenant_id)
  )
  comp_ids <- setNames(as.integer(todos_comps$id), todos_comps$nome)
  
  meios_meta <- list(
    list(codigo = "MSO",   nome = "MS basal original (1962)",
         categoria = cat_meio_ids$tecido_vegetal,
         referencia = "Murashige & Skoog (1962) Physiol Plant 15:473-497",
         doi = "10.1111/j.1399-3054.1962.tb08052.x",
         ph_alvo = 5.75, flag_inc = 0L, nota_inc = NA_character_,
         obs = "Formula original do paper (basal 1x). Antes da versao revised."),
    list(codigo = "MSR",   nome = "MS Revised (1962)",
         categoria = cat_meio_ids$tecido_vegetal,
         referencia = "Murashige & Skoog (1962) Physiol Plant 15:473-497",
         doi = "10.1111/j.1399-3054.1962.tb08052.x",
         ph_alvo = 5.75, flag_inc = 1L,
         nota_inc = "Tiamina-HCl pode ser 0.1 (MS original) ou 0.4-1.0 (variante L&S 1965).",
         obs = "Formula padrao moderna. N total verificado: 840 mg/L."),
    list(codigo = "B5",    nome = "Gamborg B5 (1968)",
         categoria = cat_meio_ids$tecido_vegetal,
         referencia = "Gamborg, Miller & Ojima (1968) Exp Cell Res 50:151-158",
         doi = "10.1016/0014-4827(68)90403-5",
         ph_alvo = 5.5, flag_inc = 0L, nota_inc = NA_character_,
         obs = "Valores 'finally adopted' do paper. Fonte de Fe: Sequestrene 330 (Geigy) ~= FeSO4.7H2O + Na2EDTA equivalentes."),
    list(codigo = "MS0",   nome = "MS0 - germinacao algodao",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.8, flag_inc = 0L, nota_inc = NA_character_,
         obs = "MS Revised salts sem hormonios. Glicose substitui sacarose."),
    list(codigo = "PIA",   nome = "Pre-inducao Agrobacterium",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.6, flag_inc = 1L,
         nota_inc = "AB salts (Chilton et al. 1974) e tampao fosfato PENDENTES de validacao. Cadastrar apos consulta ao paper original.",
         obs = "Meio liquido. 24h a 28C/200rpm. Acetosiringona induz genes vir."),
    list(codigo = "P1AS",  nome = "P1-AS - co-cultivo algodao",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52, mod. de Firoozabady & DeBoer (1993)",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.8, flag_inc = 0L, nota_inc = NA_character_,
         obs = "Co-cultivo 3 dias 25C (ou 21C). 2-iP como citocinina, glicose, Phytagel."),
    list(codigo = "P1S",   nome = "P1-c4k50 - selecao de calos",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.8, flag_inc = 0L, nota_inc = NA_character_,
         obs = "Igual P1-AS sem acetosiringona. + canamicina 50 + carbenicilina 400."),
    list(codigo = "P7M",   nome = "P7-c4k50 - manutencao de calos",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52, mod. de Firoozabady & DeBoer (1993)",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.8, flag_inc = 0L, nota_inc = NA_character_,
         obs = "Inversao 2-iP/NAA em relacao ao P1: 2-iP 0.1, NAA 5. Subcultura a cada 4 semanas."),
    list(codigo = "MSBOK", nome = "MSBOK - embriogenese somatica",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.8, flag_inc = 0L, nota_inc = NA_character_,
         obs = "B5 vitamins. KNO3 adicional (dobra). Apos 2 subculturas, omite canamicina."),
    list(codigo = "EG3",   nome = "EG3 - germinacao de embrioes",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Firoozabady & DeBoer (1993), citado em S&R 2001",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.9, flag_inc = 0L, nota_inc = NA_character_,
         obs = "MS Revised a 1/2x. Glicose 0.5%. Sem hormonios."),
    list(codigo = "MS3",   nome = "MS3 - sustentacao de plantulas",
         categoria = cat_meio_ids$transformacao_genetica,
         referencia = "Sunilkumar & Rathore (2001) Mol Breeding 8:37-52",
         doi = "10.1023/A:1011906701925",
         ph_alvo = 5.8, flag_inc = 0L, nota_inc = NA_character_,
         obs = "MS Revised a 1/2x. Solidificante composto: Phytagel 0.08% + Agar 0.4%.")
  )
  
  composicoes <- list()
  
  composicao_ms_revised_salts <- list(
    list(nome = "NH4NO3",        conc_mg_l = 1650),
    list(nome = "KNO3",          conc_mg_l = 1900),
    list(nome = "CaCl2.2H2O",    conc_mg_l = 440),
    list(nome = "MgSO4.7H2O",    conc_mg_l = 370),
    list(nome = "KH2PO4",        conc_mg_l = 170),
    list(nome = "FeSO4.7H2O",    conc_mg_l = 27.8),
    list(nome = "Na2EDTA",       conc_mg_l = 37.3),
    list(nome = "H3BO3",         conc_mg_l = 6.2),
    list(nome = "MnSO4.4H2O",    conc_mg_l = 22.3),
    list(nome = "ZnSO4.7H2O",    conc_mg_l = 8.6),
    list(nome = "KI",            conc_mg_l = 0.83),
    list(nome = "Na2MoO4.2H2O",  conc_mg_l = 0.25),
    list(nome = "CuSO4.5H2O",    conc_mg_l = 0.025),
    list(nome = "CoCl2.6H2O",    conc_mg_l = 0.025)
  )
  
  composicoes[["MSO"]] <- list(
    list(nome = "NH4NO3",        conc_mg_l = 400),
    list(nome = "KCl",           conc_mg_l = 65),
    list(nome = "KNO3",          conc_mg_l = 80),
    list(nome = "KH2PO4",        conc_mg_l = 12.5),
    list(nome = "Ca(NO3)2.4H2O", conc_mg_l = 144),
    list(nome = "MgSO4.7H2O",    conc_mg_l = 72),
    list(nome = "H3BO3",         conc_mg_l = 1.6),
    list(nome = "MnSO4.4H2O",    conc_mg_l = 6.5),
    list(nome = "ZnSO4.7H2O",    conc_mg_l = 2.7),
    list(nome = "KI",            conc_mg_l = 0.75),
    list(nome = "IAA",           conc_mg_l = 2.0),
    list(nome = "Cinetina",      conc_mg_l = 0.2,
         observacao = "0.2 (fresh pith) ou 0.04 (callus)"),
    list(nome = "Tiamina-HCl",   conc_mg_l = 0.1),
    list(nome = "Acido nicotinico", conc_mg_l = 0.5),
    list(nome = "Piridoxina-HCl", conc_mg_l = 0.5),
    list(nome = "Glicina",       conc_mg_l = 2.0),
    list(nome = "Myo-inositol",  conc_mg_l = 100),
    list(nome = "Edamin",        conc_mg_l = 1000,
         observacao = "Hidrolisado de caseina, componente do basal original"),
    list(nome = "Sacarose",      conc_mg_l = 20000),
    list(nome = "Agar",          conc_mg_l = 10000)
  )
  
  composicoes[["MSR"]] <- c(
    composicao_ms_revised_salts,
    list(
      list(nome = "Tiamina-HCl",      conc_mg_l = 0.1,
           observacao = "Variante L&S 1965 usa 0.4-1.0; cadastrar como meio separado se necessario"),
      list(nome = "Acido nicotinico", conc_mg_l = 0.5),
      list(nome = "Piridoxina-HCl",   conc_mg_l = 0.5),
      list(nome = "Glicina",          conc_mg_l = 2.0),
      list(nome = "Myo-inositol",     conc_mg_l = 100),
      list(nome = "Sacarose",         conc_mg_l = 30000,
           observacao = "Versao moderna usa 30 g/L (original era 20 g/L)"),
      list(nome = "Agar",             conc_mg_l = 8000,
           observacao = "Alternativa: Phytagel 2 g/L")
    )
  )
  
  composicoes[["B5"]] <- list(
    list(nome = "NaH2PO4.H2O",   conc_mg_l = 150),
    list(nome = "KNO3",          conc_mg_l = 2500,
         observacao = "'Finally adopted' 25 mM (tabela traz 3000 / 30 mM)"),
    list(nome = "(NH4)2SO4",     conc_mg_l = 134),
    list(nome = "MgSO4.7H2O",    conc_mg_l = 250,
         observacao = "'Finally adopted' 1 mM (tabela traz 500 / 2 mM)"),
    list(nome = "CaCl2.2H2O",    conc_mg_l = 150),
    list(nome = "FeSO4.7H2O",    conc_mg_l = 27.8,
         observacao = "Paper usa Sequestrene 330 Fe 28 mg/L; aqui Fe-EDTA equivalente"),
    list(nome = "Na2EDTA",       conc_mg_l = 37.3,
         observacao = "Acompanha FeSO4 como quelante (equivalente Sequestrene)"),
    list(nome = "MnSO4.H2O",     conc_mg_l = 10),
    list(nome = "H3BO3",         conc_mg_l = 3),
    list(nome = "ZnSO4.7H2O",    conc_mg_l = 2),
    list(nome = "Na2MoO4.2H2O",  conc_mg_l = 0.25),
    list(nome = "CuSO4 anidro",  conc_mg_l = 0.025),
    list(nome = "CoCl2.6H2O",    conc_mg_l = 0.025),
    list(nome = "KI",            conc_mg_l = 0.75),
    list(nome = "Tiamina-HCl",   conc_mg_l = 10,
         observacao = "10x maior que MS - caracteristica do B5"),
    list(nome = "Acido nicotinico", conc_mg_l = 1),
    list(nome = "Piridoxina-HCl", conc_mg_l = 1),
    list(nome = "Myo-inositol",  conc_mg_l = 100),
    list(nome = "Sacarose",      conc_mg_l = 20000),
    list(nome = "2,4-D",         conc_mg_l = 2)
  )
  
  composicoes[["MS0"]] <- c(
    composicao_ms_revised_salts,
    list(
      list(nome = "Glicose",  conc_mg_l = 20000,
           valor_original = 2, unidade_original = "%"),
      list(nome = "Phytagel", conc_mg_l = 2000,
           valor_original = 0.2, unidade_original = "%")
    )
  )
  
  composicoes[["PIA"]] <- list(
    list(nome = "Glicose",        conc_mg_l = 10000,
         valor_original = 1, unidade_original = "%"),
    list(nome = "MES",            conc_mg_l = 1463,
         valor_original = 7.5, unidade_original = "mM"),
    list(nome = "Acetosiringona", conc_mg_l = 19.62,
         valor_original = 100, unidade_original = "uM",
         observacao = "100 uM = 19.62 mg/L (MW 196.20)")
  )
  
  composicoes[["P1AS"]] <- c(
    composicao_ms_revised_salts,
    list(
      list(nome = "Myo-inositol",   conc_mg_l = 100),
      list(nome = "Tiamina-HCl",    conc_mg_l = 0.4,
           observacao = "Variante L&S 1965"),
      list(nome = "2-iP",           conc_mg_l = 5),
      list(nome = "NAA",            conc_mg_l = 0.1),
      list(nome = "Glicose",        conc_mg_l = 30000,
           valor_original = 3, unidade_original = "%"),
      list(nome = "MgCl2.6H2O",     conc_mg_l = 1000),
      list(nome = "Acetosiringona", conc_mg_l = 19.62,
           valor_original = 100, unidade_original = "uM"),
      list(nome = "Phytagel",       conc_mg_l = 2000,
           valor_original = 0.2, unidade_original = "%")
    )
  )
  
  composicoes[["P1S"]] <- c(
    composicao_ms_revised_salts,
    list(
      list(nome = "Myo-inositol",      conc_mg_l = 100),
      list(nome = "Tiamina-HCl",       conc_mg_l = 0.4),
      list(nome = "2-iP",              conc_mg_l = 5),
      list(nome = "NAA",               conc_mg_l = 0.1),
      list(nome = "Glicose",           conc_mg_l = 30000),
      list(nome = "MgCl2.6H2O",        conc_mg_l = 1000),
      list(nome = "Canamicina sulfato",       conc_mg_l = 50),
      list(nome = "Carbenicilina dissodica",  conc_mg_l = 400),
      list(nome = "Phytagel",          conc_mg_l = 2000)
    )
  )
  
  composicoes[["P7M"]] <- c(
    composicao_ms_revised_salts,
    list(
      list(nome = "Myo-inositol",      conc_mg_l = 100),
      list(nome = "Tiamina-HCl",       conc_mg_l = 0.4),
      list(nome = "2-iP",              conc_mg_l = 0.1,
           observacao = "Reduzido em relacao ao P1"),
      list(nome = "NAA",               conc_mg_l = 5,
           observacao = "Aumentado - inversao"),
      list(nome = "Glicose",           conc_mg_l = 30000),
      list(nome = "MgCl2.6H2O",        conc_mg_l = 1000),
      list(nome = "Canamicina sulfato",       conc_mg_l = 50),
      list(nome = "Carbenicilina dissodica",  conc_mg_l = 400),
      list(nome = "Phytagel",          conc_mg_l = 2000)
    )
  )
  
  composicoes[["MSBOK"]] <- list(
    list(nome = "NH4NO3",        conc_mg_l = 1650),
    list(nome = "KNO3",          conc_mg_l = 3800,
         observacao = "Dobrado em relacao ao MS Revised (1900 + 1900 adicional)"),
    list(nome = "CaCl2.2H2O",    conc_mg_l = 440),
    list(nome = "MgSO4.7H2O",    conc_mg_l = 370),
    list(nome = "KH2PO4",        conc_mg_l = 170),
    list(nome = "FeSO4.7H2O",    conc_mg_l = 27.8),
    list(nome = "Na2EDTA",       conc_mg_l = 37.3),
    list(nome = "H3BO3",         conc_mg_l = 6.2),
    list(nome = "MnSO4.4H2O",    conc_mg_l = 22.3),
    list(nome = "ZnSO4.7H2O",    conc_mg_l = 8.6),
    list(nome = "KI",            conc_mg_l = 0.83),
    list(nome = "Na2MoO4.2H2O",  conc_mg_l = 0.25),
    list(nome = "CuSO4.5H2O",    conc_mg_l = 0.025),
    list(nome = "CoCl2.6H2O",    conc_mg_l = 0.025),
    list(nome = "Myo-inositol",  conc_mg_l = 100),
    list(nome = "Tiamina-HCl",   conc_mg_l = 10,
         observacao = "B5 vitamins (Gamborg 1968)"),
    list(nome = "Acido nicotinico", conc_mg_l = 1),
    list(nome = "Piridoxina-HCl", conc_mg_l = 1),
    list(nome = "Glicose",       conc_mg_l = 30000),
    list(nome = "MgCl2.6H2O",    conc_mg_l = 1000),
    list(nome = "Canamicina sulfato",      conc_mg_l = 25,
         observacao = "Reduzida; apos 2 subculturas, omitir"),
    list(nome = "Carbenicilina dissodica", conc_mg_l = 200,
         observacao = "Reduzida em relacao ao P1/P7"),
    list(nome = "Phytagel",      conc_mg_l = 2000)
  )
  
  composicoes[["EG3"]] <- list(
    list(nome = "NH4NO3",        conc_mg_l = 825,
         observacao = "MS Revised salts a 1/2x"),
    list(nome = "KNO3",          conc_mg_l = 950),
    list(nome = "CaCl2.2H2O",    conc_mg_l = 220),
    list(nome = "MgSO4.7H2O",    conc_mg_l = 185),
    list(nome = "KH2PO4",        conc_mg_l = 85),
    list(nome = "FeSO4.7H2O",    conc_mg_l = 13.9),
    list(nome = "Na2EDTA",       conc_mg_l = 18.65),
    list(nome = "H3BO3",         conc_mg_l = 3.1),
    list(nome = "MnSO4.4H2O",    conc_mg_l = 11.15),
    list(nome = "ZnSO4.7H2O",    conc_mg_l = 4.3),
    list(nome = "KI",            conc_mg_l = 0.415),
    list(nome = "Na2MoO4.2H2O",  conc_mg_l = 0.125),
    list(nome = "CuSO4.5H2O",    conc_mg_l = 0.0125),
    list(nome = "CoCl2.6H2O",    conc_mg_l = 0.0125),
    list(nome = "Glicose",       conc_mg_l = 5000,
         valor_original = 0.5, unidade_original = "%"),
    list(nome = "Myo-inositol",  conc_mg_l = 100),
    list(nome = "Tiamina-HCl",   conc_mg_l = 0.4),
    list(nome = "Phytagel",      conc_mg_l = 2000)
  )
  
  composicoes[["MS3"]] <- list(
    list(nome = "NH4NO3",        conc_mg_l = 825),
    list(nome = "KNO3",          conc_mg_l = 950),
    list(nome = "CaCl2.2H2O",    conc_mg_l = 220),
    list(nome = "MgSO4.7H2O",    conc_mg_l = 185),
    list(nome = "KH2PO4",        conc_mg_l = 85),
    list(nome = "FeSO4.7H2O",    conc_mg_l = 13.9),
    list(nome = "Na2EDTA",       conc_mg_l = 18.65),
    list(nome = "H3BO3",         conc_mg_l = 3.1),
    list(nome = "MnSO4.4H2O",    conc_mg_l = 11.15),
    list(nome = "ZnSO4.7H2O",    conc_mg_l = 4.3),
    list(nome = "KI",            conc_mg_l = 0.415),
    list(nome = "Na2MoO4.2H2O",  conc_mg_l = 0.125),
    list(nome = "CuSO4.5H2O",    conc_mg_l = 0.0125),
    list(nome = "CoCl2.6H2O",    conc_mg_l = 0.0125),
    list(nome = "Glicose",       conc_mg_l = 5000,
         valor_original = 0.5, unidade_original = "%"),
    list(nome = "Tiamina-HCl",   conc_mg_l = 0.14),
    list(nome = "Piridoxina-HCl", conc_mg_l = 0.1),
    list(nome = "Acido nicotinico", conc_mg_l = 0.1),
    list(nome = "Phytagel",      conc_mg_l = 800,
         valor_original = 0.08, unidade_original = "%",
         observacao = "Solidificante composto com Agar"),
    list(nome = "Agar",          conc_mg_l = 4000,
         valor_original = 0.4, unidade_original = "%",
         observacao = "Solidificante composto com Phytagel")
  )
  
  n_meios_inseridos <- 0L
  for (m in meios_meta) {
    r <- .insert_if_missing(
      con, "meios",
      list(tenant_id     = tenant_id,
           categoria_id  = m$categoria,
           pop_id        = NA_integer_,
           nome          = m$nome,
           codigo_curto  = m$codigo,
           referencia    = m$referencia,
           doi           = m$doi,
           ph_alvo       = m$ph_alvo,
           observacoes   = m$obs,
           flag_incerteza = m$flag_inc,
           nota_incerteza = m$nota_inc,
           criado_por    = NA_integer_),
      list(coluna = "codigo_curto", valor = m$codigo),
      tenant_id = tenant_id
    )
    if (r$inserido) n_meios_inseridos <- n_meios_inseridos + 1L
    
    composicao <- composicoes[[m$codigo]]
    if (is.null(composicao)) {
      stop("Composicao nao definida para meio: ", m$codigo, call. = FALSE)
    }
    .insert_composicao(con, r$id, composicao, comp_ids)
  }
  
  invisible(n_meios_inseridos)
}

# ---------------------------------------------------------------------
# Seed 5: workflow do algodao (S&R 2001)
# ---------------------------------------------------------------------

#' Popula o workflow de transformacao de algodao via Agrobacterium
#'
#' 8 etapas em ordem: MS0, PIA, P1AS, P1S, P7M, MSBOK, EG3, MS3.
#'
#' @return Numero de workflows INSERIDOS (invisivel).
#' @noRd
seed_workflow_algodao <- function(con, tenant_id = TENANT_DEFAULT_ID) {
  n_meios <- DBI::dbGetQuery(
    con, "SELECT COUNT(*) AS n FROM meios WHERE tenant_id = ?;",
    params = list(tenant_id)
  )$n
  if (n_meios == 0L) {
    stop("Meios nao populados. Rode seed_meios_validados() antes.", call. = FALSE)
  }
  
  r <- .insert_if_missing(
    con, "workflows",
    list(tenant_id  = tenant_id,
         nome       = "Transformacao Algodao via Agrobacterium (S&R 2001)",
         referencia = "Sunilkumar G., Rathore K.S. (2001) Mol Breeding 8:37-52",
         doi        = "10.1023/A:1011906701925",
         descricao  = "Pipeline completo de transformacao de Gossypium hirsutum (Coker 312) via Agrobacterium, da germinacao a sustentacao de plantulas regeneradas. 8 etapas, ~6-9 meses."),
    list(coluna = "nome", valor = "Transformacao Algodao via Agrobacterium (S&R 2001)"),
    tenant_id = tenant_id
  )
  workflow_id <- r$id
  inseriu_workflow <- r$inserido
  
  meios_codigos <- c("MS0","PIA","P1AS","P1S","P7M","MSBOK","EG3","MS3")
  meios_ids <- vapply(meios_codigos, function(cod) {
    id <- .lookup_id(con, "meios", "codigo_curto", cod, tenant_id)
    if (is.na(id)) stop("Meio nao encontrado: ", cod, call. = FALSE)
    id
  }, integer(1))
  
  etapas <- list(
    list(ordem = 1, codigo = "MS0",   nome = "Germinacao de sementes (Coker 312)",
         duracao = "5-7 dias",
         condicoes = "28C, fotoperiodo 16h",
         obs = NA_character_),
    list(ordem = 2, codigo = "PIA",   nome = "Pre-inducao de Agrobacterium",
         duracao = "24h",
         condicoes = "28C, 200 rpm, ate A600 1.6-1.9",
         obs = "Meio liquido"),
    list(ordem = 3, codigo = "P1AS",  nome = "Co-cultivo",
         duracao = "3 dias",
         condicoes = "25C (ou 21C para maior eficiencia)",
         obs = "Acetosiringona induz genes vir do Agrobacterium"),
    list(ordem = 4, codigo = "P1S",   nome = "Selecao inicial de calos",
         duracao = "20-25 dias",
         condicoes = "28C",
         obs = NA_character_),
    list(ordem = 5, codigo = "P7M",   nome = "Manutencao de calos resistentes",
         duracao = "12 semanas",
         condicoes = "28C, subcultura a cada 4 semanas",
         obs = "Inversao de hormonios 2-iP/NAA em relacao ao P1"),
    list(ordem = 6, codigo = "MSBOK", nome = "Embriogenese somatica",
         duracao = "ate embrioes somaticos visiveis",
         condicoes = "28C",
         obs = "Apos 2 rounds de subcultura, omitir canamicina"),
    list(ordem = 7, codigo = "EG3",   nome = "Germinacao de embrioes somaticos",
         duracao = "ate plantulas 2-3 cm",
         condicoes = "28C, fotoperiodo 16h",
         obs = NA_character_),
    list(ordem = 8, codigo = "MS3",   nome = "Sustentacao ate transferencia para solo",
         duracao = "variavel",
         condicoes = "28C, fotoperiodo 16h",
         obs = NA_character_)
  )
  
  for (et in etapas) {
    existe <- DBI::dbGetQuery(
      con,
      "SELECT id FROM workflow_etapas WHERE workflow_id = ? AND ordem = ? LIMIT 1;",
      params = list(workflow_id, et$ordem)
    )
    if (nrow(existe) > 0L) next
    
    DBI::dbExecute(
      con,
      "INSERT INTO workflow_etapas
       (workflow_id, meio_id, ordem, nome_etapa, duracao, condicoes, observacoes)
       VALUES (?, ?, ?, ?, ?, ?, ?);",
      params = list(workflow_id,
                    meios_ids[[et$codigo]],
                    et$ordem,
                    et$nome,
                    et$duracao,
                    et$condicoes,
                    et$obs)
    )
  }
  
  invisible(as.integer(inseriu_workflow))
}