#' Funcoes puras de calculo cientifico
#'
#' Coracao cientifico do CultivaR. Erros aqui descartam lotes reais.
#' Todas as funcoes sao puras (sem side effects exceto gerar_lote_interno
#' e preparar_lote, que consultam o banco) e cobertas por golden tests
#' em tests/testthat/test-fct_calculo.R.
#'
#' Convencoes:
#'   - Unidade canonica armazenada: mg/L
#'   - Massa molar em g/mol
#'   - Volumes em mL
#'   - Massas calculadas em mg
#'   - NA, NaN e Inf em inputs numericos rejeitados explicitamente
#'
#' @noRd

# ---------------------------------------------------------------------
# Helpers internos de validacao
# ---------------------------------------------------------------------

#' Valida que um valor numerico e finito e (opcionalmente) positivo
#' @noRd
.validar_numerico <- function(valor, nome, positivo = TRUE,
                              permite_zero = FALSE, permite_na = FALSE) {
  if (is.null(valor) || length(valor) != 1L) {
    stop("Parametro '", nome, "' deve ter exatamente um valor.", call. = FALSE)
  }
  if (is.na(valor)) {
    if (permite_na) return(invisible(NULL))
    stop("Parametro '", nome, "' nao pode ser NA.", call. = FALSE)
  }
  if (!is.numeric(valor)) {
    stop("Parametro '", nome, "' deve ser numerico, recebido: ",
         class(valor)[1], call. = FALSE)
  }
  if (!is.finite(valor)) {
    stop("Parametro '", nome, "' deve ser finito (recebido NaN ou Inf).",
         call. = FALSE)
  }
  if (positivo) {
    if (permite_zero) {
      if (valor < 0) {
        stop("Parametro '", nome, "' deve ser >= 0, recebido: ", valor,
             call. = FALSE)
      }
    } else {
      if (valor <= 0) {
        stop("Parametro '", nome, "' deve ser > 0, recebido: ", valor,
             call. = FALSE)
      }
    }
  }
  invisible(NULL)
}

#' Normaliza unidade aceitando aliases unicode (µ -> u)
#' @noRd
.normalizar_unidade <- function(unidade) {
  if (is.null(unidade) || is.na(unidade)) return(NA_character_)
  if (!is.character(unidade) || length(unidade) != 1L) {
    stop("Unidade deve ser string unica.", call. = FALSE)
  }
  # Substitui mu (µ, U+00B5 e U+03BC) por 'u'
  u <- gsub("\u00b5|\u03bc", "u", unidade, fixed = FALSE)
  trimws(u)
}

# ---------------------------------------------------------------------
# 1. calcular_massa
# ---------------------------------------------------------------------

#' Calcula massa (mg) a pesar para um volume dado
#'
#' massa_mg = concentracao_mg_l * volume_ml / 1000
#'
#' @param concentracao_mg_l Numerico > 0. Concentracao alvo.
#' @param volume_ml Numerico > 0. Volume final do preparo.
#' @return Numerico: massa em mg.
#' @noRd
calcular_massa <- function(concentracao_mg_l, volume_ml) {
  .validar_numerico(concentracao_mg_l, "concentracao_mg_l", positivo = TRUE)
  .validar_numerico(volume_ml, "volume_ml", positivo = TRUE)
  concentracao_mg_l * volume_ml / 1000
}

# ---------------------------------------------------------------------
# 2. converter_para_mg_l
# ---------------------------------------------------------------------

#' Converte uma concentracao em qualquer unidade suportada para mg/L
#'
#' Unidades suportadas (com aliases para mu unicode):
#'   - mg/L (identidade)
#'   - ug/L: valor / 1000
#'   - g/L: valor * 1000
#'   - mg/mL: valor * 1000
#'   - ug/mL: valor (identidade numerica)
#'   - uM, mM, nM, M: precisam de massa_molar (g/mol)
#'   - %: assume m/v (1% = 10000 mg/L)
#'
#' @param valor Numerico > 0.
#' @param unidade String. Aceita 'uM' ou 'µM' (normalizado).
#' @param massa_molar Numerico > 0 (g/mol). Obrigatorio apenas para unidades
#'   molares (uM, mM, nM, M). Pode ser NA caso contrario.
#' @return Numerico: concentracao em mg/L.
#' @noRd
converter_para_mg_l <- function(valor, unidade, massa_molar = NA_real_) {
  .validar_numerico(valor, "valor", positivo = TRUE)
  un <- .normalizar_unidade(unidade)
  if (is.na(un) || !nzchar(un)) {
    stop("Unidade nao pode ser vazia ou NA.", call. = FALSE)
  }
  
  unidades_molares <- c("uM", "mM", "nM", "M")
  if (un %in% unidades_molares) {
    .validar_numerico(massa_molar, "massa_molar", positivo = TRUE)
  }
  
  resultado <- switch(
    un,
    "mg/L"  = valor,
    "ug/L"  = valor / 1000,
    "g/L"   = valor * 1000,
    "mg/mL" = valor * 1000,
    "ug/mL" = valor,
    "uM"    = valor * massa_molar / 1000,
    "mM"    = valor * massa_molar,
    "nM"    = valor * massa_molar / 1e6,
    "M"     = valor * massa_molar * 1000,
    "%"     = valor * 10000,  # assume m/v
    "ppm"   = valor,           # 1 ppm = 1 mg/L em meio aquoso
    "x"     = stop("Unidade 'x' (fator multiplicativo) nao convertivel sem contexto.",
                   call. = FALSE),
    stop("Unidade nao suportada: '", un, "'. Suportadas: ",
         "mg/L, ug/L, g/L, mg/mL, ug/mL, uM, mM, nM, M, %, ppm.",
         call. = FALSE)
  )
  resultado
}

# ---------------------------------------------------------------------
# 3. formatar_massa
# ---------------------------------------------------------------------

#' Formata massa em mg para exibicao na UI com 3 algarismos significativos
#'
#' Regras:
#'   - >= 1000 mg: exibe em g
#'   - 1 a 1000 mg: exibe em mg
#'   - < 1 mg: exibe em ug
#'   - 0 mg: exibe "0 mg"
#'
#' @param mg Numerico >= 0. Massa em mg.
#' @return String formatada (vetorizada).
#' @noRd
formatar_massa <- function(mg) {
  if (is.null(mg) || length(mg) == 0L) {
    return(character(0))
  }
  if (!is.numeric(mg)) {
    stop("Massa deve ser numerica.", call. = FALSE)
  }
  
  vapply(mg, function(x) {
    if (is.na(x)) return(NA_character_)
    if (!is.finite(x)) return(as.character(x))
    if (x < 0) {
      stop("Massa nao pode ser negativa: ", x, call. = FALSE)
    }
    if (x == 0) return("0 mg")
    
    if (x >= 1000) {
      val <- signif(x / 1000, 3)
      .formatar_pt(val, "g")
    } else if (x >= 1) {
      val <- signif(x, 3)
      .formatar_pt(val, "mg")
    } else {
      # < 1 mg: converte para ug
      val <- signif(x * 1000, 3)
      .formatar_pt(val, "ug")
    }
  }, character(1))
}

#' Formata numero com separador decimal pt-BR (virgula)
#' @noRd
.formatar_pt <- function(valor, unidade) {
  # signif ja arredondou; usa format() para evitar notacao cientifica
  txt <- format(valor, scientific = FALSE, big.mark = "",
                decimal.mark = ",", trim = TRUE)
  # Remove zeros a direita desnecessarios apos virgula (ex: 30,00 -> manter)
  # Mantemos os zeros para preservar a precisao visual de signif(_, 3).
  paste(txt, unidade)
}

# ---------------------------------------------------------------------
# 4. gerar_lote_interno
# ---------------------------------------------------------------------

#' Gera o proximo lote_interno atomicamente
#'
#' Formato: <codigo_curto>_<YYYY-MM-DD>_<NNNN>
#' Sequencial com 4 digitos de padding (zero-padded), expande naturalmente
#' se ultrapassar 9999.
#'
#' Estrategia anti-race-condition:
#'   1. BEGIN IMMEDIATE (lock de escrita em SQLite)
#'   2. SELECT MAX(sequencial) WHERE codigo_curto = ? AND data = ?
#'   3. Constroi proximo ID
#'   4. Retorna sem INSERT (caller insere preparo com este lote_interno)
#'
#' O INSERT do preparo deve ocorrer DENTRO da mesma transacao aberta
#' por esta funcao. A funcao retorna o lote e mantem a transacao aberta;
#' o caller e responsavel por COMMIT ou ROLLBACK.
#'
#' Para uso simples (caller nao gerencia transacao), use a variante
#' gerar_lote_interno_simples() abaixo.
#'
#' @param con Conexao SQLite.
#' @param codigo_curto String ASCII curta (ex: 'MSR').
#' @param data String 'YYYY-MM-DD' ou objeto Date.
#' @param tenant_id Inteiro. Default: TENANT_DEFAULT_ID.
#' @return String com lote_interno proposto.
#' @noRd
gerar_lote_interno <- function(con, codigo_curto, data,
                               tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  if (!is.character(codigo_curto) || length(codigo_curto) != 1L ||
      !nzchar(codigo_curto)) {
    stop("codigo_curto deve ser string nao vazia.", call. = FALSE)
  }
  if (inherits(data, "Date")) data <- format(data, "%Y-%m-%d")
  if (!is.character(data) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", data)) {
    stop("data deve estar no formato 'YYYY-MM-DD'.", call. = FALSE)
  }
  
  prefixo <- paste0(codigo_curto, "_", data, "_")
  
  # Busca o maior sequencial existente para este prefixo
  # LIKE com escape: codigo_curto e data sao controlados (nao input direto),
  # mas usamos parametro para defesa em profundidade.
  res <- DBI::dbGetQuery(
    con,
    "SELECT lote_interno FROM preparos
     WHERE tenant_id = ? AND lote_interno LIKE ?
     ORDER BY lote_interno DESC;",
    params = list(tenant_id, paste0(prefixo, "%"))
  )
  
  if (nrow(res) == 0L) {
    seq_novo <- 1L
  } else {
    # Extrai o sequencial do ultimo registro
    seqs <- as.integer(sub(paste0("^", prefixo), "", res$lote_interno))
    seqs <- seqs[!is.na(seqs)]
    if (length(seqs) == 0L) {
      seq_novo <- 1L
    } else {
      seq_novo <- max(seqs) + 1L
    }
  }
  
  # Padding minimo de 4 digitos, expande naturalmente
  seq_str <- if (seq_novo >= 10000L) {
    as.character(seq_novo)
  } else {
    sprintf("%04d", seq_novo)
  }
  paste0(prefixo, seq_str)
}

# ---------------------------------------------------------------------
# 5. preparar_lote
# ---------------------------------------------------------------------

#' Orquestra o calculo completo de um preparo
#'
#' Dado um meio e volume, retorna data.frame pronto para a UI com:
#'   - componente_id, nome, formula, cas, massa_molar
#'   - concentracao_mg_l (do meio)
#'   - massa_mg (calculada)
#'   - massa_formatada (string para UI)
#'   - observacao (do meio_componentes)
#'
#' Erros:
#'   - Meio inexistente
#'   - Meio com deleted_at preenchido
#'   - Meio com bloqueado_preparo = 1 (mensagem inclui nota_incerteza)
#'   - volume_ml <= 0 ou nao numerico
#'
#' @param con Conexao SQLite.
#' @param meio_id Inteiro: id em meios.
#' @param volume_ml Numerico > 0.
#' @param tenant_id Inteiro. Default: TENANT_DEFAULT_ID.
#' @return data.frame com componentes e massas calculadas.
#' @noRd
preparar_lote <- function(con, meio_id, volume_ml,
                          tenant_id = TENANT_DEFAULT_ID) {
  stopifnot(DBI::dbIsValid(con))
  .validar_numerico(volume_ml, "volume_ml", positivo = TRUE)
  if (!is.numeric(meio_id) || length(meio_id) != 1L || is.na(meio_id)) {
    stop("meio_id deve ser inteiro unico nao-NA.", call. = FALSE)
  }
  meio_id <- as.integer(meio_id)
  
  # Carrega meio
  meio <- DBI::dbGetQuery(
    con,
    "SELECT id, codigo_curto, nome, ph_alvo, bloqueado_preparo,
            flag_incerteza, nota_incerteza, deleted_at
     FROM meios
     WHERE id = ? AND tenant_id = ?;",
    params = list(meio_id, tenant_id)
  )
  
  if (nrow(meio) == 0L) {
    stop("Meio nao encontrado: ID ", meio_id, call. = FALSE)
  }
  if (!is.na(meio$deleted_at[1])) {
    stop("Meio '", meio$codigo_curto[1], "' foi arquivado (deleted_at: ",
         meio$deleted_at[1], ").", call. = FALSE)
  }
  if (meio$bloqueado_preparo[1] == 1L) {
    motivo <- if (is.na(meio$nota_incerteza[1])) {
      "sem motivo registrado"
    } else {
      meio$nota_incerteza[1]
    }
    stop("Meio '", meio$codigo_curto[1],
         "' esta bloqueado para preparo. Motivo: ", motivo,
         call. = FALSE)
  }
  
  # Carrega composicao
  comp <- DBI::dbGetQuery(
    con,
    "SELECT
       mc.componente_id,
       c.nome,
       c.formula,
       c.cas,
       c.massa_molar,
       mc.concentracao_mg_l,
       mc.valor_original,
       mc.unidade_original,
       mc.ordem_exibicao,
       mc.observacao
     FROM meio_componentes mc
     JOIN componentes c ON c.id = mc.componente_id
     WHERE mc.meio_id = ?
     ORDER BY mc.ordem_exibicao, c.nome;",
    params = list(meio_id)
  )
  
  if (nrow(comp) == 0L) {
    stop("Meio '", meio$codigo_curto[1],
         "' nao tem componentes cadastrados.", call. = FALSE)
  }
  
  # Calcula massa para cada componente
  comp$massa_mg <- vapply(seq_len(nrow(comp)), function(i) {
    calcular_massa(comp$concentracao_mg_l[i], volume_ml)
  }, numeric(1))
  
  comp$massa_formatada <- formatar_massa(comp$massa_mg)
  
  # Anexa metadado do meio
  attr(comp, "meio_codigo")  <- meio$codigo_curto[1]
  attr(comp, "meio_nome")    <- meio$nome[1]
  attr(comp, "meio_ph_alvo") <- meio$ph_alvo[1]
  attr(comp, "volume_ml")    <- volume_ml
  attr(comp, "flag_incerteza") <- meio$flag_incerteza[1]
  attr(comp, "nota_incerteza") <- meio$nota_incerteza[1]
  
  comp
}