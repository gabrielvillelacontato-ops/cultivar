#' Utilitarios de banco de dados
#'
#' Funcoes para abrir/fechar conexao SQLite e aplicar migrations
#' de forma idempotente. PRAGMA foreign_keys=ON e journal_mode=WAL
#' sao ativados em toda conexao (SQLite nao enforce FK por padrao).
#'
#' @noRd

#' Abre uma conexao SQLite ja configurada
#'
#' Garante foreign_keys=ON e journal_mode=WAL. Cria o arquivo do banco
#' se ainda nao existir. A conexao deve ser fechada via db_disconnect().
#' PRAGMAs sao executados FORA de transacao (journal_mode requer isso).
#'
#' @param db_path Caminho para o arquivo .sqlite. Default: DB_PATH_DEV.
#' @return Objeto SQLiteConnection.
#' @noRd
db_connect <- function(db_path = DB_PATH_DEV) {
  stopifnot(is.character(db_path), length(db_path) == 1L, nzchar(db_path))
  
  dir_pai <- dirname(db_path)
  if (!dir.exists(dir_pai)) {
    dir.create(dir_pai, recursive = TRUE)
  }
  
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  # PRAGMAs fora de transacao. WAL persiste no arquivo (idempotente).
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON;")
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL;")
  con
}

#' Fecha conexao com seguranca
#'
#' @param con Objeto SQLiteConnection retornado por db_connect().
#' @noRd
db_disconnect <- function(con) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
  invisible(NULL)
}

#' Remove comentarios SQL de um texto
#'
#' Remove comentarios de linha (-- ate fim da linha) e de bloco
#' (encadeados nao suportados, suficiente para nosso uso).
#'
#' @param sql_texto String com SQL bruto.
#' @return String com comentarios removidos.
#' @noRd
db_strip_sql_comments <- function(sql_texto) {
  # Comentarios de bloco /* ... */ (nao-greedy, multilinha)
  sql_texto <- gsub("/\\*.*?\\*/", "", sql_texto, perl = TRUE)
  # Comentarios de linha -- ate \n
  sql_texto <- gsub("--[^\n]*", "", sql_texto, perl = TRUE)
  sql_texto
}

#' Divide texto SQL em statements individuais
#'
#' Split por ';' apos remover comentarios. Filtra statements vazios.
#'
#' @param sql_texto String com SQL ja sem comentarios.
#' @return Vetor de strings, cada uma um statement.
#' @noRd
db_split_sql_statements <- function(sql_texto) {
  partes <- strsplit(sql_texto, ";", fixed = TRUE)[[1]]
  partes <- trimws(partes)
  partes[nzchar(partes)]
}

#' Lista migrations disponiveis na pasta migrations/
#'
#' @param migrations_dir Pasta com arquivos .sql nomeados NNN_descricao.sql.
#' @return data.frame com colunas version, file, path. Ordenado por version.
#' @noRd
db_list_migrations <- function(migrations_dir = MIGRATIONS_DIR) {
  if (!dir.exists(migrations_dir)) {
    return(data.frame(version = character(0),
                      file    = character(0),
                      path    = character(0),
                      stringsAsFactors = FALSE))
  }
  
  arquivos <- list.files(migrations_dir, pattern = "\\.sql$", full.names = FALSE)
  if (length(arquivos) == 0L) {
    return(data.frame(version = character(0),
                      file    = character(0),
                      path    = character(0),
                      stringsAsFactors = FALSE))
  }
  
  prefixos <- sub("^(\\d+).*\\.sql$", "\\1", arquivos)
  validos  <- grepl("^\\d+$", prefixos)
  if (!all(validos)) {
    invalidos <- arquivos[!validos]
    stop("Arquivos de migration com nome invalido (esperado NNN_*.sql): ",
         paste(invalidos, collapse = ", "), call. = FALSE)
  }
  
  df <- data.frame(
    version = prefixos,
    file    = arquivos,
    path    = file.path(migrations_dir, arquivos),
    stringsAsFactors = FALSE
  )
  df[order(df$version), , drop = FALSE]
}

#' Aplica todas as migrations pendentes
#'
#' Idempotente: migrations ja registradas em schema_version sao puladas.
#' Cada arquivo .sql roda em sua propria transacao. Em caso de erro,
#' a transacao do arquivo falho e revertida e o erro propaga.
#'
#' Parser: remove comentarios (-- e /* */) ANTES de dividir por ';'.
#' Isso evita o bug classico de comentarios contendo ';' ou statements
#' iniciados por bloco de comentario.
#'
#' @param con Conexao aberta via db_connect().
#' @param migrations_dir Pasta com arquivos .sql.
#' @param verbose Se TRUE (default), imprime progresso.
#' @return Numero de migrations aplicadas nesta chamada (invisivel).
#' @noRd
db_apply_migrations <- function(con,
                                migrations_dir = MIGRATIONS_DIR,
                                verbose = TRUE) {
  stopifnot(DBI::dbIsValid(con))
  
  # Bootstrap de schema_version (1a execucao do app).
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS schema_version (
      version     TEXT PRIMARY KEY,
      applied_at  TEXT NOT NULL DEFAULT (strftime('%Y-%m-%dT%H:%M:%fZ', 'now')),
      description TEXT
    );
  ")
  
  migs <- db_list_migrations(migrations_dir)
  if (nrow(migs) == 0L) {
    if (verbose) message("Nenhuma migration encontrada em '", migrations_dir, "'.")
    return(invisible(0L))
  }
  
  aplicadas <- DBI::dbGetQuery(con, "SELECT version FROM schema_version;")$version
  pendentes <- migs[!migs$version %in% aplicadas, , drop = FALSE]
  
  if (nrow(pendentes) == 0L) {
    if (verbose) message("Schema ja atualizado. Nenhuma migration pendente.")
    return(invisible(0L))
  }
  
  n_aplicadas <- 0L
  for (i in seq_len(nrow(pendentes))) {
    versao  <- pendentes$version[i]
    arquivo <- pendentes$file[i]
    caminho <- pendentes$path[i]
    
    if (verbose) message("Aplicando migration ", versao, " (", arquivo, ")...")
    
    sql_bruto    <- paste(readLines(caminho, encoding = "UTF-8", warn = FALSE),
                          collapse = "\n")
    sql_limpo    <- db_strip_sql_comments(sql_bruto)
    statements   <- db_split_sql_statements(sql_limpo)
    
    if (length(statements) == 0L) {
      stop("Migration ", versao, " nao contem statements executaveis.",
           call. = FALSE)
    }
    
    DBI::dbBegin(con)
    tryCatch({
      for (stmt in statements) {
        DBI::dbExecute(con, stmt)
      }
      DBI::dbExecute(
        con,
        "INSERT INTO schema_version (version, description) VALUES (?, ?);",
        params = list(versao, arquivo)
      )
      DBI::dbCommit(con)
      n_aplicadas <- n_aplicadas + 1L
      if (verbose) message("  -> aplicada com sucesso.")
    }, error = function(e) {
      DBI::dbRollback(con)
      stop("Falha ao aplicar migration ", versao, ": ", conditionMessage(e),
           call. = FALSE)
    })
  }
  
  if (verbose) message("Total: ", n_aplicadas, " migration(s) aplicada(s).")
  invisible(n_aplicadas)
}