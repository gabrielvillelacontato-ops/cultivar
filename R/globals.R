#' Constantes globais do CultivaR
#'
#' Centraliza paths, versoes e identificadores usados em todo o app.
#' Evita strings magicas espalhadas no codigo.
#'
#' @noRd

# Versao atual do schema do banco (ultima migration aplicada).
SCHEMA_VERSION_ATUAL <- "0.1.0"

# Caminho do banco SQLite local (desenvolvimento).
# Em producao, sobrescrito via golem-config.yml (Dia 7).
DB_PATH_DEV <- "inst/extdata/cultivar.sqlite"

# Pasta com arquivos de migration SQL.
MIGRATIONS_DIR <- "migrations"

# Tenant default do MVP. Multi-tenant real fica para v0.2+.
TENANT_DEFAULT_ID <- 1L

# Politica de lockout de PIN.
PIN_MAX_TENTATIVAS    <- 5L
PIN_LOCKOUT_MINUTOS   <- 15L