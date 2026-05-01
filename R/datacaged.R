#' datacaged: Microdados do CAGED em DuckDB
#'
#' \if{html}{\figure{logo.png}{options: width="120" alt="datacaged logo"}}
#'
#' @description
#' O pacote `datacaged` facilita o acesso aos microdados do CAGED
#' (Cadastro Geral de Empregados e Desempregados), cobrindo:
#'
#' - **Novo CAGED** (janeiro/2020 em diante): layout reformulado pelo MTE
#' - **CAGED antigo** (até dezembro/2019): séries históricas (`CAGEDEST_*.7z`)
#' - **CAGED Ajustes** (até dezembro/2019): correções retroativas do antigo
#'
#' Os dados são baixados do FTP oficial do MTE (`ftp://ftp.mtps.gov.br/pdet/microdados/`),
#' extraídos de arquivos `.7z` e carregados em um banco DuckDB local
#' para consultas rápidas com SQL ou dplyr.
#'
#' @section Fluxo típico de uso:
#' ```r
#' library(datacaged)
#'
#' # Baixa, parseia e grava tudo em um comando
#' caged_load(
#'   years   = 2022:2023,
#'   months  = seq_len(12L),
#'   db_path = "meu_caged.duckdb"
#' )
#'
#' # Conecta e consulta
#' con <- caged_connect("meu_caged.duckdb")
#' dplyr::tbl(con, "caged_mov") |>
#'   dplyr::filter(uf == 35) |>
#'   dplyr::count(competenciamov)
#' ```
#'
#' @section Funções principais:
#' **Novo CAGED (2020+) e CAGED antigo (até 2019):**
#' - `caged_load()`: pipeline completo (download -> parse -> DuckDB)
#' - `caged_download()`: só baixa os arquivos .7z
#' - `caged_parse()`: lê e normaliza um arquivo `.7z`
#' - `caged_parse_batch()`: lê e normaliza múltiplos arquivos `.7z`
#' - `caged_to_duckdb()`: grava um data.frame no banco
#' - `caged_connect()`: abre conexão com o banco DuckDB
#' - `caged_info()`: lista tabelas e registros disponíveis no banco
#'
#' **CAGED Ajustes (correções retroativas até 2019):**
#' - `caged_adjustments_load()`: pipeline completo para o CAGED Ajustes
#'
#' **Utilitários:**
#' - `caged_status()`: verifica se o FTP do MTE está online
#' - `caged_ftp_files()`: lista competências disponíveis no FTP
#'
#' @section Tabelas no DuckDB:
#' | Tabela | Fonte | Período |
#' |---|---|---|
#' | `caged_mov` | `CAGEDMOV*.7z` | Jan/2020+ |
#' | `caged_for` | `CAGEDFOR*.7z` | Jan/2020+ |
#' | `caged_exc` | `CAGEDEXC*.7z` | Jan/2020+ |
#' | `caged_antigo` | `CAGEDEST_*.7z` | 1992–Dez/2019 |
#' | `caged_ajustes` | `CAGEDAJUSTES_*.7z` | 1992–Dez/2019 |
#'
#' @docType package
#' @name datacaged-package
#' @aliases datacaged
#'
#' @importFrom cli cli_abort cli_inform cli_warn cli_h1 cli_h2
#'   cli_progress_bar cli_progress_done cli_progress_update
#'   col_blue col_cyan col_green col_red col_silver col_yellow
#'   pb_bar pb_current pb_elapsed pb_eta_str pb_total
#'   style_bold num_ansi_colors
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExecute
#'   dbListTables dbListFields dbExistsTable dbWriteTable dbAppendTable
#'   dbSendQuery dbSendStatement dbBind dbFetch dbClearResult
#' @importFrom dplyr bind_rows filter pull count arrange collect
#'   group_by summarise mutate full_join left_join desc tbl
#' @importFrom glue glue
#' @importFrom purrr map walk2 compact
#' @importFrom readr read_delim cols col_character locale
#' @importFrom stringr str_replace_all str_remove
#' @importFrom tibble tibble
#' @importFrom tools R_user_dir
#' @importFrom utils download.file unzip packageVersion head globalVariables
#' @importFrom archive archive_extract
#' @importFrom duckdb duckdb
"_PACKAGE"

# ── Constantes internas ────────────────────────────────────────────────────────

#' URLs base do FTP do MTE — protocolo FTP puro (porta 21)
#' @noRd
.FTP_NOVO   <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED"
.FTP_ANTIGO  <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED"
.FTP_AJUSTES <- "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED_AJUSTES"

#' Ano de corte entre CAGED antigo e Novo CAGED
#' @noRd
.ANO_CORTE <- 2020

#' Mapa de siglas de UF para código numérico do IBGE
#' @noRd
.UF_CODIGOS <- c(
  RO = 11, AC = 12, AM = 13, RR = 14, PA = 15, AP = 16, TO = 17,
  MA = 21, PI = 22, CE = 23, RN = 24, PB = 25, PE = 26, AL = 27,
  SE = 28, BA = 29, MG = 31, ES = 32, RJ = 33, SP = 35, PR = 41,
  SC = 42, RS = 43, MS = 50, MT = 51, GO = 52, DF = 53
)

# ── Utilitários internos ───────────────────────────────────────────────────────

#' Valida e normaliza vetor de UFs
#' @param states character ou NULL
#' @return character com siglas em maiúsculo, ou NULL
#' @noRd
.validate_states <- function(states) {
  if (is.null(states)) return(NULL)
  states <- toupper(trimws(states))
  invalidas <- setdiff(states, names(.UF_CODIGOS))
  if (length(invalidas) > 0) {
    cli::cli_abort(
      c("UF{?s} inválida{?s}: {.val {invalidas}}",
        "i" = "Use siglas válidas: {.val {names(.UF_CODIGOS)}}")
    )
  }
  states
}

#' Valida anos
#' @noRd
.validate_years <- function(years) {
  if (!is.numeric(years) || any(years < 1992) || any(years > as.integer(format(Sys.Date(), "%Y")))) {
    cli::cli_abort(
      c("Anos inválidos: {.val {years}}",
        "i" = "Informe anos entre 1992 e {format(Sys.Date(), '%Y')}.")
    )
  }
  as.integer(years)
}

#' Valida meses
#' @noRd
.validate_months <- function(months) {
  if (!is.numeric(months) || any(months < 1) || any(months > 12)) {
    cli::cli_abort("Meses inválidos. Informe inteiros entre 1 e 12.")
  }
  as.integer(months)
}

#' Formata competência como "AAAAMM"
#' @noRd
.format_period <- function(year, month) {
  sprintf("%04d%02d", year, month)
}

#' Determina se um ano/mês pertence ao Novo CAGED
#' Novo CAGED cobre janeiro/2020 em diante (.ANO_CORTE = 2020).
#' @noRd
.is_new_caged <- function(year, month = 1) {
  year >= .ANO_CORTE
}

# ── Suprimir warnings de "no visible binding" do R CMD check ──────────────────
# Variáveis usadas em contexto dplyr (non-standard evaluation)
utils::globalVariables(c(
  # download.R / database.R / adjustments.R
  "status",       # manifest |> dplyr::filter(status %in% ...)
  "arquivo",      # manifest_ok |> dplyr::pull(arquivo)
  "competencia",  # dplyr::filter(df, competencia %in% novas)
  # database.R — caged_info()
  "competencia_declarada",
  # adjustments.R / availability.R
  "type",
  "ano",
  "mes",
  "n",            # dplyr::count() result column
  "cnt"           # alias usado nos walk2 corrigidos
))

