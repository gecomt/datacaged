# status.R
# Verifica disponibilidade do FTP do MTE e exibe mensagem de boas-vindas

# \u2500\u2500 Verifica\u00E7\u00E3o do FTP \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Verifica se o servidor FTP do MTE está acessível
#'
#' Tenta conectar ao FTP do MTE e retorna o status do servidor.
#' Útil para diagnosticar problemas de conectividade antes de iniciar
#' um download com `caged_load()` ou `caged_download()`.
#'
#' @param timeout integer. Timeout em segundos. Default: 10.
#' @param verbose logical. Se TRUE, exibe mensagem detalhada no console.
#'   Default: TRUE.
#'
#' @return invisível: lista com campos `online` (logical), `latencia_ms`
#'   (numeric) e `mensagem` (character).
#'
#' @examples
#' \dontrun{
#' caged_status()
#'
#' # Só verificar sem imprimir
#' st <- caged_status(verbose = FALSE)
#' st$online
#' }
#'
#' @seealso [caged_ftp_files()] para listar competências disponíveis no FTP.
#' @export
caged_status <- function(timeout = 10, verbose = TRUE) {
  url_teste <- paste0(.FTP_NOVO, "/")

  inicio <- proc.time()["elapsed"]

  ok <- tryCatch({
    tmp <- tempfile()
    on.exit(unlink(tmp), add = TRUE)
    code <- utils::download.file(
      url      = url_teste,
      destfile = tmp,
      mode     = "wb",
      quiet    = TRUE,
      method   = "curl",
      extra    = c(
        "--connect-timeout", as.character(timeout),
        "--max-time",        as.character(timeout),
        "--ftp-pasv",
        "--silent",
        "--list-only"       # s\u00F3 lista o diret\u00F3rio, n\u00E3o baixa dados
      )
    )
    code == 0L
  }, warning = function(w) {
    FALSE
  }, error = function(e) {
    FALSE
  })

  latencia_ms <- round((proc.time()["elapsed"] - inicio) * 1000)

  resultado <- list(
    online      = ok,
    latencia_ms = latencia_ms,
    url         = url_teste,
    mensagem    = if (ok) {
      paste0("FTP do MTE online (", latencia_ms, " ms)")
    } else {
      "FTP do MTE inacess\u00EDvel"
    }
  )

  if (verbose) {
    ftp_url <- .FTP_NOVO
    if (ok) {
      cli::cli_inform(c(
        "v" = "FTP do MTE {cli::col_green('online')}",
        "i" = "Lat\u00EAncia: {latencia_ms} ms",
        "i" = "URL: {.url {ftp_url}}"
      ))
    } else {
      cli::cli_inform(c(
        "x" = "FTP do MTE {cli::col_red('inacess\u00EDvel')}",
        "i" = "URL testada: {.url {url_teste}}",
        "i" = "Verifique sua conex\u00E3o ou tente novamente mais tarde.",
        "i" = "O FTP do MTE pode estar em manuten\u00E7\u00E3o (comum aos domingos)."
      ))
    }
  }

  invisible(resultado)
}

# \u2500\u2500 Mensagem de boas-vindas \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Exibida automaticamente ao carregar o pacote com library(datacaged)
#' @noRd
.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion("datacaged")

  # Usa formata\u00E7\u00E3o ANSI apenas quando o terminal suporta cores;
  # caso contr\u00E1rio exibe texto simples (redirecionamento, Rscript, CI sem TTY).
  if (cli::num_ansi_colors(stdout()) > 1L) {
    titulo <- paste0(cli::col_cyan(cli::style_bold("datacaged")), " ", ver,
                     " \u2014 Microdados do CAGED em DuckDB")
    ftp    <- cli::col_silver("FTP: ftp://ftp.mtps.gov.br/pdet/microdados/")
    dicas  <- cli::col_silver(paste0(
      "  Verifique o FTP com: caged_status()\n",
      "  Uso r\u00E1pido: caged_load(years = 2023, months = 1, db_path = \"caged.duckdb\")\n",
      "  Ajuda  : help(package = \"datacaged\") | ?caged_load"
    ))
  } else {
    titulo <- paste0("datacaged ", ver, " \u2014 Microdados do CAGED em DuckDB")
    ftp    <- "FTP: ftp://ftp.mtps.gov.br/pdet/microdados/"
    dicas  <- paste0(
      "  Verifique o FTP com: caged_status()\n",
      "  Uso r\u00E1pido: caged_load(years = 2023, months = 1, db_path = \"caged.duckdb\")\n",
      "  Ajuda  : help(package = \"datacaged\") | ?caged_load"
    )
  }

  packageStartupMessage("\n", titulo, "\n", ftp, "\n", dicas, "\n")
}

