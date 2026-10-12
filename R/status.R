# status.R
# Verifica disponibilidade do repositório HuggingFace e exibe mensagem de boas-vindas

# -- Verificação do repositório HuggingFace ------------------------------------

#' Verifica se o repositório HuggingFace do CAGED está acessível
#'
#' Faz uma requisição leve à API do HuggingFace e retorna o status.
#' Útil para diagnosticar problemas de conectividade antes de iniciar
#' um download com `caged_load()` ou `caged_download()`.
#'
#' @param timeout integer. Timeout in seconds. Default: 10.
#' @param verbose logical. If TRUE, exibe mensagem detalhada no console.
#'   Default: TRUE.
#'
#' @return invisível: lista com campos `online` (logical), `latencia_ms`
#'   (numeric), `url` (character) e `mensagem` (character).
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
#' @seealso [caged_hf_files()] para listar competências disponíveis.
#' @export
caged_status <- function(timeout = 10, verbose = TRUE) {
  # Endpoint leve: raiz do repositório via API HF
  url_teste <- paste0(.HF_API)

  inicio <- proc.time()["elapsed"]

  ok <- tryCatch({
    resp <- httr2::request(url_teste) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    httr2::resp_status(resp) == 200L
  }, error = function(e) FALSE)

  latencia_ms <- round((proc.time()["elapsed"] - inicio) * 1000)

  resultado <- list(
    online      = ok,
    latencia_ms = latencia_ms,
    url         = url_teste,
    mensagem    = if (ok) {
      paste0("HuggingFace online (", latencia_ms, " ms)")
    } else {
      "HuggingFace repository unavailable"
    }
  )

  if (verbose) {
    repo_url <- paste0("https://huggingface.co/datasets/", .HF_REPO)
    if (ok) {
      status_txt <- cli::col_green(gettext("online"))
      cli::cli_inform(c(
        "v" = gettext("HuggingFace repository {status_txt}"),
        "i" = gettext("Latency: {latencia_ms} ms"),
        "i" = gettext("Dataset: {.url {repo_url}}")
      ))
    } else {
      status_txt <- cli::col_red(gettext("unavailable"))
      cli::cli_inform(c(
        "x" = gettext("HuggingFace repository {status_txt}"),
        "i" = gettext("Tested URL: {.url {url_teste}}"),
        "i" = gettext("Check your connection or try again later."),
        "i" = gettext("HuggingFace may be under temporary maintenance.")
      ))
    }
  }

  invisible(resultado)
}

# -- Mensagem de boas-vindas ---------------------------------------------------

#' Exibida automaticamente ao carregar o pacote com library(datacaged)
#' @noRd
.onAttach <- function(libname, pkgname) {
  ver  <- utils::packageVersion("datacaged")
  repo <- paste0("https://huggingface.co/datasets/", .HF_REPO)

  subtitulo <- gettext("CAGED microdata in DuckDB")
  fonte     <- paste(gettext("Source:"), repo)
  dicas     <- paste0(
    "  ", gettext("Check the connection with:"), " caged_status()\n",
    "  ", gettext("Quick start:"), " caged_load(years = 2023, months = 1, db_path = \"caged.duckdb\")\n",
    "  ", gettext("Help:"), " help(package = \"datacaged\") | ?caged_load"
  )

  if (cli::num_ansi_colors(stdout()) > 1L) {
    titulo <- paste0(cli::col_cyan(cli::style_bold("datacaged")), " ", ver, " - ", subtitulo)
    fonte  <- cli::col_silver(fonte)
    dicas  <- cli::col_silver(dicas)
  } else {
    titulo <- paste0("datacaged ", ver, " - ", subtitulo)
  }

  packageStartupMessage("\n", titulo, "\n", fonte, "\n", dicas, "\n")
}
