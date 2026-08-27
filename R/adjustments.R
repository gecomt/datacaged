# adjustments.R
# Download e carga do CAGED Ajustes no DuckDB

#' Pipeline completo para o CAGED Ajustes
#'
#' Baixa, parseia e grava os arquivos do CAGED Ajustes no banco DuckDB local.
#'
#' O CAGED Ajustes contém correções retroativas de vínculos do CAGED antigo
#' (até 2019). Cada arquivo `CAGEDEST_AJUSTES_{MM}{AAAA}.7z` registra
#' movimentações ajustadas após a declaração original --- essencial para
#' reconstrução de séries históricas mais precisas.
#'
#' Os dados são gravados na tabela `caged_ajustes` do banco DuckDB, com o mesmo
#' schema do `caged_antigo`.
#'
#' @param years integer vector. Desired years. Maximum: 2019.
#' @param months integer vector. Desired months (1--12). Default: `seq_len(12L)`.
#' @param db_path character. Path to the file `.duckdb`.
#' @param destdir character or NULL. Cache directory for `.7z` files.
#'   Default: `tools::R_user_dir("datacaged", "cache")`.
#' @param force_download logical. If TRUE, re-downloads even if already cached.
#'   Default: FALSE.
#' @param overwrite_competencies logical. If TRUE, overwrites competencies
#'   already in the database. Default: FALSE.
#' @param timeout integer. Timeout in seconds. Default: 300.
#'
#' @return invisível: tibble com estatísticas do banco após a carga.
#'
#' @examples
#' \dontrun{
#' # List available adjustment competencies first
#' avail <- caged_hf_files(type = "ajustes", n = 3, verbose = FALSE)
#'
#' if (nrow(avail) > 0) {
#'   db <- file.path(tempdir(), "caged_ajustes.duckdb")
#'
#'   # Download and load adjustments
#'   caged_adjustments_load(
#'     years   = avail$ano[1],
#'     months  = avail$mes[1],
#'     db_path = db
#'   )
#'
#'   con <- caged_connect(db)
#'   # List tables and records
#'   dplyr::tbl(con, "caged_ajustes") |>
#'     dplyr::group_by(competencia) |>
#'     dplyr::count() |>
#'     dplyr::collect()
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#' }
#'
#' @seealso [caged_load()] para Novo CAGED (2020+) e CAGED antigo (até 2019).
#' @seealso [caged_hf_files()] com `type = "ajustes"` para listar disponibilidade.
#' @export
caged_adjustments_load <- function(years,
                                   months                 = seq_len(12L),
                                   db_path,
                                   destdir                = NULL,
                                   force_download         = FALSE,
                                   overwrite_competencies = FALSE,
                                   timeout                = 300) {

  years  <- .validate_years(years)
  months <- .validate_months(months)

  # Ajustes só existem para o CAGED antigo (até 2019)
  ano_limite <- .ANO_CORTE - 1L
  invalid_years <- years[years >= .ANO_CORTE]
  if (length(invalid_years) > 0) {
    cli::cli_warn(c(
      "!" = "CAGED Adjustments only exist for years up to {.val {ano_limite}}.",
      "i" = "Ignored years: {.val {invalid_years}}"
    ))
    years <- years[years < .ANO_CORTE]
  }

  if (length(years) == 0) {
    cli::cli_abort(
      "No valid year. CAGED Adjustments cover up to {.val {ano_limite}}."
    )
  }

  cache <- .cache_dir(destdir)

  cli::cli_h1("caged_adjustments_load")
  cli::cli_inform(c(
    "i" = "Years : {years[1]}-{years[length(years)]}",
    "i" = "Months: {months[1]}-{months[length(months)]}",
    "i" = "DB    : {.path {db_path}}"
  ))

  # 1. Download
  cli::cli_h2("1/3  Download")
  tasks <- .build_adjustments_tasks(years, months)

  if (nrow(tasks) == 0) {
    cli::cli_warn("No download tasks generated.")
    return(invisible(NULL))
  }

  cli::cli_inform(c(
    "i" = "{nrow(tasks)} arquivo{?s} a processar",
    "i" = "Cache: {.path {cache}}"
  ))

  manifest <- .run_downloads(tasks, cache, force_download, timeout)

  resumo <- dplyr::count(manifest, status)
  cli::cli_h2("Summary")
  purrr::walk2(resumo$status, resumo$n, function(s, cnt) {
    icon <- switch(s,
      baixado        = cli::col_green("v"),
      cache          = cli::col_blue("o"),
      nao_encontrado = cli::col_yellow("!"),
      erro           = cli::col_red("x"),
      "?"
    )
    cli::cli_inform("{icon} {s}: {cnt}")
  })

  manifest_ok <- dplyr::filter(manifest, status %in% c("baixado", "cache"))

  if (nrow(manifest_ok) == 0) {
    cli::cli_warn("No files available to process.")
    return(invisible(NULL))
  }

  # 2. Parse e carga
  cli::cli_h2("2/3  Parse and load into DuckDB")
  n_inserido_total <- 0L

  cli::cli_progress_bar(
    name   = "Arquivos",
    total  = nrow(manifest_ok),
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
  )

  # Abre UMA conexão para todo o loop
  con_adj <- caged_connect(db_path, quiet = TRUE)
  on.exit(.disconnect(con_adj), add = TRUE)

  for (i in seq_len(nrow(manifest_ok))) {
    arq <- manifest_ok$arquivo[i]

    df <- tryCatch(
      caged_parse(arq, type = "AJUSTES"),
      error = function(e) {
        cli::cli_warn("Error in {.path {basename(arq)}}: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(df) && nrow(df) > 0) {
      n <- caged_to_duckdb(
        df,
        .con                      = con_adj,
        overwrite_competencies    = overwrite_competencies
      )
      n_inserido_total <- n_inserido_total + n
    }

    rm(df)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # 3. Resumo
  cli::cli_h2("3/3  Done")
  cli::cli_inform(c(
    "v" = "Total inserted: {format(n_inserido_total, big.mark = ',')} records"
  ))

  invisible(caged_info(db_path))
}

# -- Helper interno ------------------------------------------------------------

#' Monta tabela de tarefas de download para o CAGED Ajustes via HuggingFace
#' @noRd
.build_adjustments_tasks <- function(years, months) {
  today <- Sys.Date()
  rows  <- list()

  for (year in years) {
    for (month in months) {
      if (as.Date(sprintf("%04d-%02d-01", year, month)) > today) next

      url <- .hf_url(year, month, type = "AJUSTES")

      rows[[length(rows) + 1L]] <- tibble::tibble(
        ano          = year,
        mes          = month,
        competencia  = .format_period(year, month),
        type         = "AJUSTES",
        uf           = NA_character_,
        url          = url,
        nome_arquivo = basename(url),
        subdir       = file.path(.HF_PASTA_AJUSTES, as.character(year))
      )
    }
  }

  if (length(rows) == 0L) return(tibble::tibble())
  dplyr::bind_rows(rows)
}
