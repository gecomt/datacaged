# adjustments.R
# Download e carga do CAGED Ajustes no DuckDB

#' Pipeline completo para o CAGED Ajustes
#'
#' Baixa, parseia e grava os arquivos do CAGED Ajustes no banco DuckDB local.
#'
#' O CAGED Ajustes (`/pdet/microdados/CAGED_AJUSTES`) contém correções retroativas
#' de vínculos do CAGED antigo (até 2019). Cada arquivo `CAGEDAJUSTES_{MM}{AAAA}.7z`
#' registra movimentações ajustadas após a declaração original — essencial para
#' reconstrução de séries históricas mais precisas.
#'
#' Os dados são gravados na tabela `caged_ajustes` do banco DuckDB, com o mesmo
#' schema do `caged_antigo`.
#'
#' @param years integer vector. Anos desejados. Máximo: 2019.
#' @param months integer vector. Meses desejados (1–12). Default: `seq_len(12L)` (todos os meses).
#' @param db_path character. Caminho para o arquivo `.duckdb`.
#' @param destdir character ou NULL. Diretório de cache dos `.7z`.
#'   Default: `tools::R_user_dir("datacaged", "cache")`.
#' @param force_download logical. Se TRUE, re-baixa mesmo se já em cache.
#'   Default: FALSE.
#' @param overwrite_competencies logical. Se TRUE, regrava competências
#'   já existentes no banco. Default: FALSE.
#' @param timeout integer. Timeout em segundos. Default: 300.
#'
#' @return invisível: tibble com estatísticas do banco após a carga.
#'
#' @examples
#' \dontrun{
#' # Baixar ajustes de 2019
#' caged_adjustments_load(years = 2019, months = seq_len(12L), db_path = "caged.duckdb")
#'
#' # Série completa de ajustes
#' caged_adjustments_load(years = 2010:2019, db_path = "caged.duckdb")
#'
#' # Listar competências disponíveis antes de baixar
#' caged_ftp_files(type = "ajustes")
#'
#' # Consultar após carregar
#' con <- caged_connect("caged.duckdb")
#' dplyr::tbl(con, "caged_ajustes") |>
#'   dplyr::group_by(competencia) |>
#'   dplyr::summarise(
#'     saldo       = sum(saldomovimentacao, na.rm = TRUE),
#'     n_registros = dplyr::n()
#'   ) |>
#'   dplyr::arrange(competencia) |>
#'   dplyr::collect()
#' DBI::dbDisconnect(con, shutdown = TRUE)
#'
#' # Comparar antigo vs ajustes (saldo líquido ajustado)
#' con <- caged_connect("caged.duckdb")
#' antigo  <- dplyr::tbl(con, "caged_antigo")  |>
#'   dplyr::group_by(competencia) |>
#'   dplyr::summarise(saldo_original = sum(saldomovimentacao, na.rm = TRUE))
#' ajustes <- dplyr::tbl(con, "caged_ajustes") |>
#'   dplyr::group_by(competencia) |>
#'   dplyr::summarise(saldo_ajuste = sum(saldomovimentacao, na.rm = TRUE))
#' dplyr::full_join(antigo, ajustes, by = "competencia") |>
#'   dplyr::mutate(saldo_final = saldo_original + saldo_ajuste) |>
#'   dplyr::collect()
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @seealso [caged_load()] para Novo CAGED (2020+) e CAGED antigo (até 2019).
#' @seealso [caged_ftp_files()] com `type = "ajustes"` para listar disponibilidade.
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

  # Ajustes so existem para o CAGED antigo (ate 2019)
  invalid_years <- years[years >= .ANO_CORTE]
  if (length(invalid_years) > 0) {
    cli::cli_warn(c(
      "!" = "CAGED Ajustes s\u00F3 existe para anos at\u00E9 {.val {.ANO_CORTE - 1L}}.",
      "i" = "Anos ignorados: {.val {invalid_years}}"
    ))
    years <- years[years < .ANO_CORTE]
  }

  if (length(years) == 0) {
    cli::cli_abort(
      "Nenhum ano v\u00E1lido. CAGED Ajustes cobre at\u00E9 {.val {.ANO_CORTE - 1L}}."
    )
  }

  cache <- .cache_dir(destdir)

  cli::cli_h1("caged_adjustments_load")
  cli::cli_inform(c(
    "i" = "Anos  : {years[1]}\u2013{years[length(years)]}",
    "i" = "Meses : {months[1]}\u2013{months[length(months)]}",
    "i" = "Banco : {.path {db_path}}"
  ))

  # 1. Download
  cli::cli_h2("1/3  Download")
  tasks <- .build_adjustments_tasks(years, months)

  if (nrow(tasks) == 0) {
    cli::cli_warn("Nenhuma tarefa de download gerada.")
    return(invisible(NULL))
  }

  cli::cli_inform(c(
    "i" = "{nrow(tasks)} arquivo{?s} a processar",
    "i" = "Cache em: {.path {cache}}"
  ))

  manifest <- .run_downloads(tasks, cache, force_download, timeout)

  resumo <- dplyr::count(manifest, status)
  cli::cli_h2("Resumo")
  purrr::walk2(resumo$status, resumo$n, function(s, cnt) {
    icon <- switch(s,
      baixado        = cli::col_green("\u2713"),
      cache          = cli::col_blue("\u25CB"),
      nao_encontrado = cli::col_yellow("!"),
      erro           = cli::col_red("\u2717"),
      "?"
    )
    cli::cli_inform("{icon} {s}: {cnt}")
  })

  manifest_ok <- dplyr::filter(manifest, status %in% c("baixado", "cache"))

  if (nrow(manifest_ok) == 0) {
    cli::cli_warn("Nenhum arquivo dispon\u00EDvel para processar.")
    return(invisible(NULL))
  }

  # 2. Parse e carga
  cli::cli_h2("2/3  Parse e carga no DuckDB")
  n_inserido_total <- 0L

  cli::cli_progress_bar(
    name   = "Arquivos",
    total  = nrow(manifest_ok),
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
  )

  for (i in seq_len(nrow(manifest_ok))) {
    arq <- manifest_ok$arquivo[i]

    df <- tryCatch(
      caged_parse(arq, type = "AJUSTES"),
      error = function(e) {
        cli::cli_warn("Erro em {.path {basename(arq)}}: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(df) && nrow(df) > 0) {
      n <- caged_to_duckdb(
        df,
        db_path                   = db_path,
        overwrite_competencies    = overwrite_competencies
      )
      n_inserido_total <- n_inserido_total + n
    }

    rm(df)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # 3. Resumo
  cli::cli_h2("3/3  Conclu\u00EDdo")
  cli::cli_inform(c(
    "v" = "Total inserido: {format(n_inserido_total, big.mark = ',')} registros"
  ))

  invisible(caged_info(db_path))
}

# \u2500\u2500 Helper interno \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Monta tabela de tarefas de download para o CAGED Ajustes
#' @noRd
.build_adjustments_tasks <- function(years, months) {
  today <- Sys.Date()
  rows  <- list()

  for (year in years) {
    for (month in months) {
      if (as.Date(sprintf("%04d-%02d-01", year, month)) > today) next

      mm  <- sprintf("%02d", month)
      url <- glue::glue("{.FTP_AJUSTES}/{year}/CAGEDAJUSTES_{mm}{year}.7z")

      rows[[length(rows) + 1L]] <- tibble::tibble(
        ano          = year,
        mes          = month,
        competencia  = .format_period(year, month),
        type         = "AJUSTES",
        uf           = NA_character_,
        url          = url,
        nome_arquivo = basename(url),
        subdir       = file.path("caged_ajustes", as.character(year))
      )
    }
  }

  if (length(rows) == 0L) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

