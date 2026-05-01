# availability.R
# Lista as competências disponíveis no FTP do MTE

#' Lista as competências disponíveis no FTP do MTE
#'
#' Consulta o servidor FTP do MTE e retorna os meses disponíveis
#' para download, ordenados do mais recente para o mais antigo.
#'
#' @param type character. Qual série consultar:
#'   `"novo"` (Novo CAGED, 2020+), `"antigo"` (CAGED antigo, até 2019)
#'   ou `"ajustes"` (CAGED Ajustes, série histórica de correções).
#'   Default: `"novo"`.
#' @param n integer. Número de competências mais recentes a retornar.
#'   Use `Inf` para retornar todas. Default: `12`.
#' @param timeout integer. Timeout em segundos. Default: 15.
#' @param verbose logical. Se TRUE, exibe table no console. Default: TRUE.
#'
#' @return tibble invisível com colunas `competencia` (AAAAMM),
#'   `ano`, `mes` e `url`, ordenado do mais recente para o mais antigo.
#'
#' @examples
#' \dontrun{
#' # Últimos 12 meses disponíveis (padrão)
#' caged_ftp_files()
#'
#' # Todos os meses do Novo CAGED
#' caged_ftp_files(n = Inf)
#'
#' # CAGED antigo
#' caged_ftp_files(type = "antigo")
#'
#' # CAGED Ajustes
#' caged_ftp_files(type = "ajustes")
#'
#' # Usar o resultado para baixar automaticamente o mês mais recente
#' disp <- caged_ftp_files(n = 1, verbose = FALSE)
#' caged_load(
#'   years    = disp$ano,
#'   months   = disp$mes,
#'   db_path = "caged.duckdb"
#' )
#' }
#'
#' @seealso [caged_status()] para verificar se o FTP está online.
#' @seealso [caged_load()] para baixar após identificar as competências.
#' @export
caged_ftp_files <- function(type    = "novo",
                                    n       = 12,
                                    timeout = 15,
                                    verbose = TRUE) {
  type <- match.arg(type, c("novo", "antigo", "ajustes"))

  # Valida n: deve ser numérico positivo ou Inf
  # n=NA causa crash em is.infinite(NA) → if(NA) → erro sem contexto
  # n<=0 produziria comportamento inesperado em head()
  if (!is.numeric(n) || length(n) != 1L || (!is.infinite(n) && (is.na(n) || n < 1L))) {
    cli::cli_abort(
      "{.arg n} deve ser um inteiro positivo ou {.code Inf}, não {.val {n}}."
    )
  }
  n <- if (is.infinite(n)) Inf else as.integer(n)

  url_base <- switch(type,
    novo    = .FTP_NOVO,
    antigo  = .FTP_ANTIGO,
    ajustes = .FTP_AJUSTES
  )

  cli::cli_inform("Consultando FTP do MTE...")

  # Lista o diretório raiz do FTP para obter os years disponíveis
  anos_raw <- .ftp_list(url_base, timeout)

  if (is.null(anos_raw)) {
    cli::cli_abort(c(
      "Não foi possível acessar o FTP do MTE.",
      "i" = "Verifique sua conexão com {.fn caged_status}."
    ))
  }

  # Sanitiza linhas: converte para ASCII ignorando erros de encoding
  # O FTP retorna PDFs/XLSXs com nomes em Latin-1 que quebram o regex
  anos_raw <- iconv(anos_raw, from = "", to = "ASCII", sub = "")
  anos_raw <- anos_raw[!is.na(anos_raw) & nchar(trimws(anos_raw)) > 0]

  # Extrai apenas linhas que sejam exatamente um ano (4 dígitos)
  years <- suppressWarnings(as.integer(trimws(anos_raw)))
  ano_max <- as.integer(format(Sys.Date(), "%Y"))
  years   <- sort(unique(years[!is.na(years) &
                               years >= 1992L &
                               years <= ano_max]),
               decreasing = TRUE)

  if (length(years) == 0) {
    cli::cli_abort("Nenhum ano encontrado no FTP. Verifique a URL: {.url {url_base}}")
  }

  # Competência máxima válida = próximo mês (dados podem já estar publicados).
  # Calculado com aritmética explícita de ano/mês para evitar o "mês 13"
  # que ocorreria ao fazer as.integer(format(hoje, "%Y%m")) + 1L em dezembro.
  hoje     <- Sys.Date()
  ano_hoje <- as.integer(format(hoje, "%Y"))
  mes_hoje <- as.integer(format(hoje, "%m"))
  if (mes_hoje == 12L) {
    ano_max <- ano_hoje + 1L; mes_max <- 1L
  } else {
    ano_max <- ano_hoje;      mes_max <- mes_hoje + 1L
  }
  comp_max <- ano_max * 100L + mes_max

  competencias <- list()

  if (type == "novo") {
    # ── Novo CAGED: estrutura /AAAA/AAAAMM/ ─────────────────────────────
    for (ano in years) {
      url_ano <- paste0(url_base, "/", ano)
      linhas  <- .ftp_list(url_ano, timeout)
      if (is.null(linhas)) next

      comps <- regmatches(linhas, regexpr("\\b\\d{6}\\b", linhas))
      comps <- sort(unique(comps[nchar(comps) == 6]), decreasing = TRUE)

      for (comp in comps) {
        a <- as.integer(substr(comp, 1, 4))
        m <- as.integer(substr(comp, 5, 6))
        if (is.na(a) || is.na(m) || m < 1 || m > 12) next
        if (as.integer(comp) > comp_max) next

        competencias[[length(competencias) + 1L]] <- tibble::tibble(
          competencia = comp, ano = a, mes = m,
          url = paste0(url_ano, "/", comp)
        )
        if (!is.infinite(n) && length(competencias) >= n) break
      }
      if (!is.infinite(n) && length(competencias) >= n) break
    }

  } else {
    # ── CAGED antigo / Ajustes: estrutura /AAAA/ com arquivos por competência
    # Antigo:  CAGEDEST_{MM}{AAAA}.7z
    # Ajustes: CAGEDAJUSTES_{MM}{AAAA}.7z
    padrao <- if (type == "ajustes") "CAGEDAJUSTES_\\d{6}\\.7z" else "CAGEDEST_\\d{6}\\.7z"
    prefixo_len <- if (type == "ajustes") 13L else 9L  # nchar("CAGEDAJUSTES_") = 13, nchar("CAGEDEST_") = 9

    for (ano in years) {
      url_ano <- paste0(url_base, "/", ano)
      linhas  <- .ftp_list(url_ano, timeout)
      if (is.null(linhas) || length(linhas) == 0) next

      todos <- unlist(regmatches(linhas, gregexpr(padrao, linhas, ignore.case = TRUE)))
      if (length(todos) == 0) next

      # Extrai MM e AAAA pelas posições após o prefixo
      mm_vec   <- substr(todos, prefixo_len + 1L, prefixo_len + 2L)
      aaaa_vec <- substr(todos, prefixo_len + 3L, prefixo_len + 6L)
      comps    <- sort(unique(paste0(aaaa_vec, mm_vec)), decreasing = TRUE)
      comps    <- comps[nchar(comps) == 6 & !is.na(suppressWarnings(as.integer(comps)))]

      for (comp in comps) {
        a <- as.integer(substr(comp, 1, 4))
        m <- as.integer(substr(comp, 5, 6))
        if (is.na(a) || is.na(m) || m < 1 || m > 12) next
        if (as.integer(comp) > comp_max) next

        competencias[[length(competencias) + 1L]] <- tibble::tibble(
          competencia = comp, ano = a, mes = m,
          url = url_ano
        )
        if (!is.infinite(n) && length(competencias) >= n) break
      }
      if (!is.infinite(n) && length(competencias) >= n) break
    }
  }

  if (length(competencias) == 0) {
    cli::cli_warn("Nenhuma competência encontrada no FTP.")
    return(invisible(tibble::tibble()))
  }

  resultado <- dplyr::bind_rows(competencias) |>
    dplyr::arrange(dplyr::desc(competencia))

  if (!is.infinite(n)) {
    resultado <- utils::head(resultado, n)
  }

  if (verbose && nrow(resultado) > 0L) {
    serie <- switch(type,
      novo    = "Novo CAGED (2020+)",
      antigo  = "CAGED Antigo (até 2019)",
      ajustes = "CAGED Ajustes (série histórica)"
    )
    cli::cli_h2("Competências disponíveis — {serie}")

    meses_fmt <- c("Jan","Fev","Mar","Abr","Mai","Jun",
                   "Jul","Ago","Set","Out","Nov","Dez")

    for (i in seq_len(nrow(resultado))) {
      r <- resultado[i, ]
      cli::cli_inform(
        "  {cli::col_cyan(r$competencia)}  {meses_fmt[r$mes]}/{r$ano}"
      )
    }

    yr <- resultado$ano[1]
    mo <- resultado$mes[1]
    fn <- if (type == "ajustes") "caged_adjustments_load" else "caged_load"
    cmd <- paste0(fn, "(years = ", yr, ", months = ", mo, ")")

    cli::cli_inform(c(
      "i" = "{nrow(resultado)} competência{?s} listada{?s}",
      "i" = "Use {.code {cmd}} para baixar a mais recente."
    ))
  }

  invisible(resultado)
}

# ── Utilitário interno: lista diretório FTP ───────────────────────────────────

#' Lista o conteúdo de um diretório FTP do MTE
#' @param url URL do diretório FTP
#' @param timeout Timeout em segundos
#' @return character vector com as linhas retornadas, ou NULL em caso de erro
#' @noRd
.ftp_list <- function(url, timeout = 15) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  ok <- tryCatch({
    code <- utils::download.file(
      url      = paste0(url, "/"),
      destfile = tmp,
      mode     = "w",
      quiet    = TRUE,
      method   = "curl",
      extra    = c(
        "--connect-timeout", as.character(timeout),
        "--max-time",        as.character(timeout),
        "--ftp-pasv",
        "--silent",
        "--list-only"
      )
    )
    code == 0L
  }, warning = function(w) FALSE,
     error   = function(e) FALSE)

  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) return(NULL)

  linhas <- tryCatch(
    readLines(tmp, warn = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )

  if (is.null(linhas) || length(linhas) == 0) return(NULL)
  linhas
}

