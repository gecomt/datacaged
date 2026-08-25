# availability.R
# Lista as competências disponíveis no repositório HuggingFace do CAGED

#' Lista as competências disponíveis no repositório HuggingFace
#'
#' Consulta a API do HuggingFace e retorna os meses disponíveis
#' para download, ordenados do mais recente para o mais antigo.
#'
#' @param type character. Which series to query:
#'   `"novo"` (Novo CAGED, 2020+), `"antigo"` (CAGED antigo, até 2019)
#'   ou `"ajustes"` (CAGED Ajustes, série histórica de correções).
#'   Default: `"novo"`.
#' @param n integer. Number of most recent competencies to return.
#'   Use `Inf` to return all. Default: `12`.
#' @param timeout integer. Timeout in seconds. Default: 15.
#' @param verbose logical. If TRUE, exibe tabela no console. Default: TRUE.
#'
#' @return tibble invisível com colunas `competencia` (AAAAMM),
#'   `ano`, `mes` e `url`, ordenado do mais recente para o mais antigo.
#'
#' @examples
#' \dontrun{
#' # Últimos 12 meses disponíveis (padrão)
#' caged_hf_files()
#'
#' # Todos os meses do Novo CAGED
#' caged_hf_files(n = Inf)
#'
#' # CAGED antigo
#' caged_hf_files(type = "antigo")
#'
#' # CAGED Ajustes
#' caged_hf_files(type = "ajustes")
#'
#' # Usar o resultado para baixar automaticamente o mês mais recente
#' disp <- caged_hf_files(n = 1, verbose = FALSE)
#' caged_load(
#'   years    = disp$ano,
#'   months   = disp$mes,
#'   db_path = file.path(tempdir(), "caged.duckdb")
#' )
#' }
#'
#' @seealso [caged_status()] para verificar se o repositório HF está acessível.
#' @seealso [caged_load()] para baixar após identificar as competências.
#' @export
caged_hf_files <- function(type    = "novo",
                            n       = 12,
                            timeout = 15,
                            verbose = TRUE) {
  type <- match.arg(type, c("novo", "antigo", "ajustes"))

  if (!is.numeric(n) || length(n) != 1L || (!is.infinite(n) && (is.na(n) || n < 1L))) {
    cli::cli_abort(
      "{.arg n} deve ser um inteiro positivo ou {.code Inf}, nao {.val {n}}."
    )
  }
  n <- if (is.infinite(n)) Inf else as.integer(n)

  # Pasta raiz no repositório HF para cada série
  pasta_raiz <- switch(type,
    novo    = .HF_PASTA_NOVO,
    antigo  = .HF_PASTA_ANTIGO,
    ajustes = .HF_PASTA_AJUSTES
  )

  hf_api_url <- .HF_API
  cli::cli_inform("Querying HuggingFace repository ({.url {hf_api_url}})...")

  # Obtém a lista de anos disponíveis na pasta raiz
  anos_disponiveis <- .hf_list_dir(pasta_raiz, timeout)

  if (is.null(anos_disponiveis)) {
    cli::cli_abort(c(
      "Could not access the HuggingFace repository.",
      "i" = "Check your connection with {.fn caged_status}.",
      "i" = "Repository: {.url https://huggingface.co/datasets/alexsandroprado/caged}"
    ))
  }

  ano_max <- as.integer(format(Sys.Date(), "%Y"))
  anos <- sort(
    unique(anos_disponiveis[
      !is.na(suppressWarnings(as.integer(anos_disponiveis))) &
      suppressWarnings(as.integer(anos_disponiveis)) >= 1992L &
      suppressWarnings(as.integer(anos_disponiveis)) <= ano_max
    ]),
    decreasing = TRUE
  )

  if (length(anos) == 0) {
    cli::cli_abort(
      "No year found in the HuggingFace repository for series '{type}'."
    )
  }

  # Competência máxima válida
  hoje     <- Sys.Date()
  ano_hoje <- as.integer(format(hoje, "%Y"))
  mes_hoje <- as.integer(format(hoje, "%m"))
  if (mes_hoje == 12L) {
    ano_max_comp <- ano_hoje + 1L; mes_max_comp <- 1L
  } else {
    ano_max_comp <- ano_hoje;      mes_max_comp <- mes_hoje + 1L
  }
  comp_max <- ano_max_comp * 100L + mes_max_comp

  competencias <- list()

  if (type == "novo") {
    # Novo CAGED: estrutura caged_novo/<AAAA>/<AAAAMM>/
    for (ano in anos) {
      subpastas <- .hf_list_dir(paste0(pasta_raiz, "/", ano), timeout)
      if (is.null(subpastas)) next

      comps <- sort(
        unique(subpastas[grepl("^\\d{6}$", subpastas)]),
        decreasing = TRUE
      )

      for (comp in comps) {
        a <- as.integer(substr(comp, 1, 4))
        m <- as.integer(substr(comp, 5, 6))
        if (is.na(a) || is.na(m) || m < 1L || m > 12L) next
        if (as.integer(comp) > comp_max) next

        competencias[[length(competencias) + 1L]] <- tibble::tibble(
          competencia = comp,
          ano         = a,
          mes         = m,
          url         = glue::glue("{.HF_BASE}/{pasta_raiz}/{ano}/{comp}")
        )
        if (!is.infinite(n) && length(competencias) >= n) break
      }
      if (!is.infinite(n) && length(competencias) >= n) break
    }

  } else {
    # CAGED antigo / Ajustes: estrutura <pasta>/<AAAA>/<arquivos>.7z
    padrao <- if (type == "ajustes") "^CAGEDEST_AJUSTES_\\d{6}\\.7z$" else "^CAGEDEST_\\d{6}\\.7z$"
    prefixo_len <- if (type == "ajustes") 17L else 9L

    # Para ajustes: incluir pasta especial "2002a2009" e limitar a <= 2019
    anos_busca <- if (type == "ajustes") {
      pastas_disponiveis <- anos_disponiveis[
        grepl("^\\d{4}$", anos_disponiveis) |
        grepl("^\\d{4}a\\d{4}$", anos_disponiveis)
      ]
      # Filtrar apenas anos válidos para ajustes (<= 2019)
      pastas_disponiveis <- pastas_disponiveis[
        !grepl("^\\d{4}$", pastas_disponiveis) |
        suppressWarnings(as.integer(pastas_disponiveis)) <= 2019L
      ]
      sort(unique(pastas_disponiveis), decreasing = TRUE)
    } else {
      anos
    }

    for (ano in anos_busca) {
      arquivos <- .hf_list_dir(paste0(pasta_raiz, "/", ano), timeout)
      if (is.null(arquivos) || length(arquivos) == 0L) next

      # Padrão mensal: CAGEDEST_AJUSTES_MMAAAA.7z
      # Padrão anual (2002a2009): CAGEDEST_AJUSTES_AAAA.7z
      padrao_anual <- "^CAGEDEST_AJUSTES_\\d{4}\\.7z$"
      todos <- arquivos[grepl(padrao, arquivos, ignore.case = TRUE)]

      # Para pasta 2002a2009 (arquivos anuais sem mês)
      if (length(todos) == 0L && grepl("a", ano)) {
        todos_anuais <- arquivos[grepl(padrao_anual, arquivos, ignore.case = TRUE)]
        if (length(todos_anuais) > 0L) {
          for (arq in todos_anuais) {
            aaaa <- substr(arq, 18L, 21L)  # "CAGEDEST_AJUSTES_" = 17 chars
            if (!grepl("^\\d{4}$", aaaa)) next
            a <- as.integer(aaaa)
            if (is.na(a) || a < 1992L || a > 2019L) next
            # Usar mes=0 para indicar arquivo anual
            for (m in 1:12) {
              competencias[[length(competencias) + 1L]] <- tibble::tibble(
                competencia = sprintf("%d%02d", a, m),
                ano         = a,
                mes         = as.integer(m),
                url         = glue::glue("{.HF_BASE}/{pasta_raiz}/{ano}/{arq}")
              )
              if (!is.infinite(n) && length(competencias) >= n) break
            }
          }
        }
        next
      }

      if (length(todos) == 0L) next

      mm_vec   <- substr(todos, prefixo_len + 1L, prefixo_len + 2L)
      aaaa_vec <- substr(todos, prefixo_len + 3L, prefixo_len + 6L)
      comps    <- sort(unique(paste0(aaaa_vec, mm_vec)), decreasing = TRUE)
      comps    <- comps[nchar(comps) == 6 & !is.na(suppressWarnings(as.integer(comps)))]

      for (comp in comps) {
        a <- as.integer(substr(comp, 1, 4))
        m <- as.integer(substr(comp, 5, 6))
        if (is.na(a) || is.na(m) || m < 1L || m > 12L) next
        if (as.integer(comp) > comp_max) next

        competencias[[length(competencias) + 1L]] <- tibble::tibble(
          competencia = comp,
          ano         = a,
          mes         = m,
          url         = glue::glue("{.HF_BASE}/{pasta_raiz}/{ano}")
        )
        if (!is.infinite(n) && length(competencias) >= n) break
      }
      if (!is.infinite(n) && length(competencias) >= n) break
    }
  }

  if (length(competencias) == 0) {
    cli::cli_warn("No competency found in the HuggingFace repository.")
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
      antigo  = "CAGED Antigo (ate 2019)",
      ajustes = "CAGED Ajustes (serie historica)"
    )
    cli::cli_h2("Available competencies - {serie}")

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
      "i" = "{nrow(resultado)} competenc{?y/ies} listed",
      "i" = "Use {.code {cmd}} to download the most recent."
    ))
  }

  invisible(resultado)
}

#' @rdname caged_hf_files
#' @export
caged_ftp_files <- function(type    = "novo",
                             n       = 12,
                             timeout = 15,
                             verbose = TRUE) {
  cli::cli_warn(c(
    "!" = "{.fn caged_ftp_files} has been renamed to {.fn caged_hf_files}.",
    "i" = "The repository is now HuggingFace (HTTPS), no longer MTE's FTP.",
    "i" = "Use {.fn caged_hf_files} to avoid this warning."
  ))
  caged_hf_files(type = type, n = n, timeout = timeout, verbose = verbose)
}

# -- Utilitário interno: lista diretório via API HuggingFace ------------------

#' Lista o conteúdo de uma pasta no repositório HuggingFace via API
#'
#' @param path Relative path within the repository (e.g. "NOVO_CAGED/2023")
#' @param timeout Timeout in seconds
#' @return character vector com nomes dos itens, ou NULL em caso de erro
#' @noRd
.hf_list_dir <- function(path, timeout = 15) {
  url <- paste0(.HF_API, "/", path)

  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
  }, error = function(e) NULL)

  if (is.null(resp)) return(NULL)
  if (httr2::resp_status(resp) != 200L) return(NULL)

  parsed <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(e) NULL
  )

  if (is.null(parsed) || length(parsed) == 0L) return(NULL)

  # A API retorna lista de objetos com campos "path", "type" ("file"/"directory"), etc.
  # Extraímos apenas o componente final do path (basename)
  nomes <- vapply(parsed, function(item) {
    p <- item[["path"]]
    if (is.null(p) || !nzchar(p)) return(NA_character_)
    basename(p)
  }, character(1L))

  nomes[!is.na(nomes) & nzchar(nomes)]
}
