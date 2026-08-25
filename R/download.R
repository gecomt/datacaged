# download.R
# Lógica de construção de URLs, download dos .7z e gestão de cache local
# Fonte: HuggingFace Dataset alexsandroprado/caged (HTTPS)

# -- URLs do HuggingFace -------------------------------------------------------

#' Constrói a URL HTTPS do arquivo no repositório HuggingFace
#'
#' Estrutura de pastas no repositório:
#'   caged_novo/<AAAA>/<CAGEDMOV|FOR|EXC><AAAAMM>.7z
#'   caged_antigo/<AAAA>/<CAGEDEST_MM AAAA>.7z
#'   CAGED_AJUSTES/<AAAA>/CAGEDEST_AJUSTES_MMAAAA.7z
#'
#' @noRd
.hf_url <- function(year, month, type = "MOV") {
  competencia <- .format_period(year, month)
  mm          <- sprintf("%02d", month)

  if (type == "ANTIGO") {
    nome   <- glue::glue("CAGEDEST_{mm}{year}.7z")
    subdir <- glue::glue("{.HF_PASTA_ANTIGO}/{year}")
  } else if (type == "AJUSTES") {
    # Anos 2002-2009: arquivo anual em pasta "2002a2009"
    # Anos 2010-2019: arquivo mensal por ano
    if (year <= 2009L) {
      nome   <- glue::glue("CAGEDEST_AJUSTES_{year}.7z")
      subdir <- glue::glue("{.HF_PASTA_AJUSTES}/2002a2009")
    } else {
      nome   <- glue::glue("CAGEDEST_AJUSTES_{mm}{year}.7z")
      subdir <- glue::glue("{.HF_PASTA_AJUSTES}/{year}")
    }
  } else {
    prefixo <- switch(type,
      MOV = "CAGEDMOV",
      FOR = "CAGEDFOR",
      EXC = "CAGEDEXC",
      cli::cli_abort("Invalid type: {.val {type}}. Use MOV, FOR, EXC, ANTIGO or AJUSTES.")
    )
    nome   <- glue::glue("{prefixo}{competencia}.7z")
    subdir <- glue::glue("{.HF_PASTA_NOVO}/{year}/{competencia}")
  }

  glue::glue("{.HF_BASE}/{subdir}/{nome}")
}

#' Baixa um único arquivo .7z do HuggingFace com retry
#'
#' Usa httr2 para download HTTPS com timeout, retry e seguimento de redirecionamentos.
#' Trata arquivos inexistentes (HTTP 404/403) silenciosamente retornando NULL.
#'
#' @noRd
.download_file <- function(url, destfile, timeout = 300,
                            sleep_fn = getOption("datacaged.sleep_fn", Sys.sleep)) {
  # Tenta até 3 vezes com backoff exponencial (5 s, 10 s).
  for (tentativa in seq_len(3)) {
    resultado <- tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = 1L) |>    # retry interno desativado; gerenciamos aqui
        httr2::req_error(is_error = \(r) FALSE) |>  # do not raise error on 4xx/5xx
        httr2::req_perform()

      status <- httr2::resp_status(resp)

      # Arquivo não encontrado no repositório — retorna NULL
      if (status %in% c(404L, 403L)) {
        if (file.exists(destfile)) unlink(destfile)
        return(invisible(NULL))
      }

      # Qualquer outro erro HTTP
      if (status >= 400L) {
        return(1L)
      }

      # Sucesso: salvar no disco
      writeBin(httr2::resp_body_raw(resp), destfile)
      0L
    }, error = function(e) {
      1L
    })

    # Arquivo inexistente no repositório
    if (identical(resultado, 404L)) {
      if (file.exists(destfile)) unlink(destfile)
      return(invisible(NULL))
    }

    # Sucesso
    if (identical(resultado, 0L) && file.exists(destfile) && file.size(destfile) > 0) {
      return(invisible(destfile))
    }

    # Falha — espera antes de tentar novamente
    if (tentativa < 3) sleep_fn(5 * tentativa)
  }

  # Esgotou tentativas
  if (file.exists(destfile)) unlink(destfile)
  cli::cli_warn("Failed to download {.url {url}} after 3 attempts. Skipping.")
  invisible(NULL)
}

# -- Extração dos .7z ----------------------------------------------------------

#' Extrai um arquivo .7z para um diretório
#' Extrai arquivo comprimido com múltiplos fallbacks
#' Ordem: archive -> unzip -> 7-Zip (Rtools/PATH/Program Files)
#' @noRd
.extract_7z <- function(path, exdir) {
  if (!file.exists(path)) cli::cli_abort("File not found: {.path {path}}")

  # Tentativa 1: archive (libarchive) -- funciona para Novo CAGED (LZMA) e CAGED antigo (PPMd)
  ok <- tryCatch({
    suppressWarnings(archive::archive_extract(path, dir = exdir))
    TRUE
  }, error = function(e) FALSE)

  if (ok) {
    txts <- .list_text_files(exdir)
    if (length(txts) > 0) return(txts)
  }

  # Tentativa 2: utils::unzip -- ZIPs renomeados para .7z
  ok2 <- tryCatch({
    utils::unzip(path, exdir = exdir, junkpaths = TRUE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE)

  if (ok2) {
    txts <- .list_text_files(exdir)
    if (length(txts) > 0) return(txts)
  }

  # Tentativa 3: 7-Zip -- necessário para PPMd (CAGED antigo)
  exe <- .find_7zip()
  if (!is.null(exe)) {
    path_norm  <- normalizePath(path,  winslash = "\\", mustWork = FALSE)
    exdir_norm <- normalizePath(exdir, winslash = "\\", mustWork = FALSE)

    if (.Platform$OS.type == "windows") {
      cmd <- paste0(
        '"', exe, '" e "', path_norm, '" "-o', exdir_norm, '" -y'
      )
      ret <- tryCatch(
        shell(cmd, intern = TRUE, mustWork = FALSE),
        error = function(e) structure(character(0), status = 1L)
      )
      status <- attr(ret, "status") %||% 0L
      if (!is.integer(status)) status <- as.integer(status)
    } else {
      ret <- tryCatch(
        system2(exe,
                args   = c("e", shQuote(path_norm),
                           paste0("-o", shQuote(exdir_norm)), "-y"),
                stdout = TRUE, stderr = TRUE),
        error = function(e) structure(character(0), status = 1L)
      )
      status <- attr(ret, "status") %||% 0L
      if (!is.integer(status)) status <- as.integer(status)
    }

    if (status == 0L) {
      txts <- .list_text_files(exdir)
      if (length(txts) > 0) return(txts)
    }
  }

  cli::cli_abort(c(
    "Falha ao extrair {.path {basename(path)}}.",
    "i" = "O arquivo usa compressao PPMd (7-Zip avancado).",
    "i" = "Install 7-Zip: {.url https://www.7-zip.org/download.html}",
    "i" = "The standard Windows installer is sufficient (no need to add to PATH)."
  ))
}

#' Lista arquivos .txt/.csv extraídos em um diretório
#' @noRd
.list_text_files <- function(exdir) {
  list.files(exdir, full.names = TRUE,
             pattern = "\\.txt$|\\.csv$|\\.TXT$|\\.CSV$")
}

#' Encontra o executável do 7-Zip
#' Verifica PATH, Rtools e Program Files (Windows)
#' @return caminho do executável ou NULL
#' @noRd
.find_7zip <- function() {
  no_path <- Sys.which(c("7z", "7za", "7zz"))
  no_path <- no_path[nchar(no_path) > 0]
  if (length(no_path) > 0) return(unname(no_path[1]))

  if (.Platform$OS.type == "windows") {
    candidatos <- c(
      "C:/Program Files/7-Zip/7z.exe",
      "C:/Program Files (x86)/7-Zip/7z.exe",
      file.path(Sys.getenv("LOCALAPPDATA"), "Programs/7-Zip/7z.exe"),
      file.path(Sys.getenv("RTOOLS45_HOME", "C:/rtools45"), "usr/bin/7z.exe"),
      file.path(Sys.getenv("RTOOLS44_HOME", "C:/rtools44"), "usr/bin/7z.exe"),
      file.path(Sys.getenv("RTOOLS43_HOME", "C:/rtools43"), "usr/bin/7z.exe")
    )
    for (cand in candidatos) {
      if (file.exists(cand)) return(cand)
    }
  }
  NULL
}

# -- Cache local ---------------------------------------------------------------

#' Retorna o diretório de cache padrão do pacote
#' @noRd
.cache_dir <- function(destdir = NULL) {
  dir <- destdir %||% tools::R_user_dir("datacaged", "cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

#' Verifica se um arquivo já está em cache (existe e não está vazio)
#' @noRd
.is_cached <- function(destfile) {
  file.exists(destfile) && file.size(destfile) > 0
}

# -- Função pública: caged_download -------------------------------------------

#' Baixa microdados do CAGED do repositório HuggingFace
#'
#' Baixa os arquivos `.7z` para um diretório local, criando uma estrutura
#' de pastas por ano. Arquivos já baixados não são re-baixados (cache local).
#'
#' Para o **Novo CAGED** (2020+), são baixados três tipos por competência:
#' movimentações (`MOV`), fora do prazo (`FOR`) e exclusões (`EXC`).
#'
#' Para o **CAGED antigo** (até 2019), o download é nacional (arquivo único por competência).
#'
#' @param years integer vector. Desired years (ex: `2020:2023`).
#' @param months integer vector. Desired months (1–12). Default: `seq_len(12L)` (todos os meses).
#' @param states character vector or NULL. **Ignorado** — mantido por compatibilidade.
#' @param destdir character or NULL. Local directory to save os arquivos.
#'   If NULL, uses the default directory do sistema via
#'   `tools::R_user_dir("datacaged", "cache")`.
#' @param force logical. If TRUE, re-downloads files already in cache.
#'   Default: FALSE.
#' @param timeout integer. Timeout por arquivo em segundos. Default: 300.
#' @param workers integer. Number of parallel downloads. Default: 3.
#'   Use `1` for sequential mode. Controlled via
#'   `options(datacaged.workers = N)`.
#'
#' @return data.frame invisível com colunas `arquivo`, `competencia`, `type`,
#'   `uf` e `status` (baixado / cache / nao_encontrado / erro).
#'
#' @examples
#' \dontrun{
#' # Baixa Novo CAGED de jan-mar/2023
#' manifest <- caged_download(years = 2023, months = c(1L, 2L, 3L))
#'
#' # Ver status de cada arquivo (baixado / cache / nao_encontrado / erro)
#' dplyr::count(manifest, status)
#'
#' # Baixa CAGED antigo de 2018
#' caged_download(years = 2018)
#'
#' # Forçar re-download mesmo com cache
#' caged_download(years = 2023, months = 1, force = TRUE)
#'
#' # Salvar em diretório personalizado
#' caged_download(years = 2023, months = 1, destdir = file.path(tempdir(), "caged_cache"))
#' }
#'
#' @details
#' **Compatibilidade entre plataformas:**
#'
#' | Recurso | Windows | macOS | Linux |
#' |---|---|---|---|
#' | Download (HTTPS) | ✅ | ✅ | ✅ |
#' | Downloads paralelos | ✅ | ✅ | ✅ |
#' | Novo CAGED (2020+, LZMA) | ✅ | ✅ | ✅ |
#' | CAGED antigo (PPMd) | ⚠️ | ⚠️ | ⚠️ |
#'
#' O CAGED antigo (pré-2020) usa PPMd e requer 7-Zip instalado.
#' O Novo CAGED usa LZMA e funciona em todas as plataformas sem dependências extras.
#'
#' @seealso [caged_load()] para o pipeline completo (download + parse + DuckDB).
#' @seealso [caged_hf_files()] para listar competências disponíveis no HuggingFace.
#' @export
caged_download <- function(years,
                           months  = seq_len(12L),
                           states  = NULL,
                           destdir = NULL,
                           force   = FALSE,
                           timeout = 300,
                           workers = getOption("datacaged.workers", min(3L, future::availableCores()))) {
  years  <- .validate_years(years)
  months <- .validate_months(months)
  states <- .validate_states(states) %||% names(.UF_CODIGOS)
  cache  <- .cache_dir(destdir)

  tasks <- .build_tasks(years, months)
  n     <- nrow(tasks)

  if (n == 0) {
    cli::cli_inform("No valid competency to download.")
    return(invisible(tibble::tibble()))
  }

  workers <- max(1L, min(as.integer(workers), n))

  cli::cli_h1("Download CAGED Microdata")
  cli::cli_inform(c(
    "i" = "{n} file{?s} to process",
    "i" = paste0("Fonte: ", .HF_BASE),
    "i" = "Cache: {.path {cache}}",
    "i" = "Workers: {workers}"
  ))

  resultado_final <- .run_downloads(tasks, cache, force, timeout, workers)

  resumo <- dplyr::count(resultado_final, status)
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

  invisible(resultado_final)
}


#' Executa o download de uma tabela de tarefas e retorna o manifest
#' Função interna reutilizável por caged_download e caged_adjustments_load
#' @noRd
.run_downloads <- function(tasks, cache, force, timeout, workers = 1L) {
  n <- nrow(tasks)

  # Pré-cria todos os diretórios antes de qualquer download (evita race condition)
  destfiles <- file.path(cache, tasks$subdir, tasks$nome_arquivo)
  dirs      <- unique(dirname(destfiles))
  for (d in dirs) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  # Função que processa uma única tarefa
  .processar_tarefa <- function(i) {
    t        <- tasks[i, ]
    destfile <- destfiles[i]

    status <- if (.is_cached(destfile) && !force) {
      "cache"
    } else {
      res <- .download_file(t$url, destfile, timeout)
      if (is.null(res)) "nao_encontrado"
      else if (file.exists(destfile) && file.size(destfile) > 0) "baixado"
      else "erro"
    }

    tibble::tibble(
      arquivo     = destfile,
      competencia = t$competencia,
      type        = t$type,
      ano         = t$ano,
      mes         = t$mes,
      uf          = t$uf,
      status      = status
    )
  }

  if (workers <= 1L) {
    # Sequencial com barra de progresso cli
    resultados <- vector("list", n)
    cli::cli_progress_bar(
      name   = "Baixando",
      total  = n,
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}] ETA: {cli::pb_eta_str}"
    )
    for (i in seq_len(n)) {
      resultados[[i]] <- .processar_tarefa(i)
      cli::cli_progress_update()
    }
    cli::cli_progress_done()
  } else {
    # Paralelo com furrr + progressr
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)

    progressr::with_progress({
      p <- progressr::progressor(steps = n)
      resultados <- furrr::future_map(
        seq_len(n),
        function(i) {
          on.exit(p())   # garante p() mesmo em caso de erro
          .processar_tarefa(i)
        },
        .options = furrr::furrr_options(seed = NULL)
      )
    })
  }

  dplyr::bind_rows(resultados)
}

## -- Montagem da tabela de tarefas --------------------------------------------

#' Gera data.frame com todas as combinações de download a realizar
#' @noRd
.build_tasks <- function(years, months) {
  today <- Sys.Date()
  rows  <- list()

  for (year in years) {
    for (month in months) {

      # Ignora competencias futuras
      if (as.Date(sprintf("%04d-%02d-01", year, month)) > today) next

      competencia <- .format_period(year, month)

      if (.is_new_caged(year, month)) {

        # NOVO CAGED (2020+): 3 tipos por competencia
        for (type in c("MOV", "FOR", "EXC")) {
          url <- .hf_url(year, month, type)
          rows[[length(rows) + 1L]] <- tibble::tibble(
            ano          = year,
            mes          = month,
            competencia  = competencia,
            type         = type,
            uf           = NA_character_,
            url          = url,
            nome_arquivo = basename(url),
            subdir       = file.path(.HF_PASTA_NOVO, as.character(year), competencia)
          )
        }

      } else {

        # CAGED ANTIGO (<= 2019): 1 arquivo nacional por competencia
        url <- .hf_url(year, month, type = "ANTIGO")
        rows[[length(rows) + 1L]] <- tibble::tibble(
          ano          = year,
          mes          = month,
          competencia  = competencia,
          type         = "ANTIGO",
          uf           = NA_character_,
          url          = url,
          nome_arquivo = basename(url),
          subdir       = file.path(.HF_PASTA_ANTIGO, as.character(year))
        )

      }
    }
  }

  if (length(rows) == 0L) return(tibble::tibble())

  dplyr::bind_rows(rows)
}
