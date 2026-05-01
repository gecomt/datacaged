# download.R
# Lógica de construção de URLs, download dos .7z e gestão de cache local

# ── URLs do FTP ───────────────────────────────────────────────────────────────

#' Constrói a URL FTP do arquivo no servidor do MTE
#' O servidor usa FTP puro (ftp://), não HTTPS.
#' @noRd
.ftp_url <- function(year, month, type = "MOV") {
  competencia <- .format_period(year, month)
  mm   <- sprintf("%02d", month)

  if (type == "ANTIGO") {
    # Formato real: CAGEDEST_{MM}{AAAA}.7z (sem separação por UF)
    nome <- glue::glue("CAGEDEST_{mm}{year}.7z")
    glue::glue("{.FTP_ANTIGO}/{year}/{nome}")
  } else if (type == "AJUSTES") {
    # CAGED Ajustes: CAGEDAJUSTES_{MM}{AAAA}.7z
    nome <- glue::glue("CAGEDAJUSTES_{mm}{year}.7z")
    glue::glue("{.FTP_AJUSTES}/{year}/{nome}")
  } else {
    prefixo <- switch(type,
      MOV = "CAGEDMOV",
      FOR = "CAGEDFOR",
      EXC = "CAGEDEXC",
      cli::cli_abort("Tipo inválido: {.val {type}}. Use MOV, FOR, EXC, ANTIGO ou AJUSTES.")
    )
    nome <- glue::glue("{prefixo}{competencia}.7z")
    glue::glue("{.FTP_NOVO}/{year}/{competencia}/{nome}")
  }
}

# ── Download individual ───────────────────────────────────────────────────────

#' Baixa um único arquivo .7z do FTP do MTE com retry
#'
#' Usa download.file() com método "curl" (FTP puro, porta 21).
#' Trata arquivos inexistentes (404 FTP) silenciosamente retornando NULL.
#'
#' @noRd
.download_file <- function(url, destfile, timeout = 300,
                            sleep_fn = getOption("datacaged.sleep_fn", Sys.sleep)) {
  # Tenta até 3 vezes com backoff exponencial (5 s, 10 s).
  # Em testes, defina options(datacaged.sleep_fn = function(x) invisible(NULL))
  # para pular a espera sem modificar o código de produção.
  for (tentativa in seq_len(3)) {
    resultado <- tryCatch({
      utils::download.file(
        url      = url,
        destfile = destfile,
        mode     = "wb",       # binário — essencial para .7z
        quiet    = TRUE,
        method   = "curl",
        extra    = c(
          "--connect-timeout", as.character(timeout),
          "--max-time",        as.character(timeout * 2),
          "--retry",           "2",
          "--ftp-pasv",        # modo passivo — necessário para FTP atrás de firewall
          "--silent"
        )
      )
    }, warning = function(w) {
      # download.file emite warning para 404/erros FTP — captura e retorna código
      msg <- conditionMessage(w)
      if (grepl("404|550|cannot open|não foi poss", msg, ignore.case = TRUE)) {
        return(404L)
      }
      return(1L)
    }, error = function(e) {
      return(1L)
    })

    # Arquivo inexistente no FTP (código 550 = no such file)
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
  cli::cli_warn("Falha ao baixar {.url {url}} após 3 tentativas. Pulando.")
  invisible(NULL)
}

# ── Extração dos .7z ──────────────────────────────────────────────────────────

#' Extrai um arquivo .7z para um diretório
#' Extrai arquivo comprimido com múltiplos fallbacks
#' Ordem: archive -> unzip -> 7-Zip (Rtools/PATH/Program Files)
#' @noRd
.extract_7z <- function(path, exdir) {
  if (!file.exists(path)) cli::cli_abort("Arquivo não encontrado: {.path {path}}")

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

  # Tentativa 3: 7-Zip -- necessario para PPMd (CAGED antigo)
  exe <- .find_7zip()
  if (!is.null(exe)) {
    path_norm  <- normalizePath(path,  winslash = "\\", mustWork = FALSE)
    exdir_norm <- normalizePath(exdir, winslash = "\\", mustWork = FALSE)

    # No Windows o 7-Zip exige: 7z.exe e "arquivo.7z" -o"destino" -y
    # Sem espaço entre -o e o caminho, e sem shQuote que adiciona aspas simples
    if (.Platform$OS.type == "windows") {
      cmd <- paste0(
        '"', exe, '" e "', path_norm, '" "-o', exdir_norm, '" -y'
      )
      ret <- tryCatch(
        shell(cmd, intern = TRUE, mustWork = FALSE),
        error = function(e) structure(character(0), status = 1L)
      )
      # attr("status") é NULL quando o comando termina com código 0 (convenção de shell())
      # %||% 0L captura exactamente esse caso.
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
      # system2() também retorna NULL attr quando bem-sucedido
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
    "i" = "O arquivo usa compressão PPMd (7-Zip avançado).",
    "i" = "Instale o 7-Zip: {.url https://www.7-zip.org/download.html}",
    "i" = "O instalador padrão do Windows é suficiente (sem precisar adicionar ao PATH)."
  ))
}

#' Lista arquivos .txt/.csv extraidos em um diretorio
#' @noRd
.list_text_files <- function(exdir) {
  list.files(exdir, full.names = TRUE,
             pattern = "\\.txt$|\\.csv$|\\.TXT$|\\.CSV$")
}

#' Encontra o executavel do 7-Zip
#' Verifica PATH, Rtools e Program Files (Windows)
#' @return caminho do executavel ou NULL
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

# ── Cache local ───────────────────────────────────────────────────────────────

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

# ── Função pública: caged_download ───────────────────────────────────────────

#' Baixa microdados do CAGED do FTP do MTE
#'
#' Baixa os arquivos `.7z` para um diretório local, criando uma estrutura
#' de pastas por ano. Arquivos já baixados não são re-baixados (cache local).
#'
#' Para o **Novo CAGED** (2020+), são baixados três tipos por competência:
#' movimentações (`MOV`), fora do prazo (`FOR`) e exclusões (`EXC`).
#'
#' Para o **CAGED antigo** (até 2019), o download é nacional (arquivo único por competência).
#'
#' @param years integer vector. Anos desejados (ex: `2020:2023`).
#' @param months integer vector. Meses desejados (1–12). Default: `seq_len(12L)` (todos os meses).
#' @param states character vector ou NULL. **Ignorado** — o parâmetro é validado
#'   mas não filtra downloads de nenhuma série (Novo CAGED, antigo ou Ajustes),
#'   pois todos os arquivos são de âmbito nacional. Mantido por compatibilidade.
#' @param destdir character ou NULL. Diretório local para salvar os arquivos.
#'   Se NULL, usa o diretório padrão do sistema via
#'   `tools::R_user_dir("datacaged", "cache")` (localização varia por plataforma).
#' @param force logical. Se TRUE, re-baixa mesmo arquivos já em cache.
#'   Default: FALSE.
#' @param timeout integer. Timeout por arquivo em segundos. Default: 300.
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
#' # Baixa CAGED antigo de 2018 (nacional, sem filtro por UF)
#' caged_download(years = 2018)
#'
#' # Forçar re-download mesmo com cache
#' caged_download(years = 2023, months = 1, force = TRUE)
#'
#' # Salvar em diretório personalizado
#' caged_download(years = 2023, months = 1, destdir = "D:/dados/caged")
#' }
#'
#' @seealso [caged_load()] para o pipeline completo (download + parse + DuckDB).
#' @seealso [caged_ftp_files()] para listar competências disponíveis no FTP.
#' @export
caged_download <- function(years,
                           months  = seq_len(12L),
                           states  = NULL,
                           destdir = NULL,
                           force   = FALSE,
                           timeout = 300) {
  years  <- .validate_years(years)
  months <- .validate_months(months)
  states <- .validate_states(states) %||% names(.UF_CODIGOS)
  cache <- .cache_dir(destdir)

  tasks <- .build_tasks(years, months)
  n <- nrow(tasks)

  if (n == 0) {
    cli::cli_inform("Nenhuma competência válida para baixar.")
    return(invisible(tibble::tibble()))
  }

  cli::cli_h1("Download de Microdados do CAGED")
  cli::cli_inform(c(
    "i" = "{n} arquivo{?s} a processar",
    "i" = "Cache em: {.path {cache}}"
  ))

  resultados <- vector("list", n)

  cli::cli_progress_bar(
    name   = "Baixando",
    total  = n,
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}] ETA: {cli::pb_eta_str}"
  )

  for (i in seq_len(n)) {
    t        <- tasks[i, ]
    destfile <- file.path(cache, t$subdir, t$nome_arquivo)

    if (!dir.exists(dirname(destfile))) {
      dir.create(dirname(destfile), recursive = TRUE)
    }

    status <- if (.is_cached(destfile) && !force) {
      "cache"
    } else {
      res <- .download_file(t$url, destfile, timeout)
      if (is.null(res)) "nao_encontrado"
      else if (file.exists(destfile) && file.size(destfile) > 0) "baixado"
      else "erro"
    }

    resultados[[i]] <- tibble::tibble(
      arquivo     = destfile,
      competencia = t$competencia,
      type        = t$type,
      ano         = t$ano,
      mes         = t$mes,
      uf          = t$uf,
      status      = status
    )

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  resultado_final <- dplyr::bind_rows(resultados)

  # Exibe resumo colorido
  resumo <- dplyr::count(resultado_final, status)
  cli::cli_h2("Resumo")
  purrr::walk2(resumo$status, resumo$n, function(s, cnt) {
    icon <- switch(s,
      baixado        = cli::col_green("✓"),
      cache          = cli::col_blue("○"),
      nao_encontrado = cli::col_yellow("!"),
      erro           = cli::col_red("✗"),
      "?"
    )
    cli::cli_inform("{icon} {s}: {cnt}")
  })

  invisible(resultado_final)
}


#' Executa o download de uma tabela de tarefas e retorna o manifest
#' Função interna reutilizável por caged_download e caged_adjustments_load
#' @noRd
.run_downloads <- function(tasks, cache, force, timeout) {
  n <- nrow(tasks)
  resultados <- vector("list", n)

  cli::cli_progress_bar(
    name   = "Baixando",
    total  = n,
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}] ETA: {cli::pb_eta_str}"
  )

  for (i in seq_len(n)) {
    t        <- tasks[i, ]
    destfile <- file.path(cache, t$subdir, t$nome_arquivo)

    if (!dir.exists(dirname(destfile))) {
      dir.create(dirname(destfile), recursive = TRUE)
    }

    status <- if (.is_cached(destfile) && !force) {
      "cache"
    } else {
      res <- .download_file(t$url, destfile, timeout)
      if (is.null(res)) "nao_encontrado"
      else if (file.exists(destfile) && file.size(destfile) > 0) "baixado"
      else "erro"
    }

    resultados[[i]] <- tibble::tibble(
      arquivo     = destfile,
      competencia = t$competencia,
      type        = t$type,
      ano         = t$ano,
      mes         = t$mes,
      uf          = t$uf,
      status      = status
    )

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  dplyr::bind_rows(resultados)
}

## ── Montagem da tabela de tarefas ────────────────────────────────────────────

#' Gera data.frame com todas as combinações de download a realizar
#' @noRd
.build_tasks <- function(years, months) {
  today <- Sys.Date()
  rows  <- list()

  for (year in years) {
    for (month in months) {

      # Ignora competências futuras
      if (as.Date(sprintf("%04d-%02d-01", year, month)) > today) next

      competencia <- .format_period(year, month)

      if (.is_new_caged(year, month)) {

        # NOVO CAGED (2020+): 3 tipos por competência
        for (type in c("MOV", "FOR", "EXC")) {
          url <- .ftp_url(year, month, type)
          rows[[length(rows) + 1L]] <- tibble::tibble(
            ano          = year,
            mes          = month,
            competencia  = competencia,
            type         = type,
            uf           = NA_character_,
            url          = url,
            nome_arquivo = basename(url),
            subdir       = file.path("caged_novo", as.character(year))
          )
        }

      } else {

        # CAGED ANTIGO (<= 2019): 1 arquivo nacional por competência
        url <- .ftp_url(year, month, type = "ANTIGO")
        rows[[length(rows) + 1L]] <- tibble::tibble(
          ano          = year,
          mes          = month,
          competencia  = competencia,
          type         = "ANTIGO",
          uf           = NA_character_,
          url          = url,
          nome_arquivo = basename(url),
          subdir       = file.path("caged_antigo", as.character(year))
        )

      }
    }
  }

  if (length(rows) == 0L) return(tibble::tibble())

  dplyr::bind_rows(rows)
}


