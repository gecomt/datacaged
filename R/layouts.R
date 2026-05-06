#' Baixa layouts (dicionários) do CAGED e Novo CAGED
#'
#' Lista dinamicamente o diretório FTP de cada base e baixa todos os arquivos
#' `.xls` ou `.xlsx` cujo nome contenha a palavra "layout" (case-insensitive).
#'
#' @param type character. Qual base consultar: `"antigo"`, `"novo"`,
#'   `"ajustes"` ou `"ambos"`. Default `"ambos"`.
#' @param destdir character ou NULL. Diretório local para salvar os arquivos.
#'   Se `NULL`, usa `tools::R_user_dir("datacaged", "cache")/layouts`.
#' @param force logical. Se `TRUE`, re-baixa mesmo arquivos já existentes.
#'   Default `FALSE`.
#' @param timeout integer. Timeout por arquivo em segundos. Default `120`.
#' @param verbose logical. Se `TRUE`, exibe mensagens no console. Default `TRUE`.
#'
#' @return tibble invisível com colunas `base`, `arquivo`, `url`, `destino`
#'   e `status` (`"baixado"`, `"cache"` ou `"naoencontrado"`).
#'
#' @examples
#' \dontrun{
#' caged_download_layouts()
#' caged_download_layouts(type = "antigo")
#' caged_download_layouts(type = "novo")
#' caged_download_layouts(type = "ajustes")
#' }
#'
#' @seealso [caged_status()] para verificar se o FTP do MTE está online.
#' @seealso [caged_ftp_files()] para consultar os microdados disponíveis.
#' @export
caged_download_layouts <- function(type    = "ambos",
                                   destdir = NULL,
                                   force   = FALSE,
                                   timeout = 120,
                                   verbose = TRUE) {
  type <- match.arg(type, c("antigo", "novo", "ajustes", "ambos"))
  
  cache <- if (!is.null(destdir)) {
    file.path(destdir, "layouts")
  } else {
    file.path(tools::R_user_dir("datacaged", "cache"), "layouts")
  }
  
  if (!dir.exists(cache)) {
    dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  }
  
  ftp_dirs <- c(
    antigo  = "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED/",
    novo    = "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/",
    ajustes = "ftp://ftp.mtps.gov.br/pdet/microdados/CAGED_AJUSTES/"
  )
  
  bases <- if (type == "ambos") names(ftp_dirs) else type
  
  resultados <- list()
  
  if (verbose) {
    cli::cli_h1("caged_download_layouts")
    cli::cli_inform(c(
      "i" = "Bases: {.val {paste(bases, collapse = ', ')}}",
      "i" = "Cache em {.path {cache}}"
    ))
  }
  
  for (base_nome in bases) {
    url_dir <- ftp_dirs[[base_nome]]
    
    if (verbose) {
      cli::cli_h2("Listando - {.val {base_nome}}")
      cli::cli_inform(c("i" = "Consultando {.url {url_dir}}"))
    }
    
    tarefas <- .list_layout_urls(url_dir, timeout = timeout)
    
    if (nrow(tarefas) == 0) {
      if (verbose) cli::cli_warn("Nenhum layout encontrado em {.url {url_dir}}.")
      next
    }
    
    tarefas$base    <- base_nome
    tarefas$destino <- file.path(cache, base_nome, tarefas$arquivo)
    
    if (verbose) {
      cli::cli_inform(c("v" = "{nrow(tarefas)} arquivo(s) encontrado(s)."))
    }
    
    n     <- nrow(tarefas)
    bloco <- vector("list", n)
    
    if (verbose) {
      cli::cli_progress_bar(
        name   = paste("Baixando", base_nome),
        total  = n,
        format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cli::pb_elapsed} ETA {cli::pb_eta_str}"
      )
    }
    
    for (i in seq_len(n)) {
      destfile <- tarefas$destino[i]
      
      if (!dir.exists(dirname(destfile))) {
        dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      }
      
      status <- if (file.exists(destfile) && file.size(destfile) > 0 && !force) {
        "cache"
      } else {
        .try_download(
          urls     = tarefas$url_lista[[i]],
          destfile = destfile,
          timeout  = timeout
        )
      }
      
      bloco[[i]] <- tibble::tibble(
        base    = tarefas$base[i],
        arquivo = tarefas$arquivo[i],
        url     = tarefas$url_lista[[i]][1],
        destino = tarefas$destino[i],
        status  = status
      )
      
      if (verbose) cli::cli_progress_update()
    }
    
    if (verbose) cli::cli_progress_done()
    resultados[[length(resultados) + 1L]] <- dplyr::bind_rows(bloco)
  }
  
  if (length(resultados) == 0) {
    if (verbose) cli::cli_warn("Nenhum layout encontrado para download.")
    return(invisible(tibble::tibble()))
  }
  
  out <- dplyr::bind_rows(resultados)
  
  if (verbose) {
    resumo <- dplyr::count(out, status)
    cli::cli_h2("Resumo")
    purrr::walk2(
      resumo$status,
      resumo$n,
      function(s, cnt) {
        icon <- switch(
          s,
          baixado       = cli::col_green("\u2714"),
          cache         = cli::col_blue("\u2139"),
          naoencontrado = cli::col_yellow("!"),
          erro          = cli::col_red("\u2716"),
          "\u2022"
        )
        cli::cli_inform("{icon} {s}: {cnt}")
      }
    )
  }
  
  invisible(out)
}

# Percent-encode byte a byte em Latin-1.
# Necess\u00E1rio para nomes FTP com acentos no servidor do MTE.
# @param s character scalar em UTF-8.
# @return character scalar percent-encoded.
# @noRd
.pct_encode_latin1 <- function(s) {
  raw_vec <- iconv(s, from = "UTF-8", to = "latin1", toRaw = TRUE)[[1]]

  # Guard: iconv returns NA when the string contains chars not representable
  # in Latin-1 (e.g. '\u20AC', '\u00A9'). FTP filenames from MTE are always Portuguese
  # (Latin-1 safe) but we protect against unexpected server behaviour.
  if (anyNA(raw_vec)) {
    return(utils::URLencode(s, repeated = TRUE))
  }

  # Caracteres seguros para URLs (RFC 3986 unreserved + ponto):
  # A-Z (65-90), a-z (97-122), 0-9 (48-57), - (45), . (46), _ (95), ~ (126)
  safe <- c(45L, 46L, 95L, 126L,
            48L:57L,    # 0-9
            65L:90L,    # A-Z
            97L:122L)   # a-z

  paste0(
    vapply(as.integer(raw_vec), function(b) {
      if (b %in% safe) rawToChar(as.raw(b)) else sprintf("%%%02X", b)
    }, character(1)),
    collapse = ""
  )
}

# Lista um diret\u00F3rio FTP e retorna tibble com arquivos xls/xlsx
# cujo nome contenha "layout" (case-insensitive).
# @param url_dir character. URL do diret\u00F3rio FTP.
# @param timeout integer. Timeout em segundos.
# @return tibble com colunas `arquivo` e `url_lista`.
# @noRd
.list_layout_urls <- function(url_dir, timeout = 15) {
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  ok <- tryCatch({
    utils::download.file(
      url      = url_dir,
      destfile = tmp,
      method   = "curl",
      extra    = paste(
        "--connect-timeout", timeout,
        "--max-time", timeout,
        "--ftp-pasv", "--silent", "--list-only"
      ),
      quiet = TRUE
    )
    file.exists(tmp) && file.size(tmp) > 0
  }, error = function(e) FALSE, warning = function(w) FALSE)
  
  if (!isTRUE(ok)) {
    return(tibble::tibble(arquivo = character(), url_lista = list()))
  }
  
  raw_bytes <- readBin(tmp, what = "raw", n = file.size(tmp))
  texto <- rawToChar(raw_bytes)
  Encoding(texto) <- "latin1"
  texto_utf8 <- enc2utf8(texto)
  
  linhas <- trimws(strsplit(texto_utf8, "\r?\n", perl = TRUE)[[1]])
  linhas <- linhas[nzchar(linhas)]
  
  ok_idx <- grepl("\\.xlsx?$", linhas, ignore.case = TRUE) &
    grepl("layout", linhas, ignore.case = TRUE)
  
  nomes_utf8 <- unique(linhas[ok_idx])
  
  if (length(nomes_utf8) == 0) {
    return(tibble::tibble(arquivo = character(), url_lista = list()))
  }
  
  url_lista <- lapply(nomes_utf8, function(nome) {
    encoded <- .pct_encode_latin1(nome)
    c(paste0(url_dir, encoded))
  })
  
  tibble::tibble(
    arquivo = nomes_utf8,
    url_lista = url_lista
  )
}

# Tenta baixar um arquivo a partir de uma lista de URLs candidatas.
# @param urls character vector.
# @param destfile character.
# @param timeout integer.
# @return character. "baixado" ou "naoencontrado".
# @noRd
.try_download <- function(urls, destfile, timeout = 120) {
  for (url in urls) {
    # Tenta com curl externo diretamente (mais confi\u00E1vel para FTP bin\u00E1rio)
    ok <- tryCatch({
      curl_bin <- Sys.which("curl")
      if (nzchar(curl_bin)) {
        ret <- system2(
          command = curl_bin,
          args    = c(
            "--connect-timeout", timeout,
            "--max-time",        timeout,
            "--ftp-pasv",
            "--silent",
            "--show-error",
            "--output", shQuote(destfile),
            shQuote(url)
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        ret == 0L && file.exists(destfile) && file.size(destfile) > 0
      } else {
        FALSE
      }
    }, error = function(e) FALSE)
    
    if (isTRUE(ok)) return("baixado")
    if (file.exists(destfile)) unlink(destfile)
    
    # Fallback: utils::download.file em modo bin\u00E1rio
    metodos <- if (.Platform$OS.type == "windows") {
      c("libcurl", "auto")
    } else {
      c("libcurl", "curl", "auto")
    }
    
    for (metodo in metodos) {
      ok2 <- tryCatch({
        utils::download.file(
          url      = url,
          destfile = destfile,
          method   = metodo,
          mode     = "wb",
          quiet    = TRUE
        )
        file.exists(destfile) && file.size(destfile) > 0
      }, error = function(e) FALSE, warning = function(w) FALSE)
      
      if (isTRUE(ok2)) return("baixado")
      if (file.exists(destfile)) unlink(destfile)
    }
  }
  
  "naoencontrado"
}
