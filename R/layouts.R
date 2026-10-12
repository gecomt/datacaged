#' Baixa layouts (dicionários) do CAGED e Novo CAGED
#'
#' Lista os dicionários de variáveis disponíveis no repositório HuggingFace
#' (`alexsandroprado/caged`), na pasta raiz de cada série, e baixa todos os
#' arquivos `.xls` ou `.xlsx` cujo nome contenha a palavra "layout"
#' (case-insensitive).
#'
#' Os arquivos são gravados com o nome original sem acentos, para evitar
#' problemas de encoding em sistemas de arquivos e locales não-UTF-8; o nome
#' original fica na coluna `arquivo` do resultado.
#'
#' @param type character. Qual base consultar: `"antigo"`, `"novo"`,
#'   `"ajustes"` ou `"ambos"` (todas). Default `"ambos"`.
#' @param destdir character or NULL. Local directory to save os arquivos.
#'   Se `NULL`, usa `tools::R_user_dir("datacaged", "cache")/layouts`.
#' @param force logical. If `TRUE`, re-downloads files that already exist.
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
#' @seealso [caged_status()] para verificar conectividade.
#' @seealso [caged_hf_files()] para consultar os microdados disponíveis.
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

  pastas <- c(
    antigo  = .HF_PASTA_ANTIGO,
    novo    = .HF_PASTA_NOVO,
    ajustes = .HF_PASTA_AJUSTES
  )

  bases <- if (type == "ambos") names(pastas) else type

  resultados <- list()

  if (verbose) {
    cli::cli_h1("caged_download_layouts")
    cli::cli_inform(c(
      "i" = gettext("Bases: {.val {paste(bases, collapse = ', ')}}"),
      "i" = gettext("Cache at {.path {cache}}")
    ))
  }

  for (base_nome in bases) {
    pasta <- pastas[[base_nome]]

    if (verbose) {
      cli::cli_h2(gettext("Listing - {.val {base_nome}}"))
    }

    tarefas <- .list_layout_files(pasta, timeout = timeout)

    if (is.null(tarefas)) {
      cli::cli_warn(c(
        gettext("Could not list {.val {pasta}} on HuggingFace."),
        "i" = gettext("Check your connection with {.fn caged_status}.")
      ))
      next
    }

    if (nrow(tarefas) == 0) {
      if (verbose) cli::cli_warn(gettext("No layout found in {.val {pasta}}."))
      next
    }

    tarefas$base    <- base_nome
    tarefas$destino <- file.path(cache, base_nome, .remove_acentos(tarefas$arquivo))

    if (verbose) {
      cli::cli_inform(c("v" = gettext("{nrow(tarefas)} file{?s} found.")))
    }

    dir.create(file.path(cache, base_nome), recursive = TRUE, showWarnings = FALSE)

    status <- vapply(seq_len(nrow(tarefas)), function(i) {
      destfile <- tarefas$destino[i]
      if (file.exists(destfile) && file.size(destfile) > 0 && !force) {
        return("cache")
      }
      ok <- .download_file(tarefas$url[i], destfile, timeout = timeout)
      if (is.null(ok)) "naoencontrado" else "baixado"
    }, character(1))

    resultados[[length(resultados) + 1L]] <- tibble::tibble(
      base    = tarefas$base,
      arquivo = tarefas$arquivo,
      url     = tarefas$url,
      destino = tarefas$destino,
      status  = status
    )
  }

  if (length(resultados) == 0) {
    if (verbose) cli::cli_warn(gettext("No layout found to download."))
    return(invisible(tibble::tibble()))
  }

  out <- dplyr::bind_rows(resultados)

  if (verbose) {
    resumo <- dplyr::count(out, status)
    cli::cli_h2(gettext("Summary"))
    purrr::walk2(
      resumo$status,
      resumo$n,
      function(s, cnt) {
        icon <- switch(
          s,
          baixado       = cli::col_green("v"),
          cache         = cli::col_blue("i"),
          naoencontrado = cli::col_yellow("!"),
          "*"
        )
        cli::cli_inform("{icon} {s}: {cnt}")
      }
    )
  }

  invisible(out)
}

#' Lista os layouts (.xls/.xlsx com "layout" no nome) de uma pasta do HF
#' @param pasta character. Pasta raiz da serie no repositorio HuggingFace.
#' @param timeout integer. Timeout in seconds.
#' @return tibble com colunas `arquivo` e `url`, ou NULL se a listagem falhar.
#' @noRd
.list_layout_files <- function(pasta, timeout = 15) {
  nomes <- .hf_list_dir(pasta, timeout = timeout)
  if (is.null(nomes)) return(NULL)

  nomes <- unique(nomes[grepl("\\.xlsx?$", nomes, ignore.case = TRUE) &
                          grepl("layout", nomes, ignore.case = TRUE)])

  tibble::tibble(
    arquivo = nomes,
    url     = paste0(.HF_BASE, "/", pasta, "/",
                     vapply(nomes, function(n) utils::URLencode(enc2utf8(n), reserved = TRUE),
                            character(1), USE.NAMES = FALSE))
  )
}
