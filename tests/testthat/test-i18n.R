# Internacionalizacao: catalogo pt_BR ------------------------------------------

## Le um arquivo .mo (formato GNU gettext) e devolve msgid/msgstr
ler_mo <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  cab <- readBin(con, "integer", n = 5L, size = 4L, endian = "little")
  stopifnot(cab[1] == -1794895138L)  # 0x950412de
  n <- cab[3]; off_orig <- cab[4]; off_trad <- cab[5]
  ler_tabela <- function(off) {
    seek(con, off)
    m <- matrix(readBin(con, "integer", n = 2L * n, size = 4L, endian = "little"),
                ncol = 2L, byrow = TRUE)
    vapply(seq_len(n), function(i) {
      seek(con, m[i, 2])
      rawToChar(readBin(con, "raw", n = m[i, 1]))
    }, character(1))
  }
  res <- data.frame(msgid = ler_tabela(off_orig), msgstr = ler_tabela(off_trad))
  Encoding(res$msgstr) <- "UTF-8"
  res[nzchar(res$msgid), , drop = FALSE]
}

## Blocos {...} de nivel superior, ignorando pluralizacao do cli ({?s})
blocos_codigo <- function(x) {
  chars <- strsplit(x, "")[[1]]
  out <- character(); prof <- 0L; ini <- 0L
  for (i in seq_along(chars)) {
    if (chars[i] == "{") { if (prof == 0L) ini <- i; prof <- prof + 1L }
    if (chars[i] == "}") {
      prof <- prof - 1L
      if (prof == 0L) out <- c(out, paste(chars[ini:i], collapse = ""))
    }
  }
  sort(out[!startsWith(out, "{?")])
}

catalogo_pt <- function() {
  mo <- system.file("po", "pt_BR", "LC_MESSAGES", "R-datacaged.mo", package = "datacaged")
  skip_if(!nzchar(mo), "catalogo pt_BR nao instalado")
  ler_mo(mo)
}

test_that("o catalogo pt_BR traduz todas as mensagens", {
  cat_pt <- catalogo_pt()
  expect_gt(nrow(cat_pt), 100L)
  expect_true(all(nzchar(cat_pt$msgstr)))
})

test_that("as traducoes preservam exatamente o codigo entre chaves", {
  cat_pt <- catalogo_pt()
  divergentes <- cat_pt$msgid[!mapply(
    function(a, b) identical(blocos_codigo(a), blocos_codigo(b)),
    cat_pt$msgid, cat_pt$msgstr
  )]
  expect_equal(divergentes, character())
})

test_that("as mensagens aparecem em portugues com o R em pt_BR", {
  teste <- suppressWarnings(withr::with_language(
    "pt_BR", gettext("Invalid {.arg years} argument.", domain = "R-datacaged")
  ))
  skip_if(teste == "Invalid {.arg years} argument.",
          "troca de idioma indisponivel neste locale")

  suppressWarnings(withr::with_language("pt_BR", {
    expect_error(datacaged:::.validate_years(1991), "Anos inv\u00e1lidos: 1991")
    expect_error(datacaged:::.validate_years(1991), "Informe anos entre 2007")
  }))
  ## Fora do bloco, de volta ao ingles
  expect_error(datacaged:::.validate_years(1991), "Invalid years: 1991")
})
