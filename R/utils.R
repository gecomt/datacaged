# utils.R
# Utilitarios internos compartilhados pelo pacote

#' Operador "null coalescing"
#' Retorna `x` se não for NULL, senão `y`.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Remove acentos de forma independente do locale
#'
#' Usa chartr() com os caracteres em escapes Unicode. Nao usar acentos como
#' NOMES de vetor (c("\u00e1" = "a")): nomes sao simbolos e sao convertidos
#' para o encoding nativo na instalacao, virando "<U+00E1>" em locale C.
#' @noRd
.remove_acentos <- function(x) {
  com <- paste0(
    "\u00e1\u00e0\u00e3\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef",
    "\u00f3\u00f2\u00f5\u00f4\u00f6\u00fa\u00f9\u00fb\u00fc\u00e7\u00f1",
    "\u00c1\u00c0\u00c3\u00c2\u00c4\u00c9\u00c8\u00ca\u00cb\u00cd\u00cc\u00ce\u00cf",
    "\u00d3\u00d2\u00d5\u00d4\u00d6\u00da\u00d9\u00db\u00dc\u00c7\u00d1"
  )
  sem <- paste0(
    "aaaaaeeeeiiiiooooouuuucn",
    "AAAAAEEEEIIIIOOOOOUUUUCN"
  )
  chartr(com, sem, enc2utf8(as.character(x)))
}
