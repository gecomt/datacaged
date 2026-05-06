# utils.R
# Utilit\u00E1rios internos compartilhados pelo pacote

#' Operador "null coalescing"
#' Retorna `x` se não for NULL, senão `y`.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
