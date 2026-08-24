#' Tabela de UFs brasileiras com códigos IBGE e regiões
#'
#' Data frame com as 27 unidades federativas do Brasil, seus códigos numéricos
#' do IBGE e suas regiões geográficas. Útil para joins com os microdados do
#' CAGED, que armazenam apenas o código numérico da UF.
#'
#' @format Data frame com 27 linhas e 3 colunas:
#' \describe{
#'   \item{sigla}{Sigla da UF (character), ex: "SP", "RJ", "RN".}
#'   \item{codigo}{Código numérico IBGE da UF (integer), ex: 35, 33, 24.}
#'   \item{regiao}{Região geográfica (character): "Norte", "Nordeste",
#'     "Sudeste", "Sul" ou "Centro-Oeste".}
#' }
#'
#' @source IBGE (Instituto Brasileiro de Geografia e Estatistica).
#'
#' @examples
#' data(uf_codigos)
#' head(uf_codigos)
#'
#' # Join com microdados do CAGED
#' \donttest{
#' con <- caged_connect(file.path(tempdir(), "caged.duckdb"))
#' df  <- dplyr::tbl(con, "caged_mov") |> dplyr::collect()
#' dplyr::left_join(df, uf_codigos, by = c("uf" = "codigo"))
#' }
"uf_codigos"
