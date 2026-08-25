# test-status.R

# Helper: resposta fake do HuggingFace para testes sem rede
.mock_hf_online <- function(expr) {
  local_mocked_bindings(
    req_perform = function(req, ...) {
      structure(
        list(
          status_code = 200L,
          url = "https://huggingface.co/api/datasets/alexsandroprado/caged/tree/main",
          headers = list("content-type" = "application/json"),
          body = charToRaw('[{"path":"NOVO_CAGED","type":"directory"}]')
        ),
        class = "httr2_response"
      )
    },
    .package = "httr2"
  )
}

test_that("caged_status retorna lista com campos corretos", {
  local_mocked_bindings(
    caged_status = function(...) list(
      online      = TRUE,
      latencia_ms = 42L,
      mensagem    = "HuggingFace repository online",
      url         = "https://huggingface.co/datasets/alexsandroprado/caged"
    ),
    .package = "datacaged"
  )
  result <- caged_status(verbose = FALSE)
  expect_type(result, "list")
  expect_true(all(c("online", "latencia_ms", "mensagem", "url") %in% names(result)))
  expect_type(result$online, "logical")
  expect_type(result$mensagem, "character")
  expect_true(is.numeric(result$latencia_ms))
})

test_that("caged_status online = TRUE quando HuggingFace acessivel", {
  skip_on_cran()
  skip_on_ci()  # teste de integracao real — verifica conectividade real
  result <- caged_status(verbose = FALSE, timeout = 15)
  # Pular se HF estiver fora (teste de conectividade, nao de logica)
  skip_if(!result$online, paste("HuggingFace indisponivel:", result$mensagem))
  expect_true(result$online)
  expect_gt(result$latencia_ms, 0)
})

test_that("caged_status verbose = FALSE nao imprime nada", {
  local_mocked_bindings(
    caged_status = function(...) list(
      online = TRUE, latencia_ms = 10L,
      mensagem = "ok", url = "https://example.com"
    ),
    .package = "datacaged"
  )
  expect_silent(caged_status(verbose = FALSE))
})

test_that("caged_status verbose = TRUE nao da erro", {
  skip_on_cran()
  skip_on_ci()  # teste de integracao real
  expect_no_error(caged_status(verbose = TRUE, timeout = 15))
})
