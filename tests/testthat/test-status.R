# test-status.R

test_that("caged_status retorna lista com campos corretos", {
  result <- caged_status(verbose = FALSE, timeout = 15)
  expect_type(result, "list")
  expect_true(all(c("online", "latencia_ms", "mensagem", "url") %in% names(result)))
  expect_type(result$online, "logical")
  expect_type(result$mensagem, "character")
  expect_true(is.numeric(result$latencia_ms))
})

test_that("caged_status online = TRUE quando HuggingFace acessivel", {
  skip_on_cran()
  result <- caged_status(verbose = FALSE, timeout = 15)
  expect_true(result$online, info = paste("Mensagem:", result$mensagem))
  expect_gt(result$latencia_ms, 0)
})

test_that("caged_status verbose = FALSE nao imprime nada", {
  expect_silent(caged_status(verbose = FALSE, timeout = 15))
})

test_that("caged_status verbose = TRUE imprime mensagem", {
  skip_on_cran()
  expect_output(
    caged_status(verbose = TRUE, timeout = 15),
    regexp = NULL  # qualquer output e valido
  )
})
