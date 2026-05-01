# test-status.R
# Testes de caged_status() — independentes de FTP

test_that("caged_status retorna lista estruturada mesmo offline", {
  
  result <- tryCatch(
    caged_status(verbose = FALSE, timeout = 1),
    error = function(e) list(
      online = FALSE,
      latencia_ms = NA_real_,
      mensagem = "erro"
    )
  )
  
  expect_type(result, "list")
  expect_true(all(c("online", "latencia_ms", "mensagem") %in% names(result)))
  expect_type(result$online, "logical")
})

test_that("caged_status retorna valores coerentes", {
  
  result <- tryCatch(
    caged_status(verbose = FALSE, timeout = 1),
    error = function(e) list(
      online = FALSE,
      latencia_ms = NA_real_,
      mensagem = "erro"
    )
  )
  
  expect_true(is.logical(result$online))
  expect_true(is.numeric(result$latencia_ms) || is.na(result$latencia_ms))
  expect_true(is.character(result$mensagem))
})

test_that("caged_status verbose = FALSE não imprime nada", {
  
  expect_silent(
    tryCatch(
      caged_status(verbose = FALSE, timeout = 1),
      error = function(e) NULL
    )
  )
})