# test-availability.R

test_that(".hf_list_dir retorna NULL para path inexistente no HF", {
  skip_on_cran()
  result <- datacaged:::.hf_list_dir("caminho_inexistente_xyz123abc", timeout = 10)
  expect_null(result)
})

test_that("caged_hf_files retorna tibble com colunas corretas (novo)", {
  skip_on_cran()
  result <- caged_hf_files(n = 3, verbose = FALSE, timeout = 30)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_lte(nrow(result), 3)
  expect_true(all(result$mes >= 1 & result$mes <= 12))
  expect_true(all(result$ano >= 2020))
  expect_true(all(grepl("^https://", result$url)))
  expect_true(all(diff(as.integer(result$competencia)) <= 0))
})

test_that("caged_hf_files retorna tibble com colunas corretas (antigo)", {
  skip_on_cran()
  result <- caged_hf_files(type = "antigo", n = 3, verbose = FALSE, timeout = 30)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(all(result$ano <= 2019))
})

test_that("caged_hf_files retorna tibble com colunas corretas (ajustes)", {
  skip_on_cran()
  result <- caged_hf_files(type = "ajustes", n = 3, verbose = FALSE, timeout = 30)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(all(result$ano <= 2019))
  expect_true(all(grepl("ajustes", result$url, ignore.case = TRUE)))
})

test_that("caged_hf_files valida argumento n", {
  expect_error(caged_hf_files(n = 0),  "inteiro positivo")
  expect_error(caged_hf_files(n = -1), "inteiro positivo")
  expect_error(caged_hf_files(n = NA), "inteiro positivo")
})

test_that("caged_ftp_files emite warning de deprecacao", {
  skip_on_cran()
  expect_warning(
    caged_ftp_files(n = 1, verbose = FALSE),
    "renomeada"
  )
})
