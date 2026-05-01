# test-disponibilidade.R

test_that(".ftp_list retorna NULL para URL inexistente", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "FTP inacessível em ambiente CI")
  result <- datacaged:::.ftp_list(
    "ftp://ftp.mtps.gov.br/caminho/inexistente/xyz123",
    timeout = 3
  )
  expect_null(result)
})

test_that("caged_ftp_files retorna tibble com colunas corretas", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "FTP inacessível em ambiente CI")
  result <- tryCatch(
    caged_ftp_files(n = 3, verbose = FALSE, timeout = 15),
    error = function(e) NULL
  )
  skip_if(is.null(result), "FTP inacessível — pulando teste")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(nrow(result) <= 3)
  expect_true(all(result$mes >= 1 & result$mes <= 12))
  expect_true(all(result$ano >= 2020))
  # Ordenado do mais recente para o mais antigo
  expect_true(all(diff(as.integer(result$competencia)) <= 0))
})

test_that("caged_ftp_files aceita type = 'antigo'", {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "FTP inacessível em ambiente CI")
  result <- tryCatch(
    caged_ftp_files(type = "antigo", n = 3, verbose = FALSE, timeout = 15),
    error = function(e) NULL
  )
  skip_if(is.null(result), "FTP inacessível — pulando teste")
  expect_s3_class(result, "data.frame")
  expect_true(all(result$ano <= 2019))
})

