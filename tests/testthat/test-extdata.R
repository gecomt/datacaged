# test-extdata.R
# Testes de integração usando arquivos de exemplo em inst/extdata/
# Validam o pipeline completo parse → estrutura esperada
# sem depender do FTP ou de arquivos reais.

# Helper para encontrar extdata em dev e instalado
.extdata_path <- function(arquivo) {
  # Caminho ao rodar devtools::test()
  dev <- file.path("../../inst/extdata", arquivo)
  if (file.exists(dev)) return(dev)
  # Caminho ao rodar R CMD check
  pkg <- system.file("extdata", arquivo, package = "datacaged")
  if (nchar(pkg) > 0) return(pkg)
  NULL
}

test_that("arquivo de exemplo Novo CAGED MOV existe em inst/extdata", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.txt")
  expect_false(is.null(path))
  expect_true(file.exists(path))
})

test_that(".read_text_file lê extdata Novo CAGED com 20 linhas", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.txt")
  skip_if(is.null(path) || !file.exists(path), "extdata não encontrado")

  df <- datacaged:::.read_text_file(path, type = "MOV")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 20)
  # Layout real do Novo CAGED usa competenciamov
  expect_true("competenciamov" %in% names(df))
  expect_true("saldomovimentacao" %in% names(df))
  expect_true(is.numeric(df$saldomovimentacao))
  expect_true(is.numeric(df$salario))
  expect_equal(unique(df$fonte_tipo), "MOV")
})

test_that(".read_text_file lê extdata CAGED antigo com 20 linhas", {
  path <- .extdata_path("CAGED201801SP_exemplo.txt")
  skip_if(is.null(path) || !file.exists(path), "extdata não encontrado")

  df <- datacaged:::.read_text_file(path, type = "ANTIGO")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 20)
  expect_true("subsetor" %in% names(df))
  expect_true("saldomovimentacao" %in% names(df))
  expect_equal(unique(df$fonte_tipo), "ANTIGO")
})

test_that("pipeline parse → duckdb funciona com extdata Novo CAGED", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.txt")
  skip_if(is.null(path) || !file.exists(path), "extdata não encontrado")

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)

  df <- datacaged:::.read_text_file(path, type = "MOV")
  n  <- caged_to_duckdb(df, db_path = db)

  expect_equal(n, 20L)
  info <- caged_info(db)
  expect_equal(info$registros[info$table == "caged_mov"], 20L)
})

test_that("pipeline parse → duckdb funciona com extdata CAGED antigo", {
  path <- .extdata_path("CAGED201801SP_exemplo.txt")
  skip_if(is.null(path) || !file.exists(path), "extdata não encontrado")

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)

  df <- datacaged:::.read_text_file(path, type = "ANTIGO")
  n  <- caged_to_duckdb(df, db_path = db)

  expect_equal(n, 20L)
  info <- caged_info(db)
  expect_equal(info$registros[info$table == "caged_antigo"], 20L)
})

test_that("salario é lido como numérico (decimal vírgula)", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.txt")
  skip_if(is.null(path) || !file.exists(path), "extdata não encontrado")

  df <- datacaged:::.read_text_file(path, type = "MOV")
  expect_true(is.numeric(df$salario))
  expect_true(all(df$salario > 0, na.rm = TRUE))
})

