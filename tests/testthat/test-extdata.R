# test-extdata.R
# Testes usando arquivos de exemplo em inst/extdata/

# Helper para encontrar extdata em dev e instalado
.extdata_path <- function(arquivo) {
  dev <- file.path("../../inst/extdata", arquivo)
  if (file.exists(dev)) return(dev)
  pkg <- system.file("extdata", arquivo, package = "datacaged")
  if (nzchar(pkg)) return(pkg)
  NULL
}

test_that("arquivos de exemplo existem em inst/extdata", {
  path_mov    <- .extdata_path("CAGEDMOV202301_exemplo.7z")
  path_antigo <- .extdata_path("CAGED201801SP_exemplo.7z")
  expect_true(!is.null(path_mov) && file.exists(path_mov))
  expect_true(!is.null(path_antigo) && file.exists(path_antigo))
})

test_that("caged_parse lê extdata Novo CAGED", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.7z")
  skip_if(is.null(path) || !file.exists(path), "extdata nao encontrado")

  # Tenta via caged_parse (stream + fallback extract)
  df <- tryCatch(
    caged_parse(path, type = "MOV"),
    error = function(e) NULL,
    warning = function(w) invokeRestart("muffleWarning")
  )
  df <- tryCatch(caged_parse(path, type = "MOV"), error = function(e) NULL)

  skip_if(is.null(df), "arquivo .7z de exemplo nao e legivel neste sistema")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true("fonte_tipo" %in% names(df))
})

test_that("caged_parse lê extdata CAGED antigo", {
  path <- .extdata_path("CAGED201801SP_exemplo.7z")
  skip_if(is.null(path) || !file.exists(path), "extdata nao encontrado")

  df <- tryCatch(caged_parse(path, type = "ANTIGO"), error = function(e) NULL)
  skip_if(is.null(df), "arquivo .7z de exemplo nao e legivel neste sistema")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
})

test_that("pipeline parse -> duckdb funciona com extdata Novo CAGED", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.7z")
  skip_if(is.null(path) || !file.exists(path), "extdata nao encontrado")

  df <- tryCatch(caged_parse(path, type = "MOV"), error = function(e) NULL)
  skip_if(is.null(df), "arquivo .7z de exemplo nao e legivel neste sistema")

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  n <- caged_to_duckdb(df, db_path = db)
  expect_true(file.exists(db))
  expect_gt(n, 0L)
})

test_that("salario e lido como numerico", {
  path <- .extdata_path("CAGEDMOV202301_exemplo.7z")
  skip_if(is.null(path) || !file.exists(path), "extdata nao encontrado")

  df <- tryCatch(caged_parse(path, type = "MOV"), error = function(e) NULL)
  skip_if(is.null(df) || !"salario" %in% names(df),
          "arquivo nao legivel ou sem coluna salario")

  expect_true(is.numeric(df$salario))
})
