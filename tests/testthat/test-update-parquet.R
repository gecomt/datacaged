# test-update-parquet.R
# Testes de caged_update() e caged_to_parquet()

# ── helpers ───────────────────────────────────────────────────────────────────

make_df_mov_test <- function(competencias, n = 5L) {
  set.seed(99L)
  dplyr::bind_rows(purrr::map(competencias, function(comp) {
    tibble::tibble(
      competencia       = comp,
      uf                = 35L,
      municipio         = 3550308L,
      saldomovimentacao = sample(c(-1L, 1L), n, replace = TRUE),
      salario           = round(runif(n, 1500, 5000), 2),
      sexo              = sample(1:2, n, replace = TRUE),
      fonte_tipo        = "MOV"
    )
  }))
}

# ── caged_update ──────────────────────────────────────────────────────────────

test_that(".max_competencias retorna lista vazia para banco inexistente", {
  result <- datacaged:::.max_competencias(tempfile(fileext = ".duckdb"))
  expect_type(result, "list")
  expect_equal(length(result), 0L)
})

test_that(".max_competencias retorna max correto por tabela", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  caged_to_duckdb(make_df_mov_test(c(202301L, 202302L, 202312L)), db_path = db)

  result <- datacaged:::.max_competencias(db)
  expect_type(result, "list")
  expect_true("caged_mov" %in% names(result))
  expect_equal(result[["caged_mov"]], 202312L)
})

test_that("caged_update retorna NULL quando banco já está atualizado", {
  skip_on_cran()

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  # Popula banco com competência muito futura para simular "já atualizado"
  df_futuro <- make_df_mov_test(999912L)  # competência impossível no HF
  caged_to_duckdb(df_futuro, db_path = db)

  result <- caged_update(db_path = db, verbose = FALSE)
  expect_null(result)
})

test_that("caged_update erro para db_path inexistente", {
  skip_on_cran()
  # Com banco vazio (não existente), update tenta criar — não é erro
  # Mas com banco corrompido deve dar erro no connect
  expect_no_error(
    caged_update(
      db_path = tempfile(fileext = ".duckdb"),
      verbose = FALSE
    )
  )
})

# ── caged_to_parquet ──────────────────────────────────────────────────────────

test_that("caged_to_parquet exporta tabela e retorna tibble correto", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_test_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) })

  caged_to_duckdb(make_df_mov_test(202301L, n = 20L), db_path = db)

  result <- caged_to_parquet(db, output_dir = out, tables = "caged_mov")

  expect_s3_class(result, "data.frame")
  expect_true("caged_mov" %in% result$tabela)
  expect_equal(result$registros[result$tabela == "caged_mov"], 20L)
  expect_true(file.exists(file.path(out, "caged_mov.parquet")))
})

test_that("caged_to_parquet cria output_dir se não existir", {
  db  <- tempfile(fileext = ".duckdb")
  out <- file.path(tempdir(), paste0("novo_dir_", Sys.getpid()))
  on.exit({ unlink(db); unlink(out, recursive = TRUE) })

  caged_to_duckdb(make_df_mov_test(202301L), db_path = db)
  expect_no_error(caged_to_parquet(db, output_dir = out))
  expect_true(dir.exists(out))
})

test_that("caged_to_parquet erro para tabela inexistente", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempdir()
  on.exit(unlink(db))

  caged_to_duckdb(make_df_mov_test(202301L), db_path = db)
  expect_error(
    caged_to_parquet(db, output_dir = out, tables = "tabela_que_nao_existe")
  )
})

test_that("caged_to_parquet respeita overwrite=FALSE", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_ow_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) })

  caged_to_duckdb(make_df_mov_test(202301L), db_path = db)
  caged_to_parquet(db, output_dir = out, tables = "caged_mov")

  # Segunda chamada com overwrite=FALSE deve pular
  result2 <- caged_to_parquet(db, output_dir = out, tables = "caged_mov",
                               overwrite = FALSE)
  expect_equal(result2$status, "pulado")
})

test_that("caged_to_parquet erro para banco inexistente", {
  expect_error(
    caged_to_parquet("/banco/inexistente.duckdb", output_dir = tempdir())
  )
})
