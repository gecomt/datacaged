# test-pipelines.R
# Testes de smoke para as funções de pipeline de alto nível.
# Requerem acesso ao HuggingFace; pulados apenas no CRAN.

# ── caged_download() ──────────────────────────────────────────────────────────

test_that("caged_download() retorna manifest com colunas esperadas", {
  skip_on_cran()

  destdir <- tempfile("caged_dl_")
  on.exit(unlink(destdir, recursive = TRUE), add = TRUE)

  ano <- as.integer(format(Sys.Date(), "%Y")) - 1L

  result <- caged_download(
    years   = ano,
    months  = 1L,
    destdir = destdir,
    timeout = 120L
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("arquivo", "competencia", "type", "status", "ano", "mes") %in% names(result)))
  expect_gt(nrow(result), 0L)
  expect_true(any(result$status %in% c("baixado", "cache")),
              info = paste("Status obtidos:", paste(unique(result$status), collapse = ", ")))
})

# ── caged_load() ─────────────────────────────────────────────────────────────

test_that("caged_load() cria banco DuckDB com tabela caged_mov", {
  skip_on_cran()

  db_path <- tempfile("caged_load_", fileext = ".duckdb")
  destdir  <- tempfile("caged_load_cache_")
  on.exit({
    unlink(db_path)
    unlink(destdir, recursive = TRUE)
  }, add = TRUE)

  ano <- as.integer(format(Sys.Date(), "%Y")) - 1L

  caged_load(
    years   = ano,
    months  = 1L,
    db_path = db_path,
    destdir = destdir,
    timeout = 120L
  )

  expect_true(file.exists(db_path))
  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("caged_mov" %in% DBI::dbListTables(con))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_gt(n, 0L)
})

# ── caged_download_layouts() ──────────────────────────────────────────────────

test_that("caged_download_layouts() retorna tibble com colunas esperadas", {
  skip_on_cran()

  destdir <- tempfile("caged_lay_")
  on.exit(unlink(destdir, recursive = TRUE), add = TRUE)

  result <- caged_download_layouts(type = "novo", destdir = destdir, timeout = 60L)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("base", "arquivo", "url", "destino", "status") %in% names(result)))
  expect_gt(nrow(result), 0L)
})

# ── caged_adjustments_load() ─────────────────────────────────────────────────

test_that("caged_adjustments_load() cria banco com tabela caged_ajustes", {
  skip_on_cran()

  db_path <- tempfile("caged_adj_", fileext = ".duckdb")
  destdir  <- tempfile("caged_adj_cache_")
  on.exit({
    unlink(db_path)
    unlink(destdir, recursive = TRUE)
  }, add = TRUE)

  caged_adjustments_load(
    years   = 2019L,
    months  = 1L,
    db_path = db_path,
    destdir = destdir,
    timeout = 120L
  )

  expect_true(file.exists(db_path))
  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("caged_ajustes" %in% DBI::dbListTables(con))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_ajustes")$n
  expect_gt(n, 0L)
})
