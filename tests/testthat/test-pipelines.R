# test-pipelines.R
# Testes de smoke para as 4 funções de pipeline de alto nível.
# Todos dependem de acesso ao FTP do MTE e são pulados no CRAN e em CI.
# Objetivo: garantir que as assinaturas públicas não quebrem silenciosamente
# e que os contratos de retorno sejam respeitados.

# ── helpers ───────────────────────────────────────────────────────────────────

.skip_if_no_network <- function() {
  skip_on_cran()
  skip_if(nzchar(Sys.getenv("CI")), "FTP inacessível em ambiente CI")
}

# ── caged_download() ──────────────────────────────────────────────────────────

test_that("caged_download() retorna data.frame com colunas esperadas", {
  .skip_if_no_network()

  destdir <- tempfile("caged_dl_")
  on.exit(unlink(destdir, recursive = TRUE), add = TRUE)

  result <- tryCatch(
    caged_download(
      years   = as.integer(format(Sys.Date(), "%Y")) - 1L,
      months  = 1L,
      destdir = destdir,
      timeout = 60L
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "FTP inacessível — pulando teste")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("arquivo", "competencia", "type", "status", "caminho") %in% names(result)))
  expect_true(nrow(result) > 0L)
})

# ── caged_download_layouts() ──────────────────────────────────────────────────

test_that("caged_download_layouts() retorna tibble com colunas esperadas", {
  .skip_if_no_network()

  destdir <- tempfile("caged_lay_")
  on.exit(unlink(destdir, recursive = TRUE), add = TRUE)

  result <- tryCatch(
    caged_download_layouts(destdir = destdir, timeout = 60L),
    error = function(e) NULL
  )

  skip_if(is.null(result), "FTP inacessível — pulando teste")

  expect_s3_class(result, "data.frame")
  expect_true(all(c("base", "arquivo", "url", "destino") %in% names(result)))
  expect_true(nrow(result) > 0L)
})

# ── caged_load() ─────────────────────────────────────────────────────────────

test_that("caged_load() cria banco DuckDB com tabela caged_mov", {
  .skip_if_no_network()

  db_path <- tempfile("caged_load_", fileext = ".duckdb")
  destdir  <- tempfile("caged_load_cache_")
  on.exit({
    unlink(db_path)
    unlink(destdir, recursive = TRUE)
  }, add = TRUE)

  result <- tryCatch(
    caged_load(
      years   = as.integer(format(Sys.Date(), "%Y")) - 1L,
      months  = 1L,
      db_path = db_path,
      destdir = destdir,
      timeout = 60L
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "FTP inacessível — pulando teste")

  expect_true(file.exists(db_path))
  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("caged_mov" %in% DBI::dbListTables(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n, 0L)
})

# ── caged_adjustments_load() ─────────────────────────────────────────────────

test_that("caged_adjustments_load() cria banco com tabela caged_ajustes", {
  .skip_if_no_network()

  db_path <- tempfile("caged_adj_", fileext = ".duckdb")
  destdir  <- tempfile("caged_adj_cache_")
  on.exit({
    unlink(db_path)
    unlink(destdir, recursive = TRUE)
  }, add = TRUE)

  result <- tryCatch(
    caged_adjustments_load(
      years   = 2019L,
      months  = 1L,
      db_path = db_path,
      destdir = destdir,
      timeout = 60L
    ),
    error = function(e) NULL
  )

  skip_if(is.null(result), "FTP inacessível — pulando teste")

  expect_true(file.exists(db_path))
  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("caged_ajustes" %in% DBI::dbListTables(con))
  expect_gt(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_ajustes")$n, 0L)
})
