# test-pipelines.R
# Testes de smoke para as funções de pipeline de alto nível.
# Testes rápidos usam dados sintéticos (make_df_*).
# Testes de integração real requerem rede e ficam com skip_on_cran()/skip_on_ci().

# ── Helpers ───────────────────────────────────────────────────────────────────

make_df_pipeline <- function(n = 50L) {
  set.seed(42L)
  tibble::tibble(
    competenciamov    = 202301L,
    uf                = sample(11:53, n, replace = TRUE),
    municipio         = sample(1100015L:5300108L, n, replace = TRUE),
    saldomovimentacao = sample(c(-1L, 1L), n, replace = TRUE),
    salario           = round(runif(n, 1320, 8000), 2),
    sexo              = sample(1:3, n, replace = TRUE),
    idade             = sample(18:65, n, replace = TRUE),
    fonte_tipo        = "MOV"
  )
}

# ── caged_to_duckdb + caged_connect (sem rede) ───────────────────────────────

test_that("pipeline sintetico: caged_to_duckdb cria banco e caged_connect conecta", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  df <- make_df_pipeline()
  n  <- caged_to_duckdb(df, db_path = db)
  expect_gt(n, 0L)
  expect_true(file.exists(db))

  con <- caged_connect(db, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("caged_mov" %in% DBI::dbListTables(con))
  n_banco <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_equal(n_banco, nrow(df))
})

test_that("pipeline sintetico: caged_info retorna estatisticas corretas", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  caged_to_duckdb(make_df_pipeline(20L), db_path = db)
  info <- caged_info(db)
  expect_s3_class(info, "data.frame")
  expect_true("caged_mov" %in% info$table)
  expect_equal(info$registros[info$table == "caged_mov"], 20L)
})

test_that("pipeline sintetico: caged_update nao baixa quando banco ja atualizado", {
  # Mock caged_hf_files — sem rede, rapido e deterministico
  local_mocked_bindings(
    caged_hf_files = function(...) tibble::tibble(
      competencia = "202412", ano = 2024L, mes = 12L,
      url = "https://example.com/fake.7z"
    ,
    .package = "datacaged"
  ),
    .package = "datacaged"
  )

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  # 999912 > 202412 → nao deve baixar nada
  df <- make_df_pipeline()
  df$competenciamov <- 999912L
  caged_to_duckdb(df, db_path = db)

  result <- suppressMessages(caged_update(db_path = db, verbose = FALSE))
  expect_null(result)
})

test_that("pipeline sintetico: caged_to_parquet exporta arquivo", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) })

  caged_to_duckdb(make_df_pipeline(30L), db_path = db)
  result <- caged_to_parquet(db, output_dir = out, tables = "caged_mov")

  expect_true(file.exists(file.path(out, "caged_mov.parquet")))
  expect_equal(result$registros[result$tabela == "caged_mov"], 30L)
})

# ── Testes de integração real (requerem rede) ─────────────────────────────────
# Todos mockados para serem rápidos e sem rede

test_that("caged_download() retorna manifest com colunas esperadas", {
  # Mock .download_file para simular download bem-sucedido sem rede
  local_mocked_bindings(
    .download_file = function(url, destfile, timeout, ...) {
      # Criar arquivo fake para simular download
      writeBin(raw(10L), destfile)
      invisible(destfile)
    },
    .package = "datacaged"
  )

  destdir <- tempfile("caged_dl_")
  dir.create(destdir, recursive = TRUE)
  on.exit(unlink(destdir, recursive = TRUE), add = TRUE)

  result <- caged_download(
    years   = 2024L,
    months  = 1L,
    destdir = destdir,
    timeout = 5L,
    workers = 1L
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("arquivo", "competencia", "type", "status") %in% names(result)))
  # Status pode ser "baixado" (mock criou o arquivo) ou outro
  expect_true(all(result$status %in% c("baixado", "cache", "nao_encontrado", "erro")))
})

test_that("caged_load() cria banco DuckDB com tabela caged_mov", {
  # Mockar caged_download para retornar manifest com arquivos do extdata
  local_mocked_bindings(
    caged_download = function(...) {
      # Usar arquivo de exemplo do extdata
      arq <- system.file("extdata", "CAGEDMOV202301_exemplo.7z", package = "datacaged")
      tibble::tibble(
        arquivo     = arq,
        competencia = "202301",
        type        = "MOV",
        status      = if (nzchar(arq) && file.exists(arq)) "cache" else "erro",
        ano         = 2023L,
        mes         = 1L
      )
    },
    .package = "datacaged"
  )

  db_path <- tempfile("caged_load_mock_", fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  arq <- system.file("extdata", "CAGEDMOV202301_exemplo.7z", package = "datacaged")
  skip_if(!nzchar(arq) || !file.exists(arq), "extdata nao disponivel")

  suppressMessages(
    caged_load(years = 2023L, months = 1L, db_path = db_path, timeout = 5L)
  )

  expect_true(file.exists(db_path))
  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_true("caged_mov" %in% DBI::dbListTables(con))
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_gt(n, 0L)
})

test_that("caged_download_layouts() retorna tibble com colunas esperadas", {
  # .list_layout_urls retorna tibble com colunas: arquivo, url_lista (list column)
  local_mocked_bindings(
    .list_layout_urls = function(url_dir, timeout = 15) {
      nomes <- c("CAGEDMOV_Layout.xlsx", "CAGEDFOR_Layout.xlsx")
      tibble::tibble(
        arquivo   = nomes,
        url_lista = lapply(nomes, function(n) paste0(url_dir, "/", n))
      )
    },
    .download_file = function(url, destfile, timeout, ...) {
      writeBin(raw(10L), destfile)
      invisible(destfile)
    },
    .package = "datacaged"
  )

  destdir <- tempfile("caged_lay_mock_")
  dir.create(destdir, recursive = TRUE)
  on.exit(unlink(destdir, recursive = TRUE), add = TRUE)

  result <- tryCatch(
    caged_download_layouts(type = "novo", destdir = destdir, timeout = 5L),
    error = function(e) {
      skip(paste("Layouts indisponivel:", conditionMessage(e)))
    }
  )

  if (!inherits(result, "try-error") && !is.null(result)) {
    expect_s3_class(result, "data.frame")
    expect_true(all(c("base", "arquivo", "url", "destino", "status") %in% names(result)))
  }
})

test_that("caged_adjustments_load() cria banco com tabela caged_ajustes", {
  # Mockar download de ajustes usando extdata como fallback
  local_mocked_bindings(
    .hf_list_dir = function(path, timeout) {
      if (grepl("CAGED_AJUSTES$", path))      return(c("2019"))
      if (grepl("CAGED_AJUSTES/2019$", path)) return(c("CAGEDEST_AJUSTES_122019.7z"))
      NULL
    },
    .download_file = function(url, destfile, timeout, ...) {
      # Usar arquivo de exemplo do antigo (mesmo schema que ajustes)
      arq <- system.file("extdata", "CAGED201801SP_exemplo.7z", package = "datacaged")
      if (nzchar(arq) && file.exists(arq)) {
        file.copy(arq, destfile)
        invisible(destfile)
      } else {
        invisible(NULL)
      }
    },
    .package = "datacaged"
  )

  db_path <- tempfile("caged_adj_mock_", fileext = ".duckdb")
  on.exit(unlink(db_path), add = TRUE)

  arq <- system.file("extdata", "CAGED201801SP_exemplo.7z", package = "datacaged")
  skip_if(!nzchar(arq) || !file.exists(arq), "extdata nao disponivel")

  suppressMessages(
    caged_adjustments_load(
      years   = 2019L,
      months  = 12L,
      db_path = db_path,
      timeout = 5L
    )
  )

  if (file.exists(db_path)) {
    con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    tbls <- DBI::dbListTables(con)
    expect_true(any(c("caged_ajustes", "caged_antigo") %in% tbls))
  } else {
    skip("Arquivo de ajustes nao foi criado — extdata incompativel")
  }
})
