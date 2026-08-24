# test-database.R
# Testes de gravação, leitura e utilitários do DuckDB

# ── Fixture: df simulado ──────────────────────────────────────────────────────

make_df_novo <- function(competencias = c(202301L, 202302L), n_por_comp = 10L) {
  set.seed(42L)
  dplyr::bind_rows(purrr::map(competencias, function(comp) {
    tibble::tibble(
      competencia       = comp,
      uf                = 35L,
      municipio         = 3550308L,
      saldomovimentacao = sample(c(-1L, 1L), n_por_comp, replace = TRUE),
      salario           = round(runif(n_por_comp, 1500, 5000), 2),
      sexo              = sample(1:2, n_por_comp, replace = TRUE),
      idade             = sample(18:65, n_por_comp, replace = TRUE),
      escolaridade      = sample(1:9, n_por_comp, replace = TRUE),
      fonte_tipo        = "MOV"
    )
  }))
}

make_df_antigo <- function(competencias = c(201801L, 201802L), n_por_comp = 8L) {
  set.seed(43L)
  dplyr::bind_rows(purrr::map(competencias, function(comp) {
    tibble::tibble(
      competencia       = comp,
      uf                = 35L,
      municipio         = 3550308L,
      saldomovimentacao = sample(c(-1L, 1L), n_por_comp, replace = TRUE),
      salario           = round(runif(n_por_comp, 1000, 3000), 2),
      sexo              = sample(1:2, n_por_comp, replace = TRUE),
      idade             = sample(18:65, n_por_comp, replace = TRUE),
      fonte_tipo        = "ANTIGO"
    )
  }))
}


make_df_ajustes <- function(competencias = c(201801L, 201802L), n_por_comp = 8L) {
  set.seed(44L)
  dplyr::bind_rows(purrr::map(competencias, function(comp) {
    tibble::tibble(
      competencia       = comp,
      uf                = 35L,
      municipio         = 3550308L,
      saldomovimentacao = sample(c(-1L, 1L), n_por_comp, replace = TRUE),
      salario           = round(runif(n_por_comp, 1000, 3000), 2),
      sexo              = sample(1:2, n_por_comp, replace = TRUE),
      idade             = sample(18:65, n_por_comp, replace = TRUE),
      fonte_tipo        = "AJUSTES"
    )
  }))
}
# ── Testes de detecção de table ──────────────────────────────────────────────

test_that(".detect_table identifica caged_mov corretamente", {
  df <- make_df_novo()
  expect_equal(datacaged:::.detect_table(df), "caged_mov")
})

test_that(".detect_table identifica caged_antigo corretamente", {
  df <- make_df_antigo()
  expect_equal(datacaged:::.detect_table(df), "caged_antigo")
})


test_that(".detect_table identifica caged_ajustes corretamente", {
  df <- make_df_ajustes()
  expect_equal(datacaged:::.detect_table(df), "caged_ajustes")
})

test_that(".detect_table erro quando df sem fonte_tipo", {
  df <- tibble::tibble(competencia = 202301L, uf = 35L)
  expect_error(datacaged:::.detect_table(df), "fonte_tipo")
})

test_that(".detect_table erro quando tipos misturados", {
  df <- dplyr::bind_rows(make_df_novo(), make_df_antigo())
  expect_error(datacaged:::.detect_table(df), "mistura")
})

# ── Testes de conexão ─────────────────────────────────────────────────────────

test_that("caged_connect cria banco e retorna conexão DBI válida", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  con <- caged_connect(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  expect_s4_class(con, "duckdb_connection")
  expect_true(file.exists(db))
})

test_that("caged_connect em memória funciona", {
  con <- caged_connect(":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_s4_class(con, "duckdb_connection")
})

# ── Testes de gravação ────────────────────────────────────────────────────────

test_that("caged_to_duckdb grava dados e retorna contagem correta", {
  db  <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  df  <- make_df_novo(competencias = 202301L, n_por_comp = 20L)
  n   <- caged_to_duckdb(df, db_path = db)

  expect_equal(n, 20L)

  con <- caged_connect(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  n_banco <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_equal(n_banco, 20L)
})

test_that("caged_to_duckdb pula competência já existente", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  df <- make_df_novo(competencias = 202301L, n_por_comp = 10L)

  caged_to_duckdb(df, db_path = db)
  n2 <- caged_to_duckdb(df, db_path = db) # segunda tentativa

  expect_equal(n2, 0L)

  con <- caged_connect(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  n_banco <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_equal(n_banco, 10L) # não duplicou
})

test_that("caged_to_duckdb com overwrite_competencies = TRUE regrava", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  df <- make_df_novo(competencias = 202301L, n_por_comp = 10L)
  caged_to_duckdb(df, db_path = db)

  df2 <- make_df_novo(competencias = 202301L, n_por_comp = 5L)
  caged_to_duckdb(df2, db_path = db, overwrite_competencies = TRUE)

  con <- caged_connect(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  n_banco <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_equal(n_banco, 5L)
})

test_that("caged_to_duckdb grava antigo e novo em tabelas separadas", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  caged_to_duckdb(make_df_novo(),   db_path = db)
  caged_to_duckdb(make_df_antigo(), db_path = db)

  con <- caged_connect(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tabelas <- DBI::dbListTables(con)
  expect_true("caged_mov"   %in% tabelas)
  expect_true("caged_antigo" %in% tabelas)
})


test_that("caged_to_duckdb grava ajustes em table caged_ajustes", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  df <- make_df_ajustes(competencias = 201901L, n_por_comp = 10L)
  n  <- caged_to_duckdb(df, db_path = db)

  expect_equal(n, 10L)

  con <- caged_connect(db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  tabelas <- DBI::dbListTables(con)
  expect_true("caged_ajustes" %in% tabelas)
  n_banco <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_ajustes")$n
  expect_equal(n_banco, 10L)
})

test_that("caged_to_duckdb retorna 0 para df vazio", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))
  n <- caged_to_duckdb(NULL, db_path = db)
  expect_equal(n, 0L)
})

# ── Testes de caged_info ──────────────────────────────────────────────────────

test_that("caged_info retorna tibble com estatísticas corretas", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  caged_to_duckdb(make_df_novo(competencias = c(202301L, 202302L), n_por_comp = 10L), db)

  info <- caged_info(db)
  expect_s3_class(info, "data.frame")
  expect_true("caged_mov" %in% info$table)
  expect_equal(info$registros[info$table == "caged_mov"], 20L)
})

test_that("caged_info erro para banco inexistente", {
  expect_error(caged_info("/caminho/inexistente.duckdb"))
})


# ── Testes de .con= (reúso de conexão) ───────────────────────────────────────

test_that("caged_to_duckdb aceita .con= e não fecha a conexão", {
  db  <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  con <- caged_connect(db, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- make_df_novo(competencias = 202301L, n_por_comp = 5L)
  n  <- caged_to_duckdb(df, .con = con)

  expect_equal(n, 5L)
  # Conexão ainda deve estar aberta
  expect_true(DBI::dbIsValid(con))

  n_banco <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_equal(n_banco, 5L)
})

test_that("caged_to_duckdb com .con= e db_path=NULL não dá erro", {
  db  <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  con <- caged_connect(db, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- make_df_novo(competencias = 202302L, n_por_comp = 3L)
  expect_no_error(caged_to_duckdb(df, .con = con))
})

test_that("caged_to_duckdb sem db_path e sem .con dá erro claro", {
  df <- make_df_novo(competencias = 202301L, n_por_comp = 3L)
  expect_error(caged_to_duckdb(df), "db_path|.con")
})

test_that("caged_to_duckdb múltiplas gravações com mesma .con acumulam corretamente", {
  db  <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db))

  con <- caged_connect(db, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  caged_to_duckdb(make_df_novo(202301L, 10L), .con = con)
  caged_to_duckdb(make_df_novo(202302L, 10L), .con = con)
  caged_to_duckdb(make_df_novo(202303L, 10L), .con = con)

  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n
  expect_equal(n, 30L)
})

# ── Testes de workers= (downloads paralelos) ──────────────────────────────────

test_that(".run_downloads workers=1 funciona em modo sequencial", {
  tasks <- datacaged:::.build_tasks(2023L, 1L)
  cache <- tempfile("cache_test_")
  on.exit(unlink(cache, recursive = TRUE))
  dir.create(cache)

  # Só verificamos que a função retorna o tibble com a estrutura correta
  # sem depender de rede — usando tarefas de cache já existente com .is_cached
  result <- datacaged:::.run_downloads(tasks, cache, force = FALSE, timeout = 5, workers = 1L)

  expect_s3_class(result, "data.frame")
  expect_true(all(c("arquivo", "competencia", "type", "status") %in% names(result)))
  expect_equal(nrow(result), 3L)  # MOV, FOR, EXC
  # Status pode ser "nao_encontrado" (404) ou "erro" (timeout) dependendo da rede
  expect_true(all(result$status %in% c("nao_encontrado", "erro", "cache")))
})

test_that(".run_downloads workers=3 retorna mesma estrutura que workers=1", {
  skip_on_cran()

  tasks <- datacaged:::.build_tasks(2023L, 1L)
  cache <- tempfile("cache_test_")
  on.exit(unlink(cache, recursive = TRUE))
  dir.create(cache)

  r1 <- datacaged:::.run_downloads(tasks, cache, force = FALSE, timeout = 5, workers = 1L)
  r3 <- datacaged:::.run_downloads(tasks, cache, force = FALSE, timeout = 5, workers = 3L)

  expect_equal(names(r1), names(r3))
  expect_equal(nrow(r1), nrow(r3))
  expect_equal(r1$competencia, r3$competencia)
  expect_equal(r1$type,        r3$type)
})

test_that("caged_download workers capado em min(workers, n_tasks)", {
  # Testa que workers > n não causa erro
  skip_on_cran()
  expect_no_error(
    caged_download(
      years   = as.integer(format(Sys.Date(), "%Y")) + 1L,  # futuro = 0 tarefas
      months  = 1L,
      workers = 99L
    )
  )
})
