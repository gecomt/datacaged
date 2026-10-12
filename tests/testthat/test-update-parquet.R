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
  on.exit(unlink(db), add = TRUE)

  caged_to_duckdb(make_df_mov_test(c(202301L, 202302L, 202312L)), db_path = db)

  result <- datacaged:::.max_competencias(db)
  expect_type(result, "list")
  expect_true("caged_mov" %in% names(result))
  expect_equal(result[["caged_mov"]], 202312L)
})

test_that("caged_update retorna NULL quando banco ja esta atualizado", {
  # Mock caged_hf_files para nao consultar a rede — teste rapido e deterministico
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
  on.exit(unlink(db), add = TRUE)

  # 999912 > 202412 → caged_update deve retornar NULL sem baixar nada
  caged_to_duckdb(make_df_mov_test(999912L), db_path = db)

  result <- suppressMessages(
    caged_update(db_path = db, verbose = FALSE)
  )
  expect_null(result)
})

test_that("caged_update com banco vazio pede todas as competencias do HF", {
  ## Listagem real do HuggingFace (rapida); a carga e mockada para nao
  ## baixar o historico inteiro do Novo CAGED
  skip_on_cran()
  skip_if_offline("huggingface.co")

  chamadas <- list()
  local_mocked_bindings(
    caged_load = function(years, months, timeout, ...) {
      chamadas[[length(chamadas) + 1L]] <<- list(years = years, months = months,
                                                 timeout = timeout)
      invisible(NULL)
    },
    caged_info = function(...) invisible(tibble::tibble()),
    .package = "datacaged"
  )

  disponiveis <- caged_hf_files(n = Inf, verbose = FALSE, timeout = 30)
  suppressMessages(
    caged_update(db_path = tempfile(fileext = ".duckdb"), verbose = FALSE, timeout = 30)
  )

  ## Uma chamada por ano e, somadas, exatamente as competencias disponiveis
  anos <- vapply(chamadas, `[[`, integer(1), "years")
  expect_equal(anos, sort(unique(disponiveis$ano)))
  pedidas <- unlist(lapply(chamadas, function(x) x$years * 100L + x$months))
  expect_setequal(pedidas, as.integer(disponiveis$competencia))
  expect_true(all(vapply(chamadas, `[[`, numeric(1), "timeout") == 30))
})

# ── caged_to_parquet ──────────────────────────────────────────────────────────

test_that("caged_to_parquet exporta tabela e retorna tibble correto", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_test_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) }, add = TRUE)

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
  on.exit({ unlink(db); unlink(out, recursive = TRUE) }, add = TRUE)

  caged_to_duckdb(make_df_mov_test(202301L), db_path = db)
  expect_no_error(caged_to_parquet(db, output_dir = out))
  expect_true(dir.exists(out))
})

test_that("caged_to_parquet erro para tabela inexistente", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempdir()
  on.exit(unlink(db), add = TRUE)

  caged_to_duckdb(make_df_mov_test(202301L), db_path = db)
  expect_error(
    caged_to_parquet(db, output_dir = out, tables = "tabela_que_nao_existe")
  )
})

test_that("caged_to_parquet respeita overwrite=FALSE", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_ow_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) }, add = TRUE)

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

# -- caged_update: competencias faltantes ------------------------------------

test_that(".competencias_faltantes devolve tudo com o banco vazio", {
  fn <- datacaged:::.competencias_faltantes
  expect_equal(
    fn(c(202301L, 202302L), list(), c("caged_mov", "caged_for", "caged_exc")),
    c(202301L, 202302L)
  )
})

test_that(".competencias_faltantes detecta novas, lacunas e tabelas incompletas", {
  fn <- datacaged:::.competencias_faltantes
  disp <- c(202212L, 202301L, 202302L, 202303L, 202304L)
  no_banco <- list(
    caged_mov = c(202301L, 202303L),            # lacuna em 202302
    caged_for = c(202301L, 202302L, 202303L),
    caged_exc = c(202301L, 202302L)             # falta 202303
  )
  res <- fn(disp, no_banco, c("caged_mov", "caged_for", "caged_exc"))
  ## 202212 e anterior ao inicio do banco: nao e baixada
  expect_equal(res, c(202302L, 202303L, 202304L))
})

test_that(".competencias_faltantes usa a tabela principal para o inicio", {
  fn <- datacaged:::.competencias_faltantes
  ## caged_for com chave antiga (competenciamov) contendo meses anteriores
  no_banco <- list(caged_mov = 202301L, caged_for = c(202201L, 202301L),
                   caged_exc = 202301L)
  res <- fn(c(202201L, 202301L), no_banco, c("caged_mov", "caged_for", "caged_exc"))
  expect_equal(res, integer())
})

test_that("caged_update chama caged_load por ano, sem produto cartesiano", {
  chamadas <- list()
  local_mocked_bindings(
    caged_hf_files = function(...) tibble::tibble(
      competencia = c("202511", "202512", "202601", "202602"),
      ano = c(2025L, 2025L, 2026L, 2026L), mes = c(11L, 12L, 1L, 2L),
      url = "https://example.com"
    ),
    caged_load = function(years, months, ...) {
      chamadas[[length(chamadas) + 1L]] <<- list(years = years, months = months)
      invisible(NULL)
    },
    caged_info = function(...) invisible(tibble::tibble()),
    .package = "datacaged"
  )

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)
  df <- make_df_mov_test(202511L)
  suppressMessages(caged_to_duckdb(df, db_path = db))
  suppressMessages(caged_to_duckdb(transform(df, fonte_tipo = "FOR"), db_path = db))
  suppressMessages(caged_to_duckdb(transform(df, fonte_tipo = "EXC"), db_path = db))

  suppressMessages(caged_update(db_path = db, verbose = FALSE))

  expect_length(chamadas, 2L)
  expect_equal(chamadas[[1]], list(years = 2025L, months = 12L))
  expect_equal(chamadas[[2]], list(years = 2026L, months = c(1L, 2L)))
})

# -- caged_to_duckdb: transacao e erros ---------------------------------------

test_that("overwrite_competencies desfaz o DELETE se a insercao falhar", {
  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)
  suppressMessages(caged_to_duckdb(make_df_mov_test(202301L, n = 7L), db_path = db))

  local_mocked_bindings(
    .insert_rows = function(...) stop("falha simulada"),
    .package = "datacaged"
  )
  expect_error(
    suppressMessages(caged_to_duckdb(make_df_mov_test(202301L), db_path = db,
                                     overwrite_competencies = TRUE)),
    "falha simulada"
  )

  con <- caged_connect(db, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM caged_mov")$n, 7)
})

test_that(".existing_competencies propaga erros em vez de devolver vazio", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "CREATE TABLE caged_mov (competencia VARCHAR)")
  DBI::dbExecute(con, "INSERT INTO caged_mov VALUES ('202301')")
  local_mocked_bindings(
    dbGetQuery = function(...) stop("consulta falhou"),
    .package = "DBI"
  )
  expect_error(
    datacaged:::.existing_competencies(con, "caged_mov", 202301),
    "consulta falhou"
  )
})

# -- caged_to_parquet: particionamento ----------------------------------------

test_that("caged_to_parquet particiona por varias colunas e respeita overwrite", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_part_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) }, add = TRUE)
  suppressMessages(caged_to_duckdb(make_df_mov_test(c(202301L, 202302L)), db_path = db))

  r1 <- suppressMessages(caged_to_parquet(db, out, partition_by = c("competencia", "uf")))
  expect_equal(r1$status, "exportado")
  expect_true(length(list.files(file.path(out, "caged_mov"), recursive = TRUE)) >= 2L)

  r2 <- suppressMessages(caged_to_parquet(db, out, partition_by = c("competencia", "uf")))
  expect_equal(r2$status, "pulado")

  r3 <- suppressMessages(caged_to_parquet(db, out, partition_by = c("competencia", "uf"),
                                          overwrite = TRUE))
  expect_equal(r3$status, "exportado")
})

test_that("caged_to_parquet avisa sobre coluna de particao inexistente", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_bad_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) }, add = TRUE)
  suppressMessages(caged_to_duckdb(make_df_mov_test(202301L), db_path = db))

  expect_warning(
    r <- suppressMessages(caged_to_parquet(db, out, partition_by = "nao_existe")),
    "not found"
  )
  expect_equal(r$status, "erro")
  expect_error(caged_to_parquet(db, out, partition_by = 1L), "character vector")
})

test_that("caged_to_parquet grava codigos sem casas decimais nos diretorios", {
  db  <- tempfile(fileext = ".duckdb")
  out <- tempfile("parquet_int_")
  on.exit({ unlink(db); unlink(out, recursive = TRUE) }, add = TRUE)
  suppressMessages(caged_to_duckdb(make_df_mov_test(202301L), db_path = db))

  suppressMessages(caged_to_parquet(db, out, partition_by = c("competencia", "uf")))
  arqs <- list.files(file.path(out, "caged_mov"), recursive = TRUE)
  expect_true(all(grepl("^competencia=202301/uf=35/", arqs)))

  ## O banco continua com a coluna original (DOUBLE)
  con <- caged_connect(db, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  tipo <- DBI::dbGetQuery(con, "SELECT DISTINCT typeof(competencia) AS t FROM caged_mov")$t
  expect_equal(tipo, "DOUBLE")

  ## Colunas fora da lista de codigos sao exportadas como estao
  out2 <- tempfile("parquet_sal_")
  on.exit(unlink(out2, recursive = TRUE), add = TRUE)
  suppressMessages(caged_to_parquet(db, out2, partition_by = "salario"))
  dirs <- list.dirs(file.path(out2, "caged_mov"), recursive = FALSE, full.names = FALSE)
  expect_true(any(grepl("\\.", sub("^salario=", "", dirs))))
})
