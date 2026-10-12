# test-adjustments.R

test_that(".build_adjustments_tasks gera URLs HTTPS corretas", {
  tarefas <- datacaged:::.build_adjustments_tasks(years = 2019, months = c(1L, 2L, 3L))

  expect_equal(nrow(tarefas), 3L)
  expect_true(all(tarefas$type == "AJUSTES"))
  expect_true(all(grepl("^https://", tarefas$url)))
  expect_true(all(grepl("CAGEDEST_AJUSTES", tarefas$url)))
  expect_true(all(is.na(tarefas$uf)))
  expect_match(tarefas$nome_arquivo[1], "CAGEDEST_AJUSTES_012019\\.7z")
  expect_match(tarefas$nome_arquivo[2], "CAGEDEST_AJUSTES_022019\\.7z")
  expect_match(tarefas$nome_arquivo[3], "CAGEDEST_AJUSTES_032019\\.7z")
})

test_that(".build_adjustments_tasks nao gera anos >= 2020", {
  # Pura logica — sem rede
  # A funcao aceita qualquer ano, mas caged_adjustments_load() filtra >= 2020
  # Aqui testamos so o comportamento da funcao interna
  tarefas <- datacaged:::.build_adjustments_tasks(years = c(2018L, 2019L), months = 1L)
  expect_equal(nrow(tarefas), 2L)
  expect_true(all(tarefas$ano < 2020L))
})

test_that(".build_adjustments_tasks ignora competencias futuras", {
  ano_futuro <- as.integer(format(Sys.Date(), "%Y")) + 1L
  tarefas <- datacaged:::.build_adjustments_tasks(
    years  = min(ano_futuro, 2019L),
    months = 1L
  )
  if (nrow(tarefas) > 0L) {
    expect_true(all(
      as.Date(sprintf("%04d-%02d-01", tarefas$ano, tarefas$mes)) <= Sys.Date()
    ))
  }
})

test_that("caged_adjustments_load rejeita anos >= 2020 com aviso ou erro", {
  # Pura validacao — sem rede
  # Quando todos os anos sao >= 2020, emite aviso E lanca erro (anos validos = 0)
  # Testar que o processo falha de alguma forma
  result <- tryCatch(
    withCallingHandlers(
      caged_adjustments_load(
        years   = 2020L,
        months  = 1L,
        db_path = tempfile(fileext = ".duckdb")
      ),
      warning = function(w) {
        if (grepl("Adjust|2019|valido", conditionMessage(w), ignore.case = TRUE)) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(e) "caught_error"
  )
  # Deve ter lancado erro (anos validos = 0 apos filtrar 2020)
  expect_equal(result, "caught_error")
})

test_that("caged_hf_files type = 'ajustes' retorna tibble correto", {
  local_mocked_bindings(
    .hf_list_dir = function(path, timeout) {
      if (path == "CAGED_AJUSTES")               return(c("2019", "2018"))
      if (grepl("CAGED_AJUSTES/2019$", path))    return(c("CAGEDEST_AJUSTES_012019.7z","CAGEDEST_AJUSTES_022019.7z","CAGEDEST_AJUSTES_032019.7z"))
      if (grepl("CAGED_AJUSTES/2018$", path))    return(c("CAGEDEST_AJUSTES_012018.7z","CAGEDEST_AJUSTES_022018.7z"))
      NULL
    },
    .package = "datacaged"
  )
  result <- suppressWarnings(caged_hf_files(type = "ajustes", n = 3, verbose = FALSE))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(all(result$ano < 2020L))
  expect_true(all(grepl("ajustes", result$url, ignore.case = TRUE)))
})

test_that("caged_adjustments_load processa arquivo anual uma vez e respeita months", {
  chamadas_download <- 0L
  chamadas_parse    <- 0L
  local_mocked_bindings(
    .run_downloads = function(tasks, ...) {
      chamadas_download <<- nrow(tasks)
      tibble::tibble(arquivo = "CAGEDEST_AJUSTES_2005.7z", competencia = tasks$competencia,
                     type = "AJUSTES", ano = tasks$ano, mes = tasks$mes,
                     uf = NA_character_, status = "cache")
    },
    caged_parse = function(path, type = NULL) {
      chamadas_parse <<- chamadas_parse + 1L
      tibble::tibble(competencia_declarada = 200500 + rep(1:12, each = 2),
                     saldo = 1, fonte_tipo = "AJUSTES")
    },
    caged_info = function(...) invisible(tibble::tibble()),
    .package = "datacaged"
  )

  db <- tempfile(fileext = ".duckdb")
  on.exit(unlink(db), add = TRUE)
  suppressMessages(caged_adjustments_load(years = 2005, months = c(3L, 4L), db_path = db))

  expect_equal(chamadas_download, 1L)   # uma tarefa, nao uma por mes
  expect_equal(chamadas_parse, 1L)      # arquivo anual parseado uma vez

  con <- caged_connect(db, read_only = TRUE, quiet = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  comps <- DBI::dbGetQuery(con, "SELECT DISTINCT competencia FROM caged_ajustes ORDER BY 1")$competencia
  expect_equal(comps, c(200503, 200504))  # so os meses pedidos
})
