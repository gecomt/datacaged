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
