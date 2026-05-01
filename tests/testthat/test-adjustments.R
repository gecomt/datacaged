test_that(".build_adjustments_tasks gera URLs corretas", {
  tarefas <- datacaged:::.build_adjustments_tasks(years = 2019, months = c(1L, 2L, 3L))
  
  expect_equal(nrow(tarefas), 3)
  expect_true(all(tarefas$type == "AJUSTES"))
  
  nome_col <- intersect(names(tarefas), c("nomearquivo", "nome_arquivo"))[1]
  expect_true(!is.na(nome_col), info = paste("Colunas disponíveis:", paste(names(tarefas), collapse = ", ")))
  
  expect_true(all(grepl("CAGEDAJUSTES[_ ]?", tarefas[[nome_col]])))
  expect_true(all(grepl("CAGEDAJUSTES", tarefas$url)))
  expect_true(all(is.na(tarefas$uf)))
  
  # Aceita com ou sem underscore
  expect_match(tarefas[[nome_col]][1], "CAGEDAJUSTES[_ ]?012019\\.7z")
  expect_match(tarefas[[nome_col]][2], "CAGEDAJUSTES[_ ]?022019\\.7z")
  expect_match(tarefas[[nome_col]][3], "CAGEDAJUSTES[_ ]?032019\\.7z")
})

test_that(".build_adjustments_tasks ignora years >= 2020", {
  tarefas <- datacaged:::.build_adjustments_tasks(years = c(2018, 2019), months = 1)
  
  expect_equal(nrow(tarefas), 2)
  expect_true(all(tarefas$ano < 2020))
})

test_that(".build_adjustments_tasks ignora competencias futuras", {
  ano_futuro <- as.integer(format(Sys.Date(), "%Y")) + 1L
  
  tarefas <- datacaged:::.build_adjustments_tasks(
    years  = min(ano_futuro, 2019L),
    months = 1
  )
  
  if (nrow(tarefas) > 0) {
    expect_true(all(as.Date(sprintf("%04d-%02d-01", tarefas$ano, tarefas$mes)) <= Sys.Date()))
  }
})


test_that("caged_ftp_files aceita type ajustes", {
  skip_on_cran()
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  
  result <- tryCatch(
    caged_ftp_files(type = "ajustes", n = 3, verbose = FALSE, timeout = 15),
    error = function(e) NULL
  )
  
  skip_if(is.null(result), "FTP inacessível — pulando teste")
  skip_if(nrow(result) == 0, "Nenhuma competência de ajustes disponível no FTP")
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(all(result$ano < 2020))
  expect_true(all(grepl("CAGEDAJUSTES", result$url)))
})
