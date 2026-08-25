# test-availability.R

# Dados fake do HF para testes sem rede
.hf_mock_novo <- function(path, timeout) {
  if (path == "NOVO_CAGED")                          return(c("2023", "2024"))
  if (grepl("NOVO_CAGED/2024$", path))               return(c("202401", "202402", "202403"))
  if (grepl("NOVO_CAGED/2023$", path))               return(c("202312", "202311"))
  if (grepl("NOVO_CAGED/202401$", path))             return(c("CAGEDMOV202401.7z","CAGEDFOR202401.7z","CAGEDEXC202401.7z"))
  if (grepl("NOVO_CAGED/202402$", path))             return(c("CAGEDMOV202402.7z","CAGEDFOR202402.7z","CAGEDEXC202402.7z"))
  if (grepl("NOVO_CAGED/202403$", path))             return(c("CAGEDMOV202403.7z","CAGEDFOR202403.7z","CAGEDEXC202403.7z"))
  if (grepl("NOVO_CAGED/202312$", path))             return(c("CAGEDMOV202312.7z","CAGEDFOR202312.7z","CAGEDEXC202312.7z"))
  NULL
}

.hf_mock_antigo <- function(path, timeout) {
  if (path == "CAGED")                               return(c("2018", "2019"))
  if (grepl("CAGED/2019$", path))                    return(c("CAGEDEST_012019.7z","CAGEDEST_022019.7z","CAGEDEST_032019.7z"))
  if (grepl("CAGED/2018$", path))                    return(c("CAGEDEST_012018.7z","CAGEDEST_022018.7z"))
  NULL
}

.hf_mock_ajustes <- function(path, timeout) {
  if (path == "CAGED_AJUSTES")                       return(c("2019", "2018"))
  if (grepl("CAGED_AJUSTES/2019$", path))            return(c("CAGEDEST_AJUSTES_012019.7z","CAGEDEST_AJUSTES_022019.7z","CAGEDEST_AJUSTES_032019.7z"))
  if (grepl("CAGED_AJUSTES/2018$", path))            return(c("CAGEDEST_AJUSTES_012018.7z","CAGEDEST_AJUSTES_022018.7z"))
  NULL
}

test_that(".hf_list_dir retorna NULL para path inexistente no HF", {
  skip_on_cran()
  skip_on_ci()  # teste de integracao real
  result <- datacaged:::.hf_list_dir("caminho_inexistente_xyz123abc", timeout = 10)
  expect_null(result)
})

test_that("caged_hf_files retorna tibble com colunas corretas (novo)", {
  local_mocked_bindings(
    .hf_list_dir = .hf_mock_novo,
    .package = "datacaged"
  )
  result <- caged_hf_files(n = 3, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_lte(nrow(result), 3)
  expect_true(all(result$mes >= 1 & result$mes <= 12))
  expect_true(all(result$ano >= 2020))
  expect_true(all(grepl("^https://", result$url)))
  expect_true(all(diff(as.integer(result$competencia)) <= 0))
})

test_that("caged_hf_files retorna tibble com colunas corretas (antigo)", {
  local_mocked_bindings(
    .hf_list_dir = .hf_mock_antigo,
    .package = "datacaged"
  )
  result <- caged_hf_files(type = "antigo", n = 3, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(all(result$ano <= 2019))
})

test_that("caged_hf_files retorna tibble com colunas corretas (ajustes)", {
  local_mocked_bindings(
    .hf_list_dir = .hf_mock_ajustes,
    .package = "datacaged"
  )
  result <- suppressWarnings(caged_hf_files(type = "ajustes", n = 3, verbose = FALSE))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("competencia", "ano", "mes", "url") %in% names(result)))
  expect_true(all(result$ano <= 2019))
  expect_true(all(grepl("ajustes", result$url, ignore.case = TRUE)))
})

test_that("caged_hf_files valida argumento n", {
  # Validacao de argumentos — sem rede, erro ocorre antes da conexao
  expect_error(caged_hf_files(n = 0),  "inteiro positivo")
  expect_error(caged_hf_files(n = -1), "inteiro positivo")
  expect_error(caged_hf_files(n = NA), "inteiro positivo")
})

test_that("caged_ftp_files emite warning de deprecacao", {
  local_mocked_bindings(
    caged_hf_files = function(...) tibble::tibble(),
    .package = "datacaged"
  )
  warned <- FALSE
  withCallingHandlers(
    caged_ftp_files(n = 1, verbose = FALSE),
    warning = function(w) {
      if (grepl("renamed|hf_files|renomeada", conditionMessage(w), ignore.case = TRUE))
        warned <<- TRUE
      invokeRestart("muffleWarning")
    }
  )
  expect_true(warned)
})
