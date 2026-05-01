# test-utils.R
# Testes das funções utilitárias internas de datacaged.R

test_that(".validate_states aceita siglas válidas", {
  expect_equal(datacaged:::.validate_states("sp"), "SP")
  expect_equal(datacaged:::.validate_states(c("SP", "rj", "MG")), c("SP", "RJ", "MG"))
  expect_null(datacaged:::.validate_states(NULL))
})

test_that(".validate_states rejeita siglas inválidas", {
  expect_error(datacaged:::.validate_states("XX"), "inválida")
  expect_error(datacaged:::.validate_states(c("SP", "ZZ")), "inválida")
})

test_that(".validate_years aceita years válidos", {
  expect_equal(datacaged:::.validate_years(2020), 2020L)
  expect_equal(datacaged:::.validate_years(c(2018, 2019, 2020)), c(2018L, 2019L, 2020L))
})

test_that(".validate_years rejeita years fora do intervalo", {
  expect_error(datacaged:::.validate_years(1991), "inválidos")
  expect_error(datacaged:::.validate_years(as.integer(format(Sys.Date(), "%Y")) + 1), "inválidos")
})

test_that(".validate_months aceita months de 1 a 12", {
  expect_equal(datacaged:::.validate_months(1:12), 1:12)
  expect_equal(datacaged:::.validate_months(6), 6L)
})

test_that(".validate_months rejeita valores inválidos", {
  expect_error(datacaged:::.validate_months(0), "inválidos")
  expect_error(datacaged:::.validate_months(13), "inválidos")
})

test_that(".format_period formata corretamente", {
  expect_equal(datacaged:::.format_period(2023, 1),  "202301")
  expect_equal(datacaged:::.format_period(2023, 12), "202312")
  expect_equal(datacaged:::.format_period(2019, 6),  "201906")
})

test_that(".is_new_caged retorna TRUE para 2020+", {
  expect_true(datacaged:::.is_new_caged(2020))
  expect_true(datacaged:::.is_new_caged(2023))
  expect_false(datacaged:::.is_new_caged(2019))
  expect_false(datacaged:::.is_new_caged(2000))
})

