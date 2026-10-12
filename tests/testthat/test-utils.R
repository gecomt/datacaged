# test-utils.R
# Testes das funções utilitárias internas de datacaged.R

test_that(".validate_states aceita siglas válidas", {
  expect_equal(datacaged:::.validate_states("sp"), "SP")
  expect_equal(datacaged:::.validate_states(c("SP", "rj", "MG")), c("SP", "RJ", "MG"))
  expect_null(datacaged:::.validate_states(NULL))
})

test_that(".validate_states rejeita siglas inválidas", {
  expect_error(datacaged:::.validate_states("XX"), "Invalid UF")
  expect_error(datacaged:::.validate_states(c("SP", "ZZ")), "Invalid UF")
})

test_that(".validate_years aceita years válidos", {
  expect_equal(datacaged:::.validate_years(2020), 2020L)
  expect_equal(datacaged:::.validate_years(c(2018, 2019, 2020)), c(2018L, 2019L, 2020L))
})

test_that(".validate_years rejeita years fora do intervalo", {
  expect_error(datacaged:::.validate_years(1991), "Invalid")
  ## CAGED antigo comeca em 2007 no HuggingFace
  expect_error(datacaged:::.validate_years(2006), "Invalid")
  expect_equal(datacaged:::.validate_years(2007), 2007L)
  ## Ajustes comecam em 2002
  expect_equal(datacaged:::.validate_years(2002, min_year = 2002L), 2002L)
  expect_error(datacaged:::.validate_years(2001, min_year = 2002L), "Invalid")
  expect_error(datacaged:::.validate_years(as.integer(format(Sys.Date(), "%Y")) + 1), "Invalid")
})

test_that(".validate_months aceita months de 1 a 12", {
  expect_equal(datacaged:::.validate_months(1:12), 1:12)
  expect_equal(datacaged:::.validate_months(6), 6L)
})

test_that(".validate_months rejeita valores inválidos", {
  expect_error(datacaged:::.validate_months(0), "Invalid")
  expect_error(datacaged:::.validate_months(13), "Invalid")
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


test_that(".normalize_names remove acentos corretamente", {
  fn <- datacaged:::.normalize_names
  # Vogais acentuadas comuns no CAGED antigo
  expect_equal(fn("compet\u00eancia"),          "competencia")
  expect_equal(fn("saldomovimenta\u00e7\u00e3o"), "saldomovimentacao")
  expect_equal(fn("munic\u00edpio"),            "municipio")
  expect_equal(fn("ra\u00e7a_cor"),            "raca_cor")
  # Colunas sem acento devem ser preservadas
  expect_equal(fn("competenciamov"),           "competenciamov")
  expect_equal(fn("ibge_subsetor"),            "ibge_subsetor")
  # Espaços e chars especiais viram _
  expect_equal(fn("nome coluna"),              "nome_coluna")
  expect_equal(fn("_borda_"),                  "borda")
})

test_that(".remove_acentos remove acentos minusculos e maiusculos", {
  fn <- datacaged:::.remove_acentos
  expect_equal(fn("Movimenta\u00e7\u00e3o"), "Movimentacao")
  expect_equal(fn("SAL\u00c1RIO \u00c9 N\u00c3O"), "SALARIO E NAO")
  expect_equal(fn("sem acento"), "sem acento")
})

test_that(".normalize_names independe de acentos maiusculos", {
  fn <- datacaged:::.normalize_names
  expect_equal(fn("SAL\u00c1RIO"), "salario")
  expect_equal(fn("Se\u00e7\u00e3o"), "secao")
})
