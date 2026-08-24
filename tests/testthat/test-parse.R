# test-parse.R
# Testes de leitura, normalização e detecção de type

# ── Helpers de fixture ────────────────────────────────────────────────────────

#' Cria um arquivo .txt simulando o Novo CAGED MOV com separador ";"
make_txt_novo_mov <- function(path, n = 5) {
  header <- paste(
    "competencia", "regiao", "uf", "municipio", "secao", "subclasse",
    "saldomovimentacao", "numerocnpj", "razaosocial", "naturezajuridica",
    "indicadoraprendiz", "subatividade", "tipomovimentacao", "tipodedeficiencia",
    "indtrabintermitente", "indtrabparcial", "salario", "tamestabjan",
    "escolaridade", "idade", "horascontratuais", "racacor", "sexo",
    "tipoempregador", "tipoestabelecimento", "tipologradouro", "logradouro",
    "numero", "complemento", "bairro", "cep", "pais", "codigomunicipio",
    sep = ";"
  )
  rows <- replicate(n, paste(
    "202301", "3", "35", "3550308", "C", "1011201",
    "1", "12345678000195", "EMPRESA TESTE LTDA", "2062",
    "0", "10112", "1", "0",
    "0", "0", "2500,00", "3",
    "7", "30", "44", "2", "1",
    "1", "1", "1", "RUA TESTE",
    "123", "", "CENTRO", "01310100", "1058", "3550308",
    sep = ";"
  ))
  writeLines(c(header, rows), path, useBytes = FALSE)
}

#' Cria arquivo .txt simulando CAGED antigo
make_txt_antigo <- function(path, n = 5) {
  header <- paste(
    "competencia", "regiao", "uf", "municipio", "subsetor", "subatividade",
    "saldomovimentacao", "razaosocial", "indicadoraprendiz", "tipomovimentacao",
    "tipodedeficiencia", "salario", "escolaridade", "idade",
    "horascontratuais", "racacor", "sexo",
    sep = ";"
  )
  rows <- replicate(n, paste(
    "201801", "3", "35", "3550308", "10", "10112",
    "1", "EMPRESA ANTIGA LTDA", "0", "1",
    "0", "1800,00", "6", "25",
    "44", "2", "1",
    sep = ";"
  ))
  writeLines(c(header, rows), path, useBytes = FALSE)
}

# ── Testes de detecção de type ────────────────────────────────────────────────

test_that(".detect_type identifica corretamente cada prefixo", {
  expect_equal(datacaged:::.detect_type("CAGEDMOV202301.7z"),  "MOV")
  expect_equal(datacaged:::.detect_type("CAGEDFOR202301.7z"),  "FOR")
  expect_equal(datacaged:::.detect_type("CAGEDEXC202301.7z"),  "EXC")
  expect_equal(datacaged:::.detect_type("CAGED202301SP.7z"),   "ANTIGO")
  expect_equal(datacaged:::.detect_type("/path/to/CAGEDMOV202212.txt"), "MOV")
})

# ── Testes de normalização de nomes ──────────────────────────────────────────

test_that(".normalize_names remove acentos e padroniza", {
  nomes <- c("Competência", "salário", "raça_cor", "UF Código")
  result <- datacaged:::.normalize_names(nomes)
  expect_equal(result, c("competencia", "salario", "raca_cor", "uf_codigo"))
  expect_true(all(result == tolower(result)))
  expect_false(any(grepl("[áéíóúàèìòùãõâêîôûç ]", result)))
})

# ── Testes de leitura de arquivo ──────────────────────────────────────────────

test_that(".read_text_file lê Novo CAGED MOV corretamente", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  make_txt_novo_mov(tmp, n = 10)

  df <- datacaged:::.read_text_file(tmp, type = "MOV")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 10)
  expect_true("competencia" %in% names(df))
  expect_true("saldomovimentacao" %in% names(df))
  expect_true("salario" %in% names(df))
  expect_true("fonte_tipo" %in% names(df))
  expect_equal(unique(df$fonte_tipo), "MOV")
})

test_that(".read_text_file lê CAGED antigo corretamente", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  make_txt_antigo(tmp, n = 7)

  df <- datacaged:::.read_text_file(tmp, type = "ANTIGO")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 7)
  expect_true("subsetor" %in% names(df))
  expect_equal(unique(df$fonte_tipo), "ANTIGO")
})

test_that(".read_text_file retorna NULL para arquivo inexistente", {
  # cli::cli_warn() emite uma condição da classe c("cli_warning","warning"),
  # que expect_warning() intercepta corretamente.
  tmp_inexistente <- tempfile()
  on.exit(unlink(tmp_inexistente), add = TRUE)
  expect_warning(
    df <- datacaged:::.read_text_file(tmp_inexistente, type = "MOV"),
    regexp = "Erro ao ler"
  )
  expect_null(df)
})

test_that("salario é lido como numérico (decimal vírgula)", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp))
  make_txt_novo_mov(tmp, n = 3)

  df <- datacaged:::.read_text_file(tmp, type = "MOV")
  expect_true(is.numeric(df$salario))
  expect_equal(unique(df$salario), 2500.00, tolerance = 1e-6)
})

test_that("caged_parse retorna NULL para arquivo .7z inexistente", {
  expect_error(
    caged_parse("/caminho/inexistente/CAGEDMOV202301.7z")
  )
})

test_that("caged_parse_batch ignora paths inexistentes", {
  paths_invalidos <- c(tempfile(), tempfile())
  on.exit(unlink(paths_invalidos), add = TRUE)
  result <- caged_parse_batch(paths_invalidos, .progress = FALSE)
  expect_null(result)
})

