# test-download.R
# Testes da construção de URLs e lógica de cache

test_that(".ftp_url gera URL correta para Novo CAGED MOV", {
  url <- datacaged:::.ftp_url(2023, 1, type = "MOV")
  expect_match(url, "^ftp://ftp[.]mtps[.]gov[.]br.*NOVO%20CAGED/2023/202301/CAGEDMOV202301[.]7z$")
})

test_that(".ftp_url gera URL correta para Novo CAGED FOR", {
  url <- datacaged:::.ftp_url(2022, 11, type = "FOR")
  expect_match(url, "^ftp://ftp[.]mtps[.]gov[.]br.*CAGEDFOR202211[.]7z$")
})

test_that(".ftp_url gera URL correta para Novo CAGED EXC", {
  url <- datacaged:::.ftp_url(2021, 6, type = "EXC")
  expect_match(url, "^ftp://ftp[.]mtps[.]gov[.]br.*CAGEDEXC202106[.]7z$")
})

test_that(".ftp_url gera URL correta para CAGED antigo", {
  url <- datacaged:::.ftp_url(2019, 3, type = "ANTIGO")
  expect_match(url, "^ftp://ftp[.]mtps[.]gov[.]br.*CAGEDEST_032019[.]7z$")
})

test_that(".ftp_url erro para type inválido", {
  expect_error(
    datacaged:::.ftp_url(2023, 1, type = "XYZ"),
    "Tipo inválido"
  )
})

# -------------------------------
# montar_tarefas
# -------------------------------

test_that(".build_tasks gera 3 tipos para Novo CAGED", {
  tarefas <- datacaged:::.build_tasks(
    years  = 2023,
    months = 1
  )
  
  expect_equal(sort(unique(tarefas$type)), c("EXC", "FOR", "MOV"))
  expect_true(all(is.na(tarefas$uf)))
  expect_true(all(!is.na(tarefas$url)))
})

test_that(".build_tasks gera 1 linha por competência para CAGED antigo", {
  tarefas <- datacaged:::.build_tasks(
    years  = 2018,
    months = c(1L, 2L, 3L)
  )
  
  expect_equal(nrow(tarefas), 3)
  expect_true(all(tarefas$type == "ANTIGO"))
  expect_true(all(is.na(tarefas$uf)))
  expect_match(tarefas$nome_arquivo[1], "CAGEDEST_")
  expect_true(all(!is.na(tarefas$url)))
})

test_that(".build_tasks ignora competências futuras", {
  ano_futuro <- as.integer(format(Sys.Date(), "%Y")) + 1
  
  tarefas <- datacaged:::.build_tasks(
    years  = ano_futuro,
    months = seq_len(12L)
  )
  
  expect_equal(nrow(tarefas), 0)
})

# -------------------------------
# consistência interna (NOVO)
# -------------------------------

test_that("montar_tarefas e url_ftp são consistentes (NOVO CAGED)", {
  
  tarefas <- datacaged:::.build_tasks(
    years  = 2023,
    months = 1
  )
  
  for (i in seq_len(nrow(tarefas))) {
    linha <- tarefas[i, ]
    
    url <- datacaged:::.ftp_url(
      year = 2023,
      month = 1,
      type = linha$type
    )
    
    expect_equal(url, linha$url)
  }
})

# -------------------------------
# consistência interna (ANTIGO)
# -------------------------------

test_that("montar_tarefas e url_ftp são consistentes (CAGED antigo)", {
  
  tarefas <- datacaged:::.build_tasks(
    years  = 2019,
    months = 3
  )
  
  expect_equal(nrow(tarefas), 1)
  
  url <- datacaged:::.ftp_url(
    year = 2019,
    month = 3,
    type = "ANTIGO"
  )
  
  expect_equal(url, tarefas$url[1])
})

# -------------------------------
# cache
# -------------------------------

test_that(".cache_dir cria diretório se não existir", {
  tmp <- file.path(tempdir(), "datacaged_test_cache")
  on.exit(unlink(tmp, recursive = TRUE))
  
  dir <- datacaged:::.cache_dir(tmp)
  
  expect_true(dir.exists(dir))
  expect_equal(dir, tmp)
})

test_that(".is_cached retorna FALSE para arquivo inexistente", {
  expect_false(datacaged:::.is_cached(tempfile()))
})

test_that(".is_cached retorna TRUE para arquivo existente com conteúdo", {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  
  writeLines("teste", tmp)
  
  expect_true(datacaged:::.is_cached(tmp))
})
