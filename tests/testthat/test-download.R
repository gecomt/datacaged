# test-download.R

test_that(".hf_url gera URL correta para Novo CAGED MOV", {
  url <- datacaged:::.hf_url(2023, 1, type = "MOV")
  expect_match(url, "^https://huggingface\\.co/datasets/alexsandroprado/caged/resolve/main/NOVO_CAGED/2023/202301/CAGEDMOV202301\\.7z$")
})

test_that(".hf_url gera URL correta para Novo CAGED FOR", {
  url <- datacaged:::.hf_url(2022, 11, type = "FOR")
  expect_match(url, "CAGEDFOR202211\\.7z$")
})

test_that(".hf_url gera URL correta para Novo CAGED EXC", {
  url <- datacaged:::.hf_url(2021, 6, type = "EXC")
  expect_match(url, "CAGEDEXC202106\\.7z$")
})

test_that(".hf_url gera URL correta para CAGED antigo", {
  url <- datacaged:::.hf_url(2019, 3, type = "ANTIGO")
  expect_match(url, "CAGED/2019/CAGEDEST_032019\\.7z$")
})

test_that(".hf_url gera URL correta para CAGED AJUSTES", {
  url <- datacaged:::.hf_url(2018, 5, type = "AJUSTES")
  expect_match(url, "CAGED_AJUSTES/2018/CAGEDAJUSTES_052018\\.7z$")
})

test_that(".hf_url erro para type inválido", {
  expect_error(
    datacaged:::.hf_url(2023, 1, type = "XYZ")
  )
})

test_that(".build_tasks gera tarefas corretas para Novo CAGED", {
  tasks <- datacaged:::.build_tasks(2023L, 1L)
  expect_s3_class(tasks, "data.frame")
  expect_equal(nrow(tasks), 3L)  # MOV, FOR, EXC
  expect_true(all(c("MOV", "FOR", "EXC") %in% tasks$type))
  expect_true(all(grepl("NOVO_CAGED", tasks$url)))
})

test_that(".build_tasks e .hf_url são consistentes (NOVO CAGED)", {
  tasks <- datacaged:::.build_tasks(2023L, 1L)
  for (tipo in c("MOV", "FOR", "EXC")) {
    url_direto <- datacaged:::.hf_url(2023L, 1L, type = tipo)
    url_task   <- tasks$url[tasks$type == tipo]
    expect_equal(url_task, url_direto)
  }
})

test_that(".build_tasks e .hf_url são consistentes (CAGED antigo)", {
  tasks <- datacaged:::.build_tasks(2019L, 3L)
  expect_equal(nrow(tasks), 1L)
  expect_equal(tasks$type, "ANTIGO")
  url_direto <- datacaged:::.hf_url(2019L, 3L, type = "ANTIGO")
  expect_equal(tasks$url, url_direto)
})

test_that(".build_tasks ignora competências futuras", {
  ano_futuro <- as.integer(format(Sys.Date(), "%Y")) + 1L
  tasks <- datacaged:::.build_tasks(ano_futuro, 1L)
  expect_equal(nrow(tasks), 0L)
})

test_that(".is_cached retorna FALSE para arquivo inexistente", {
  expect_false(datacaged:::.is_cached(tempfile()))
})

test_that(".cache_dir cria diretório se não existir", {
  tmp <- file.path(tempdir(), paste0("datacaged_test_", Sys.getpid()))
  on.exit(unlink(tmp, recursive = TRUE))
  dir_criado <- datacaged:::.cache_dir(tmp)
  expect_true(dir.exists(dir_criado))
})

test_that(".download_file retorna NULL para URL inexistente (404)", {
  skip_on_cran()
  # URL que definitivamente não existe no repositório HF
  url_404 <- paste0(
    "https://huggingface.co/datasets/alexsandroprado/caged/resolve/main/",
    "arquivo_que_nao_existe_xyz123.7z"
  )
  destfile <- tempfile(fileext = ".7z")
  on.exit(unlink(destfile))

  result <- datacaged:::.download_file(url_404, destfile, timeout = 15)
  expect_null(result)
  expect_false(file.exists(destfile))
})
