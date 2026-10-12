## Mensagens do pacote sao traduzidas conforme o idioma da sessao. Os testes
## conferem textos em ingles (o idioma do codigo-fonte), entao fixamos "en"
## durante toda a suite. Em locale C a troca nao tem efeito (as mensagens ja
## saem em ingles) e o withr apenas avisaria, por isso o suppressWarnings().
suppressWarnings(
  withr::local_language("en", .local_envir = testthat::teardown_env())
)
