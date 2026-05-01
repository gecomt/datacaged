# datacaged 0.1.0 — CRAN Submission Comments

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* **New submission** — `NOTE: New submission` é esperado na primeira submissão
  ao CRAN e não indica nenhum problema com o pacote.

## Plataformas testadas

* Ubuntu 24.04 LTS (GitHub Actions / CI): R release, R devel, R oldrel-1 — OK
* Windows 11 x64 (local): R 4.4.x, `--as-cran` — OK
* macOS (local): R 4.4.x — OK

## Dependências de sistema

* **7-Zip** (opcional): necessário apenas para descompactar arquivos PPMd do
  CAGED antigo (pré-2020) e CAGED Ajustes. O pacote `archive` (listado em
  `Imports:`) é usado para todos os demais arquivos e não requer instalação
  adicional.

## Notas adicionais

* O pacote acessa o servidor FTP público do Ministério do Trabalho e Emprego
  (MTE) em `ftp://ftp.mtps.gov.br/pdet/microdados/`. Todos os testes que
  dependem de rede estão protegidos com `skip_on_cran()`.

* Os dados do CAGED são dados públicos do governo brasileiro — não há questões
  de licença ou privacidade nos dados de exemplo incluídos em `inst/extdata/`.
