# datacaged 0.2.0 — CRAN Re-submission

## Resumo das mudanças

Esta versão migra a fonte dos dados do FTP do MTE para o repositório
HuggingFace <https://huggingface.co/datasets/alexsandroprado/caged>,
eliminando dependência de FTP puro (porta 21) e substituindo por HTTPS.

### Principais alterações

* Fonte migrada de `ftp://ftp.mtps.gov.br/pdet/microdados/` para
  `https://huggingface.co/datasets/alexsandroprado/caged`.
* Nova função `caged_hf_files()` substitui `caged_ftp_files()` (mantida
  como alias com aviso de deprecação).
* `httr2 (>= 1.0.0)` adicionado a `Imports` para consultas à API HuggingFace.
* Adicionado ORCID do autor: `0000-0002-7072-3621`.
* Adicionados exemplos executáveis sem internet (`system.file("extdata", ...)`).

### Acrônimos

* **CAGED**: Cadastro Geral de Empregados e Desempregados (Brazilian labor
  registry maintained by the Ministry of Labor and Employment).
* **MTE**: Ministério do Trabalho e Emprego (Brazilian Ministry of Labor).
* **DuckDB**: name of the embedded analytical database engine
  (<https://duckdb.org/>).

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* `NOTE: New submission` — espera-se este NOTE pois o pacote não estava
  disponível no CRAN nesta versão.

## Plataformas testadas

* Windows 11 x64 (R 4.6.0) — OK
* Ubuntu 24.04 LTS via GitHub Actions (R release, devel, oldrel-1) — OK
* macOS Sonoma arm64 (R release via GitHub Actions) — OK

## Compatibilidade cross-platform

O pacote é compatível com Windows, macOS e Linux sem configuração adicional
para o Novo CAGED (2020+, compressão LZMA).

O CAGED antigo (pré-2020, compressão PPMd) requer 7-Zip externo, documentado
em `SystemRequirements: 7-Zip (optional, ...)` no DESCRIPTION.

## Novas dependências (v0.2.0)

* `httr2`: downloads HTTPS com controle de timeout e retry
* `future` + `furrr`: downloads paralelos (`workers = 3` por padrão)
* `progressr`: barra de progresso compatível com execução paralela

Todas disponíveis no CRAN para Windows, macOS e Linux.

## Notes on R CMD check warnings

### Vignettes warning on Windows
The warning "Files in the 'vignettes' directory but no files in 'inst/doc'"
occurs on Windows due to a staged installation issue with R 4.6.0 when the
package name exceeds the 8.3 filename limit (DATACA~1). This does not affect
the tarball submitted to CRAN, where vignettes are built successfully as
confirmed by `── creating vignettes (12s)` in the build log.

The vignettes build correctly on Linux and macOS (verified via GitHub Actions CI).
