# datacaged 0.2.0

## Breaking changes

* Source migrated from FTP do MTE (`ftp://ftp.mtps.gov.br/pdet/microdados/`) to
  HuggingFace dataset (`https://huggingface.co/datasets/alexsandroprado/caged`).
  Downloads now use HTTPS — no more FTP port 21 issues behind firewalls.

## New functions

* `caged_hf_files()`: replaces `caged_ftp_files()` — lists available competencies
  from the HuggingFace repository via its REST API.
* `caged_update()`: incremental update — downloads only competencies not yet in
  the local DuckDB bank, querying the bank max competência and HuggingFace availability.
* `caged_to_parquet()`: exports DuckDB tables to Parquet files using DuckDB's native
  `COPY TO ... (FORMAT PARQUET)` — supports optional column partitioning. — lists available competencies
  from the HuggingFace repository via its REST API.

## Deprecated

* `caged_ftp_files()`: kept as an alias with a deprecation warning. Will be removed
  in a future version. Use `caged_hf_files()` instead.

## Internal changes

* `download.R`: `.ftp_url()` renamed to `.hf_url()`; `.download_file()` now uses
  `method = "libcurl"` with HTTPS and `--location` to follow CDN redirects.
* `availability.R`: `.ftp_list()` renamed to `.hf_list_dir()`; uses `httr2` instead
  of curl FTP listing.
* `status.R`: `caged_status()` now checks HuggingFace API endpoint.
* Added `httr2 (>= 1.0.0)` to `Imports`.
# datacaged 0.1.2

* `DESCRIPTION`: URL do FTP do MTE adicionada ao campo `Description:`
  conforme política do CRAN.
* `man/*.Rd`: `\dontrun{}` substituído por `\donttest{}` em todos os
  exemplos — os exemplos requerem internet mas podem ser executados
  pelo usuário.
* `man/*.Rd`: `db_path = "caged.duckdb"` substituído por
  `db_path = file.path(tempdir(), "caged.duckdb")` em todos os exemplos
  para evitar escrita no diretório de trabalho do usuário.
# datacaged 0.1.1

* `DESCRIPTION`: `'DuckDB'` entre aspas simples em `Title` e `Description`
  conforme política do CRAN.
# datacaged 0.1.0

Primeira versão do pacote — submetida ao CRAN.

## Funções exportadas

* `caged_load()` — pipeline completo: download → parse → DuckDB (Novo CAGED e antigo).
* `caged_download()` — baixa arquivos `.7z` do FTP do MTE com cache local e retry automático.
* `caged_parse()` — lê e normaliza microdados de um arquivo `.7z`.
* `caged_parse_batch()` — versão vetorizada de `caged_parse()` com barra de progresso.
* `caged_to_duckdb()` — grava dados no DuckDB com controle de duplicatas por competência.
* `caged_connect()` — abre conexão DBI com o banco DuckDB.
* `caged_info()` — exibe estatísticas das tabelas no banco.
* `caged_adjustments_load()` — pipeline completo para o CAGED Ajustes.
* `caged_status()` — verifica disponibilidade do servidor FTP do MTE.
* `caged_ftp_files()` — lista competências disponíveis no FTP.
* `caged_download_layouts()` — baixa os layouts oficiais do MTE.
* Dataset `uf_codigos` — tabela de UFs com códigos IBGE e regiões.
