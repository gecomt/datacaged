# datacaged 0.2.3

## Message language

* All console messages (errors, warnings, progress) are now written in
  English in the source code and translated to Brazilian Portuguese through
  the standard R/gettext mechanism. Users with R set to Portuguese see them in
  Portuguese automatically; `Sys.setLanguage("pt_BR")` or
  `Sys.setLanguage("en")` switches the language within a session.
  Previously the messages mixed both languages and the shipped translations
  were never applied, because the `cli` calls did not go through `gettext()`.
* The translation catalog now contains only `R-pt_BR.po`; `R-en.po` was
  removed because the source language is English.
* The tests run in English regardless of the machine's locale, and a new test
  checks that every translation keeps the `{...}` code blocks of the original
  message unchanged.

## Bug fixes

* Messages such as "20 registro inserido" now use the correct plural ("20
  records inserted" / "20 registros inseridos"): the count was formatted as
  text before reaching `cli`, which then always used the singular.

# datacaged 0.2.2

## Bug fixes

* Fixed test failures on CRAN with duckdb >= 1.5, which caches database
  instances per file and refuses `read_only = TRUE` when an instance for the
  same path is already open (`read_only can't be applied to the database
  instance ... which already exists`):
  * `caged_load()` and `caged_adjustments_load()` now close their write
    connection before calling `caged_info()`.
  * The internal `.disconnect()` helper now also calls
    `duckdb::duckdb_shutdown()`, since `dbDisconnect(shutdown = TRUE)` no
    longer releases the cached instance.
  * `caged_info()`, `caged_to_parquet()` and `caged_update()` reuse an
    already-open instance (e.g. a user connection) instead of failing.
* **Data loss in `caged_for` and `caged_exc`**: the deduplication key
  `competencia` was taken from `competenciamov`, but FOR/EXC files contain
  movements from earlier months that repeat across files, so loading month
  *t+1* silently skipped rows whose `competenciamov` had already appeared
  (e.g. 46% of `CAGEDFOR202303`). The key is now `competenciadec` for
  `caged_for` and `competenciaexc` for `caged_exc` (one value per file).
  `caged_mov`, `caged_antigo` and `caged_ajustes` are unchanged.
  **Action required:** databases built with earlier versions should drop
  `caged_for` and `caged_exc` and reload them with `caged_load()`. Use
  `competenciamov` for analyses by month of movement.
* Fixed column-name normalization when the package is installed or run in a
  non-UTF-8 locale (e.g. `C`/`POSIX`): accented headers such as
  `competênciamov` became `compet_nciamov`, and `caged_to_duckdb()` aborted
  with "Nenhuma coluna de competencia encontrada". `.normalize_names()` now
  uses `chartr()` and `read_delim()` uses `name_repair = "unique_quiet"`.

## Behaviour changes

* `caged_download_layouts()` now downloads the layout files from the
  HuggingFace repository instead of the MTE FTP server, which was slow and
  often unreachable. Local file names have accents removed; the original
  name is kept in the `arquivo` column.
* `caged_update()` now compares competencies table by table instead of using
  the maximum competency. It also fills gaps inside the period already in the
  database (e.g. a FOR file whose download failed) and calls the pipeline
  once per year, so it no longer downloads unneeded months (requesting
  202512 and 202601 used to also download 202501 and 202612).
* Year validation now matches the data available on HuggingFace: legacy CAGED
  from 2007 (`caged_load()`, `caged_download()`) and CAGED Adjustments from
  2002 (`caged_adjustments_load()`). Earlier years now fail immediately with
  a clear message instead of finding no files.

## More bug fixes

* `caged_to_duckdb(overwrite_competencies = TRUE)` now runs DELETE and INSERT
  in a single transaction: if the insert fails, the previous data is kept.
* Errors while checking which competencies are already stored are no longer
  swallowed (they used to be treated as "none stored", duplicating rows).
* `caged_to_parquet()`: `partition_by` accepts several columns, `overwrite =
  FALSE` is respected for partitioned output, a missing partition column gives
  a clear warning, and paths containing `'` work. Partition directories for
  code columns (competencies, `uf`, `regiao`, `municipio`) are now named
  `competencia=202301/uf=35` instead of `competencia=202301.0/uf=35.0`; the
  cast to integer happens only in the export, so the database schema is
  unchanged.
* `caged_adjustments_load()` for 2002-2009, where the source is one annual
  file per year: the file is now downloaded and parsed once instead of once
  per requested month (about 6x faster for a full year), and only the
  requested `months` are stored (previously the whole year was stored).
* Downloads are written to a temporary `.part` file and renamed when
  complete, so an interrupted write can no longer leave a truncated file
  that the cache would treat as valid.
* The internal HuggingFace listing no longer fails under a non-UTF-8 locale
  for file names with accents.
* `caged_hf_files(verbose = FALSE)` no longer prints the "Querying
  HuggingFace" message.

## Tests

* No test accesses the network on CRAN anymore: the layouts test and the
  sequential download test are mocked; real-network tests use
  `skip_on_cran()` and `skip_if_offline()`.
* Fixed mocks leaking across test files: `on.exit()` without `add = TRUE`
  after `local_mocked_bindings()` discarded the restore handler.

## Documentation

* Corrected data coverage: legacy CAGED is available from 2007 and CAGED
  Adjustments from 2002 (previously documented as 1992).
* `caged_to_duckdb()` documentation now describes the actual deduplication
  mechanism and the key column per table (it never used `INSERT OR IGNORE`).
* `?datacaged` now lists `caged_update()`, `caged_to_parquet()` and
  `caged_download_layouts()`.
* `inst/CITATION` now reads the version from `DESCRIPTION`.
* Updated `caged_info()` sample output in the vignettes.

# datacaged 0.2.1

## Breaking changes

* `caged_ftp_files()` removed — use `caged_hf_files()` instead.

## Bug fixes

* Fixed `caged_hf_files(type = "ajustes")` returning years > 2019 — now
  correctly filters to years <= 2019.
* Fixed CAGED Adjustments file name pattern (`CAGEDEST_AJUSTES_` was
  incorrectly read as `CAGEDAJUSTES_`); updated `prefixo_len` from 13 to 17.
* Added support for `CAGED_AJUSTES/2002a2009/` folder (annual files
  `CAGEDEST_AJUSTES_AAAA.7z`).
* Fixed `.insert_rows()` to handle schema evolution when loading data from
  multiple years with different column sets (`ALTER TABLE ... ADD COLUMN`).
* Converted `\donttest{}` examples that require network access or a populated
  database to `\dontrun{}` to pass `R CMD check --run-donttest`.
* Fixed duplicate `caged_ftp_files` alias in documentation.
* Removed U+FE0F variation selector from source files (LaTeX PDF error).
* Updated CI workflows: added `concurrency`, `CI=true`, reduced coverage
  timeout to 15 min, added `dependabot.yml`.
* Replaced absolute file paths in examples (`~/Downloads/`, `D:/dados/`)
  with portable alternatives (`file.path(tempdir(), ...)`, `file.path("~", ...)`).

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
