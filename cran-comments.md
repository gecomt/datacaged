# datacaged 0.2.1 — CRAN Submission

## Test environments

* Windows 11 x64 (R 4.6.0, local)
* win-builder R-devel (Windows Server 2022 x64)
* GitHub Actions: Ubuntu 24.04 (R release, devel, oldrel-1)
* GitHub Actions: macOS Sonoma arm64 (R release)

## R CMD check results

```
0 errors | 0 warnings | 1 note
```

### NOTE: New submission

Expected for a first CRAN submission.

### NOTE: Possibly misspelled words in DESCRIPTION

The following words flagged by the spell checker are intentional:

- **Cadastro, Geral, Empregados, Desempregados, de, Novo**: parts of the
  official Brazilian government acronym CAGED (Cadastro Geral de Empregados
  e Desempregados). These are proper nouns in Portuguese — the official name
  of Brazil's General Register of Employed and Unemployed Workers.
- **HuggingFace**: name of the data hosting platform
  (<https://huggingface.co>).
- **Microdata / microdata**: standard English term for unit-record
  (individual-level) data files.
- **pre**: prefix in "pre-2020", referring to the period before the 2020
  CAGED reform.

## Acronyms used in DESCRIPTION

- **CAGED**: Cadastro Geral de Empregados e Desempregados — Brazilian
  government labor registry maintained by MTE.
- **MTE**: Ministério do Trabalho e Emprego (Brazilian Ministry of Labor
  and Employment).
- **DuckDB**: embedded analytical database engine (<https://duckdb.org/>).

## Platform compatibility

The package works on Windows, macOS and Linux without additional
configuration for Novo CAGED (2020+, LZMA compression).

The legacy CAGED (pre-2020, PPMd compression) requires an external 7-Zip
binary, documented in `SystemRequirements` in DESCRIPTION.

## Dependencies (all on CRAN)

- `httr2`: HTTPS downloads with timeout and retry
- `future` + `furrr`: parallel downloads (default `workers = 3`)
- `progressr`: progress bar compatible with parallel execution
- `archive`: streaming decompression of `.7z` files
