# datacaged 0.2.3 — CRAN Submission

## Update

This is a patch release of a package already on CRAN (0.2.1); version 0.2.2
was never submitted, so this submission includes its changes. It fixes the
test ERRORs on the CRAN check results page and two further bugs found while
investigating them.

### 1. Test ERRORs on CRAN (duckdb >= 1.5)

Reported on r-devel-linux-x86_64-debian-gcc, r-devel-linux-x86_64-fedora-clang,
r-devel-linux-x86_64-fedora-gcc, r-patched-linux-x86_64 and
r-release-windows-x86_64. duckdb >= 1.5 caches database instances per file
path and refuses `read_only = TRUE` when an instance for the same path is
already open. `caged_load()` and `caged_adjustments_load()` called
`caged_info()`, which opens read-only, while their own write connection was
still open. Because `dbDisconnect(shutdown = TRUE)` no longer releases the
cached instance, connections are now closed and the instance released with
`duckdb::duckdb_shutdown()`. Read-only helpers reuse an already-open instance
instead of failing.

### 2. Silent data loss in two output tables

`caged_to_duckdb()` deduplicates by a per-file period key. For late
declarations (`caged_for`) and exclusions (`caged_exc`) the key was taken from
a column that also contains earlier months, so loading the next month
silently skipped rows (about 46% of one real monthly file). The key is now the
column that identifies the source file. NEWS.md tells users to reload these
two tables.

### 3. Locale-dependent column names

Column names were normalised with accented characters used as *names* of a
character vector. Names are symbols and are converted to the native encoding
when the package is installed, so under a non-UTF-8 locale (e.g. `C`) the
patterns became literal `<U+00E1>` strings and accents were not removed;
`read_delim(name_repair = "universal")` was also locale-dependent. Loading
then failed under such locales. Normalisation now uses `chartr()` and
`name_repair = "unique_quiet"`; the test suite passes under the `C` locale.

### 4. Tests no longer use the network on CRAN

Two tests downloaded real files during CRAN checks (one from the MTE FTP
server, one ~47 MB from HuggingFace). They are now mocked; tests that need the
network use `skip_on_cran()` and `skip_if_offline()`. The full suite passes
with network access blocked.

### 5. Message translations

Console messages are now written in English and translated to Brazilian
Portuguese via `gettext()` (`po/R-pt_BR.po`). Tests pin the language to
English with `withr::local_language()` (withr added to Suggests), so they do
not depend on the check machine's locale; the suite was run under
`pt_BR.UTF-8`, `en_US.UTF-8` and `C` locales.

Other changes (incremental update logic, transactional writes, Parquet
partitioning) are listed in NEWS.md.

## Test environments

* Local: Windows 11 x64, R 4.6.1 (ucrt), `--as-cran --run-donttest`
* win-builder: R-devel (2026-10-10 r90659 ucrt), Windows Server 2022 x64
* GitHub Actions: macos-latest (R release), windows-latest (R release),
  ubuntu-latest (R devel, release and oldrel-1)
* Test suite also run under the pt_BR.UTF-8, en_US.UTF-8 and C locales (Linux)

## R CMD check results

```
0 errors | 0 warnings | 0 notes
```

All environments above returned no ERRORs, WARNINGs or NOTEs.

### Possible NOTE: misspelled words in DESCRIPTION

Where a spell checker is available, the following words may be flagged; they
are intentional:

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