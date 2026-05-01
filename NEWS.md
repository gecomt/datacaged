# datacaged 0.1.0

Primeira versão do pacote — preparado para submissão ao CRAN.

## Funções exportadas

* `caged_load()` — pipeline completo: download → parse → DuckDB (Novo CAGED e antigo).
* `caged_download()` — baixa arquivos `.7z` do FTP do MTE com cache local e retry automático.
* `caged_parse()` — lê e normaliza microdados de um arquivo `.7z`.
* `caged_parse_batch()` — versão vetorizada de `caged_parse()` com barra de progresso.
* `caged_to_duckdb()` — grava dados no DuckDB com controle de duplicatas por competência.
* `caged_connect()` — abre conexão DBI com o banco DuckDB (aceita `quiet = TRUE`).
* `caged_info()` — exibe estatísticas das tabelas no banco (registros, range de competências).
* `caged_adjustments_load()` — pipeline completo para o CAGED Ajustes (série histórica até 2019).
* `caged_status()` — verifica disponibilidade do servidor FTP do MTE.
* `caged_ftp_files()` — lista competências disponíveis no FTP (Novo CAGED, antigo e Ajustes).
* `caged_download_layouts()` — baixa os layouts oficiais (dicionários) do MTE.
* Dataset `uf_codigos` — tabela de UFs com códigos IBGE e regiões.

## Características técnicas

* Suporte ao **Novo CAGED** (jan/2020 em diante): arquivos `CAGEDMOV`, `CAGEDFOR`, `CAGEDEXC`.
* Suporte ao **CAGED antigo** (até dez/2019): arquivos `CAGEDEST_*.7z` (arquivo nacional).
* Suporte ao **CAGED Ajustes**: arquivos `CAGEDAJUSTES_*.7z` (correções retroativas).
* Cache local em `tools::R_user_dir("datacaged", "cache")` com reúso automático entre sessões.
* Extração PPMd via 7-Zip (detectado via `Sys.which()` em Linux/macOS; Program Files + Rtools no Windows).
* Fallback para `archive::archive_extract()` quando 7-Zip não está disponível.
* Queries SQL parametrizadas via `DBI::dbBind()` — sem risco de SQL injection.
* Cross-platform: Windows (R ≥ 4.2.0), macOS e Linux.
