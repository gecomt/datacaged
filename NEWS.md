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
