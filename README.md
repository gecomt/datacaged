datacaged
================

[![R-CMD-check](https://github.com/gecomt/datacaged/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gecomt/datacaged/actions)
[![codecov](https://codecov.io/gh/gecomt/datacaged/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gecomt/datacaged)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE.md)


<img src="man/figures/logo.png" align="right" height="200" alt="datacaged logo" />

> Microdados do CAGED direto no seu DuckDB, com pipeline completo e pronto para análise.

O **datacaged** é um pacote R que automatiza todo o fluxo de trabalho
com os microdados do CAGED (Cadastro Geral de Empregados e
Desempregados), desde o download até o armazenamento estruturado em
banco analítico local (DuckDB), permitindo consultas rápidas e
escaláveis com `dplyr` ou SQL.

------------------------------------------------------------------------

## ✨ Principais funcionalidades

- 📥 Download automatizado dos microdados diretamente do FTP do MTE

- 📦 Leitura e normalização de arquivos `.7z` (CAGED antigo e Novo
  CAGED)

- 🗄️ Armazenamento eficiente em banco **DuckDB**

- 🔄 Pipeline completo em uma única função

- 📊 Integração com `dplyr` para análise de dados

- 🧩 Suporte a múltiplas bases:

  - Novo CAGED (2020+)
  - CAGED antigo (1992–2019)
  - CAGED Ajustes (correções históricas)

------------------------------------------------------------------------

## 🚀 Instalação

### Via GitHub

``` r
# install.packages("pak")
pak::pkg_install("gecomt/datacaged")
```

### Via remotes

``` r
# install.packages("remotes")
remotes::install_github("gecomt/datacaged")
```

------------------------------------------------------------------------

## ⚡ Uso rápido

``` r
library(datacaged)

# Pipeline completo
caged_load(
  years  = 2022:2023,
  months  = seq_len(12L),
  db_path = "caged.duckdb"
)

# Ajustes
caged_adjustments_load(
  years = 2018:2019,
  db_path = "caged.duckdb"
)

# Baixar os layouts oficiais do CAGED
caged_download_layouts(type = "ambos")

# Conexão
con <- caged_connect("caged.duckdb")

# Análise
library(dplyr)

dplyr::tbl(con, "caged_mov") |>
  filter(uf == 35, competenciamov >= 202201L) |>
  group_by(competencia) |>
  summarise(saldo = sum(saldomovimentacao, na.rm = TRUE)) |>
  collect()
```

------------------------------------------------------------------------

## 🧠 Pipeline

    Download → Extração → Parsing → Normalização → DuckDB → Análise

------------------------------------------------------------------------

## 📚 Funções principais

| Função                     | Descrição                     |
|----------------------------|-------------------------------|
| `caged_load()`             | Pipeline completo             |
| `caged_adjustments_load()` | Pipeline de ajustes           |
| `caged_download()`         | Download dos arquivos         |
| `caged_download_layouts()` | Download dos layouts oficiais |
| `caged_parse()`            | Leitura de arquivos           |
| `caged_parse_batch()`      | Processamento em lote         |
| `caged_to_duckdb()`        | Persistência no banco         |
| `caged_connect()`          | Conexão                       |
| `caged_info()`             | Metadados                     |
| `caged_status()`           | Status do FTP                 |
| `caged_ftp_files()`        | Lista arquivos                |

------------------------------------------------------------------------

## 📚 Download de layouts

O pacote também baixa os arquivos de layout oficiais do CAGED e Novo
CAGED, úteis para conferência de estrutura, dicionários de variáveis e
validação de colunas.

``` r
caged_download_layouts()
caged_download_layouts(type = "antigo")
caged_download_layouts(type = "novo")
caged_download_layouts(type = "ajustes")
```

A função salva os arquivos em cache local por padrão e retorna um tibble
com o status de cada download.

------------------------------------------------------------------------

## 🗄️ Estrutura do banco

| Tabela          | Conteúdo                   | Período   |
|-----------------|----------------------------|-----------|
| `caged_mov`     | Movimentações              | 2020+     |
| `caged_for`     | Informações complementares | 2020+     |
| `caged_exc`     | Exclusões                  | 2020+     |
| `caged_antigo`  | Histórico                  | 1992–2019 |
| `caged_ajustes` | Ajustes                    | 1992–2019 |

------------------------------------------------------------------------

## 📊 Casos de uso

- Análise do mercado de trabalho
- Indicadores econômicos
- Pesquisa acadêmica
- Monitoramento de emprego formal
- Modelagem econométrica

------------------------------------------------------------------------

## ⚙️ Dependências

- `duckdb`
- `dplyr`
- `archive`
- `readr`
- `cli`

------------------------------------------------------------------------

## 🧾 Requisitos

- R ≥ 4.1.0
- 7-Zip (opcional, para arquivos PPMd do CAGED antigo)

------------------------------------------------------------------------

## 📄 Licença

MIT © Alexsandro Prado

------------------------------------------------------------------------

## 🤝 Contribuições

Pull requests são bem-vindos. Para mudanças maiores, abra uma issue
primeiro.

------------------------------------------------------------------------

## 📬 Contato

Autor: Alexsandro Prado Email: <alexsandro.prado@ufersa.edu.br> GitHub:
<https://github.com/gecomt/datacaged>
