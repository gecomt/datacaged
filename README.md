[![R-CMD-check](https://github.com/gecomt/datacaged/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gecomt/datacaged/actions)
[![codecov](https://codecov.io/gh/gecomt/datacaged/branch/main/graph/badge.svg)](https://app.codecov.io/gh/gecomt/datacaged)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE.md)

<img src="man/figures/logo.svg" align="center" height="139" alt="datacaged logo" /> <img src="man/figures/logo.svg" align="right" height="139" alt="datacaged hex sticker"/>
> Microdados do CAGED direto no seu DuckDB, com pipeline completo e
> pronto para análise.

O **datacaged** é um pacote R que automatiza todo o fluxo de trabalho
com os microdados do CAGED (Cadastro Geral de Empregados e
Desempregados), desde o download até o armazenamento estruturado em
banco analítico local (DuckDB), permitindo consultas rápidas e
escaláveis com `dplyr` ou SQL.

------------------------------------------------------------------------

## ✨ Principais funcionalidades

-   📥 Download automatizado dos microdados diretamente do FTP do MTE

-   📦 Leitura e normalização de arquivos `.7z` (CAGED antigo e Novo
    CAGED)

-   🗄️ Armazenamento eficiente em banco **DuckDB**

-   🔄 Pipeline completo em uma única função

-   📊 Integração com `dplyr` para análise de dados

-   🧩 Suporte a múltiplas bases:

    -   Novo CAGED (2020+)
    -   CAGED antigo (1992–2019)
    -   CAGED Ajustes (correções históricas)

------------------------------------------------------------------------

## 🚀 Instalação

### Via GitHub

    # install.packages("pak")
    pak::pkg_install("gecomt/datacaged")

### Via remotes

    # install.packages("remotes")
    remotes::install_github("gecomt/datacaged")

------------------------------------------------------------------------

## ⚡ Uso rápido

    library(datacaged)

    # Pipeline completo
    caged_load(
      years   = 2022:2023,
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

------------------------------------------------------------------------

## 🧠 Pipeline

    Download → Extração → Parsing → Normalização → DuckDB → Análise

------------------------------------------------------------------------

## 📚 Funções principais

<table>
<colgroup>
<col style="width: 37%" />
<col style="width: 62%" />
</colgroup>
<thead>
<tr>
<th>Função</th>
<th>Descrição</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>caged_load()</code></td>
<td>Pipeline completo</td>
</tr>
<tr>
<td><code>caged_adjustments_load()</code></td>
<td>Pipeline de ajustes</td>
</tr>
<tr>
<td><code>caged_download()</code></td>
<td>Download dos arquivos</td>
</tr>
<tr>
<td><code>caged_download_layouts()</code></td>
<td>Download dos layouts oficiais</td>
</tr>
<tr>
<td><code>caged_parse()</code></td>
<td>Leitura de arquivos</td>
</tr>
<tr>
<td><code>caged_parse_batch()</code></td>
<td>Processamento em lote</td>
</tr>
<tr>
<td><code>caged_to_duckdb()</code></td>
<td>Persistência no banco</td>
</tr>
<tr>
<td><code>caged_connect()</code></td>
<td>Conexão</td>
</tr>
<tr>
<td><code>caged_info()</code></td>
<td>Metadados</td>
</tr>
<tr>
<td><code>caged_status()</code></td>
<td>Status do FTP</td>
</tr>
<tr>
<td><code>caged_ftp_files()</code></td>
<td>Lista arquivos</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

## 📚 Download de layouts

O pacote também baixa os arquivos de layout oficiais do CAGED e Novo
CAGED, úteis para conferência de estrutura, dicionários de variáveis e
validação de colunas.

    caged_download_layouts()
    caged_download_layouts(type = "antigo")
    caged_download_layouts(type = "novo")
    caged_download_layouts(type = "ajustes")

A função salva os arquivos em cache local por padrão e retorna um tibble com o status de cada download.

## 🗄️ Estrutura do banco

<table>
<thead>
<tr>
<th>Tabela</th>
<th>Conteúdo</th>
<th>Período</th>
</tr>
</thead>
<tbody>
<tr>
<td><code>caged_mov</code></td>
<td>Movimentações</td>
<td>2020+</td>
</tr>
<tr>
<td><code>caged_for</code></td>
<td>Informações complementares</td>
<td>2020+</td>
</tr>
<tr>
<td><code>caged_exc</code></td>
<td>Exclusões</td>
<td>2020+</td>
</tr>
<tr>
<td><code>caged_antigo</code></td>
<td>Histórico</td>
<td>1992–2019</td>
</tr>
<tr>
<td><code>caged_ajustes</code></td>
<td>Ajustes</td>
<td>1992–2019</td>
</tr>
</tbody>
</table>

------------------------------------------------------------------------

## 📊 Casos de uso

-   Análise do mercado de trabalho
-   Indicadores econômicos
-   Pesquisa acadêmica
-   Monitoramento de emprego formal
-   Modelagem econométrica

------------------------------------------------------------------------

## ⚙️ Dependências

-   `duckdb`
-   `dplyr`
-   `archive`
-   `readr`
-   `cli`

------------------------------------------------------------------------

## 🧾 Requisitos

-   R ≥ 4.1.0

-   7-Zip (opcional)

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
