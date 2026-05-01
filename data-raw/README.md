# data-raw/

Scripts que geram os dados embutidos no pacote (`data/`).

Conforme [R Packages 2e, cap. 7](https://r-pkgs.org/data.html), os dados exportados
devem ter sua origem documentada aqui.

| Script | Dado gerado | Descrição |
|---|---|---|
| `uf_codigos.R` | `data/uf_codigos.rda` | Tabela de siglas de UF, códigos IBGE e regiões |

## Como regenerar

```r
source("data-raw/uf_codigos.R")
```
