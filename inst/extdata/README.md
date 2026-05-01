# inst/extdata/

Arquivos de exemplo para testes e documentação do pacote.

| Arquivo | Descrição |
|---|---|
| `CAGEDMOV202301_exemplo.txt` | 20 linhas do Novo CAGED MOV (jan/2023) — layout real, dados fictícios |
| `CAGED201801SP_exemplo.txt`  | 20 linhas do CAGED antigo SP (jan/2018) — layout real, dados fictícios |

Esses arquivos são usados nos exemplos de `caged_parse()` e nos testes de integração.

## Acesso em código

```r
system.file("extdata", "CAGEDMOV202301_exemplo.txt", package = "datacaged")
```
