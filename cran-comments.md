# datacaged 0.1.1 — CRAN Re-submission

## Resposta ao revisor

Obrigado pelo feedback. As seguintes correções foram aplicadas:

* `'DuckDB'` agora está entre aspas simples em `Title:` e `Description:`
  no arquivo `DESCRIPTION`, conforme solicitado.

* A URL `https://www.ibge.gov.br/explica/codigos-dos-municipios.php` em
  `man/uf_codigos.Rd` foi mantida — o revisor confirmou que o timeout é
  causado pela inacessibilidade de servidores do governo brasileiro a partir
  de fora do Brasil, e indicou que confia que o link funciona.

* Confirmamos que o pacote funciona conforme esperado: testado em
  Windows 11 (R 4.6.0) e Ubuntu 24.04 (R release/devel/oldrel).

## R CMD check results

0 errors | 0 warnings | 1 note

## Notes

* **New submission** — `NOTE: New submission` é esperado na primeira
  submissão ao CRAN.

## Plataformas testadas

* Windows 11 x64 (R 4.6.0) — OK
* Ubuntu 24.04 LTS via GitHub Actions (R release, devel, oldrel-1) — OK
