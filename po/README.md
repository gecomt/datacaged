# po/ — Traduções do datacaged

Este diretório contém os arquivos de internacionalização (i18n) do pacote,
seguindo o padrão GNU gettext conforme documentado em *Writing R Extensions*
e *R Packages 2e* (cap. 8).

## Estrutura

| Arquivo | Descrição |
|---|---|
| `R-datacaged.pot` | Template — todas as strings traduzíveis extraídas do código R |
| `R-pt_BR.po` | Tradução: Português do Brasil |
| `R-en.po` | Tradução: Inglês |

As traduções compiladas (`.mo`) ficam em `inst/po/` após `tools::update_pkg_po(".")`.

## Como adicionar uma nova língua

```r
# 1. Crie o arquivo .po a partir do template
file.copy("po/R-datacaged.pot", "po/R-es.po")

# 2. Edite po/R-es.po preenchendo os campos msgstr com as traduções
# (use Poedit, POEdit ou qualquer editor de texto)

# 3. Compile e instale as traduções
tools::update_pkg_po(".")
```

## Como atualizar o template após adicionar mensagens ao código

```r
# Extrai strings novas dos arquivos R e atualiza o .pot
tools::update_pkg_po(".")
```

## Fluxo gettext em pacotes R

```
Código R (.R)
    ↓ tools::xgettext()
po/R-datacaged.pot    ← template com msgid, msgstr vazio
    ↓ msginit / copiar e traduzir
po/R-pt_BR.po         ← tradução completa
    ↓ tools::update_pkg_po() / msgfmt
inst/po/pt_BR/LC_MESSAGES/R-datacaged.mo   ← binário instalado
```

## Referências

- [Writing R Extensions — Internationalization](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Internationalization)
- [potools package](https://github.com/MichaelChirico/potools)
