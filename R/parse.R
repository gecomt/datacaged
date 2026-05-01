# parse.R
# Leitura e normalização dos microdados do CAGED (antigo e Novo CAGED)

# ── Colunas numéricas conhecidas ───────────────────────────────────────────────

#' Colunas que devem ser numéricas no Novo CAGED
#' Baseado no layout real dos arquivos do MTE (2023+).
#' Inclui tanto nomes antigos ("competencia") quanto novos ("competenciamov").
#' @noRd
.cols_numericas_novo <- c(
  # Competência — nome varia por período
  "competencia", "competenciamov", "competenciadec",
  # Localização
  "regiao", "uf", "municipio",
  # Estabelecimento
  "subclasse", "tipoempregador", "tipoestabelecimento",
  "tamestabjan", "indicadoraprendiz", "origemdainformacao",
  # Movimentação
  "saldomovimentacao", "tipomovimentacao", "tipodedeficiencia",
  "indtrabintermitente", "indtrabparcial", "indicadordeforadoprazo",
  # Trabalhador
  "categoria", "graudeinstrucao", "idade", "horascontratuais",
  "racacor", "sexo", "cbo2002ocupacao", "unidadesalariocodigo",
  # Salário
  "salario", "valorsalariofixo",
  # Nomes antigos (compatibilidade)
  "naturezajuridica", "subatividade", "escolaridade",
  "numerocnpj", "codigomunicipio", "pais", "tipologradouro"
)

#' Colunas que devem ser numéricas no CAGED antigo
#' @noRd
.cols_numericas_antigo <- c(
  "competencia", "competencia_declarada", "ano_declarado",
  "regiao", "uf", "municipio", "ibge_subsetor",
  "subsetor", "subatividade",
  "saldo_mov", "saldomovimentacao",
  "ind_aprendiz", "indicadoraprendiz",
  "admitidos_desligados", "tipomovimentacao",
  "ind_portador_defic", "tipodedeficiencia",
  "salario_mensal", "salario",
  "grau_instrucao", "escolaridade",
  "idade", "qtd_hora_contrat", "horascontratuais",
  "raca_cor", "racacor", "sexo",
  "cbo_2002_ocupacao", "cnae_1_0_classe",
  "cnae_2_0_classe", "cnae_2_0_subclas",
  "faixa_empr_inicio_jan", "ind_trab_intermitente"
)

#' Coerce colunas para numérico onde o layout indica que devem ser numéricas.
#' Usa suppressWarnings para lidar com "{ñ class.}" e similares no MTE.
#' @noRd
.coerce_numeric_columns <- function(df, cols_num) {
  for (col in intersect(names(df), cols_num)) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- suppressWarnings(as.numeric(
        gsub(",", ".", as.character(df[[col]]))
      ))
    }
  }
  df
}

# ── Leitura de arquivo individual ─────────────────────────────────────────────

#' Detecta o tipo de arquivo a partir do nome
#' @return "MOV", "FOR", "EXC" ou "ANTIGO"
#' @noRd
.detect_type <- function(path) {
  nome <- toupper(basename(path))
  if      (grepl("CAGEDMOV",     nome)) "MOV"
  else if (grepl("CAGEDFOR",     nome)) "FOR"
  else if (grepl("CAGEDEXC",     nome)) "EXC"
  else if (grepl("CAGEDAJUSTES", nome)) "AJUSTES"
  else                                   "ANTIGO"
}

#' Lê um único arquivo .txt do CAGED (já extraído) e retorna tibble normalizado
#'
#' @param path Caminho para o arquivo .txt
#' @param type character ou NULL. Se NULL, detecta pelo nome do arquivo.
#' @return tibble com colunas padronizadas + coluna `fonte_tipo`
#' @noRd
.read_text_file <- function(path, type = NULL) {
  if (is.null(type)) type <- .detect_type(path)

  eh_novo   <- type %in% c("MOV", "FOR", "EXC")
  enc       <- if (eh_novo) "UTF-8" else "latin1"  # antigo e ajustes: latin1

  df <- tryCatch(
    readr::read_delim(
      path,
      delim            = ";",
      locale           = readr::locale(encoding = enc, decimal_mark = ","),
      col_types        = readr::cols(.default = readr::col_character()),
      show_col_types   = FALSE,
      progress         = FALSE,
      name_repair      = "universal_quiet",
      skip_empty_rows  = TRUE,
      trim_ws          = TRUE
    ),
    error = function(e) {
      cli::cli_warn("Erro ao ler {.path {path}}: {conditionMessage(e)}")
      return(NULL)
    }
  )

  if (is.null(df) || nrow(df) == 0) return(NULL)

  # Remove BOM UTF-8 (﻿) e espaços do primeiro nome de coluna
  # O MTE frequentemente inclui BOM nos arquivos, o que gruda no header
  names(df)[1] <- gsub("^\uFEFF", "", names(df)[1])
  names(df)[1] <- trimws(names(df)[1])

  # Padroniza nomes (minúsculo sem acentos)
  names(df) <- .normalize_names(names(df))

  # Coerce colunas numéricas conhecidas
  cols_num <- if (eh_novo) .cols_numericas_novo else .cols_numericas_antigo  # ajustes usa mesmo schema do antigo
  df <- .coerce_numeric_columns(df, cols_num)

  # Adiciona coluna de metadados
  df$fonte_tipo <- type

  df
}

#' Normaliza nomes de colunas: minúsculo, sem espaços, sem acentos
#' @noRd
.normalize_names <- function(nomes) {
  nomes |>
    tolower() |>
    stringr::str_replace_all("[áàãâä]", "a") |>
    stringr::str_replace_all("[éèêë]",  "e") |>
    stringr::str_replace_all("[íìîï]",  "i") |>
    stringr::str_replace_all("[óòõôö]", "o") |>
    stringr::str_replace_all("[úùûü]",  "u") |>
    stringr::str_replace_all("[ç]",     "c") |>
    stringr::str_replace_all("[^a-z0-9_]", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove("^_|_$")
}

# ── Parse de arquivo .7z (extrai + lê) ───────────────────────────────────────

#' Lê microdados de um arquivo .7z do CAGED (extrai em temp e lê)
#'
#' Extrai o `.7z` em um diretório temporário, lê todos os `.txt` encontrados
#' e devolve um único tibble com os dados normalizados.
#'
#' @param path Caminho para o arquivo `.7z`.
#' @param type character ou NULL. Tipo do arquivo ("MOV", "FOR", "EXC",
#'   "ANTIGO"). Se NULL, detecta pelo nome.
#'
#' @return tibble com os microdados, ou NULL se o arquivo estiver vazio/inválido.
#'
#' @examples
#' \dontrun{
#' # Ler arquivo MOV (movimentações)
#' df_mov <- caged_parse("~/Downloads/CAGEDMOV202301.7z")
#' dplyr::glimpse(df_mov)
#'
#' # Ler arquivo FOR (fora do prazo) — tipo detectado automaticamente
#' df_for <- caged_parse("~/Downloads/CAGEDFOR202301.7z")
#'
#' # Especificar o tipo manualmente
#' df <- caged_parse("~/Downloads/CAGEDMOV202301.7z", type = "MOV")
#'
#' # Ver colunas disponíveis
#' names(df_mov)
#' }
#'
#' @seealso [caged_parse_batch()] para processar múltiplos arquivos de uma vez.
#' @seealso [caged_to_duckdb()] para gravar o resultado no banco.
#' @export
caged_parse <- function(path, type = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("Arquivo não encontrado: {.path {path}}")
  }

  type <- type %||% .detect_type(path)

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)

  cli::cli_inform("Extraindo {.path {basename(path)}}...")

  # Extrai .7z
  txts <- tryCatch(
    .extract_7z(path, exdir = tmp),
    error = function(e) {
      cli::cli_abort("Falha ao extrair {.path {path}}: {conditionMessage(e)}")
    }
  )

  if (length(txts) == 0) {
    cli::cli_warn("Nenhum .txt encontrado em {.path {path}}. Pulando.")
    return(invisible(NULL))
  }

  # Lê e empilha (normalmente 1 txt por .7z, mas generaliza)
  dfs <- purrr::map(txts, \(f) .read_text_file(f, type = type))
  dfs <- purrr::compact(dfs)

  if (length(dfs) == 0) return(invisible(NULL))

  dplyr::bind_rows(dfs)
}

# ── Função pública: parse em lote ─────────────────────────────────────────────

#' Lê e normaliza múltiplos arquivos .7z do CAGED
#'
#' Wrapper sobre `caged_parse()` que processa um vetor de caminhos,
#' empilha os resultados e reporta progresso.
#'
#' Tipicamente você não chama esta função diretamente — ela é usada
#' internamente por `caged_load()`. Mas é útil quando você quer controle
#' manual sobre o que parsear.
#'
#' @param paths character vector. Caminhos dos arquivos `.7z`.
#' @param .progress logical. Exibe barra de progresso. Default: TRUE.
#'
#' @return tibble empilhado com todos os registros, ou NULL se nenhum
#'   arquivo for legível.
#'
#' @examples
#' \dontrun{
#' # Ler todos os arquivos MOV de um diretório de cache
#' cache <- tools::R_user_dir("datacaged", "cache")
#' arquivos_mov <- list.files(
#'   file.path(cache, "caged_novo", "2023"),
#'   pattern    = "CAGEDMOV",
#'   full.names = TRUE
#' )
#' df <- caged_parse_batch(arquivos_mov)
#'
#' # Gravar no banco após parsear
#' caged_to_duckdb(df, db_path = "caged.duckdb")
#' }
#'
#' @seealso [caged_parse()] para processar um arquivo individual.
#' @seealso [caged_to_duckdb()] para gravar o resultado no banco.
#' @export
caged_parse_batch <- function(paths, .progress = TRUE) {
  paths <- paths[file.exists(paths) & file.size(paths) > 0]

  if (length(paths) == 0) {
    cli::cli_inform("Nenhum arquivo válido para parsear.")
    return(invisible(NULL))
  }

  cli::cli_h2("Lendo {length(paths)} arquivo{?s}...")

  dfs <- purrr::map(
    paths,
    \(p) {
      tryCatch(
        caged_parse(p),
        error = function(e) {
          cli::cli_warn("Erro em {.path {basename(p)}}: {conditionMessage(e)}")
          NULL
        }
      )
    },
    .progress = .progress
  )

  dfs <- purrr::compact(dfs)

  if (length(dfs) == 0) {
    cli::cli_warn("Nenhum dado lido.")
    return(invisible(NULL))
  }

  # Valida que todos os arquivos são do mesmo type
  tipos <- unique(vapply(dfs, \(d) unique(d$fonte_tipo)[1L], character(1L)))
  if (length(tipos) > 1) {
    cli::cli_abort(c(
      "Os arquivos são de tipos diferentes: {.val {tipos}}.",
      "i" = "Use `caged_parse()` individualmente para cada type."
    ))
  }

  resultado <- dplyr::bind_rows(dfs)
  cli::cli_inform(c(
    "v" = "{format(nrow(resultado), big.mark = ',')} registros lidos",
    "i" = "{ncol(resultado)} colunas"
  ))

  resultado
}

