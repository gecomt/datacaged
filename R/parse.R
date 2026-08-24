# parse.R
# Leitura e normalizacao dos microdados do CAGED (antigo e Novo CAGED)

# -- Colunas numericas conhecidas -----------------------------------------------

#' Colunas que devem ser numéricas no Novo CAGED
#' Baseado no layout real dos arquivos do MTE (2023+).
#' Inclui tanto nomes antigos ("competencia") quanto novos ("competenciamov").
#' @noRd
.cols_numericas_novo <- c(
  # Competencia - nome varia por periodo
  "competencia", "competenciamov", "competenciadec",
  # Localizacao
  "regiao", "uf", "municipio",
  # Estabelecimento
  "subclasse", "tipoempregador", "tipoestabelecimento",
  "tamestabjan", "indicadoraprendiz", "origemdainformacao",
  # Movimentacao
  "saldomovimentacao", "tipomovimentacao", "tipodedeficiencia",
  "indtrabintermitente", "indtrabparcial", "indicadordeforadoprazo",
  # Trabalhador
  "categoria", "graudeinstrucao", "idade", "horascontratuais",
  "racacor", "sexo", "cbo2002ocupacao", "unidadesalariocodigo",
  # Salario
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

# -- Leitura de arquivo individual ---------------------------------------------

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
#' @param path Path to the file .txt
#' @param type character or NULL. Se NULL, detecta pelo nome do arquivo.
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

  # Remove BOM UTF-8 (\uFEFF) e espacos do primeiro nome de coluna
  # O MTE frequentemente inclui BOM nos arquivos, o que gruda no header
  names(df)[1] <- gsub("^\uFEFF", "", names(df)[1])
  names(df)[1] <- trimws(names(df)[1])

  # Padroniza nomes (minusculo sem acentos)
  names(df) <- .normalize_names(names(df))

  # Remove colunas geradas por semicolons finais no cabecalho do MTE:
  # readr name_repair cria "...N" para campos vazios (ex: "col1;col2;").
  # .normalize_names() pode gerar "" para colunas com apenas chars especiais.
  # Ambos os casos causariam falha em DBI::dbAppendTable.
  df <- df[, nchar(names(df)) > 0L & !grepl("^\\.\\.\\.\\d+$", names(df)),
           drop = FALSE]

  # Coerce colunas numericas conhecidas
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
    # Remove diacríticos via escapes Unicode — evita dependência de locale
    stringr::str_replace_all(
      c("\u00e1|\u00e0|\u00e3|\u00e2|\u00e4" = "a",
        "\u00e9|\u00e8|\u00ea|\u00eb"         = "e",
        "\u00ed|\u00ec|\u00ee|\u00ef"         = "i",
        "\u00f3|\u00f2|\u00f5|\u00f4|\u00f6" = "o",
        "\u00fa|\u00f9|\u00fb|\u00fc"         = "u",
        "\u00e7"                               = "c")
    ) |>
    stringr::str_replace_all("[^a-z0-9_]", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_remove_all("^_+|_+$")
}

# -- Parse de arquivo .7z (extrai + le) ---------------------------------------

#' Lê microdados de um arquivo .7z do CAGED
#'
#' Lê os `.txt` internos diretamente via stream (sem extrair para disco),
#' usando `archive::archive_read()`. Se o stream falhar, faz fallback para
#' extração em diretório temporário. Devolve um único tibble normalizado.
#'
#' @param path Path to the file `.7z`.
#' @param type character or NULL. File type ("MOV", "FOR", "EXC",
#'   "ANTIGO"). Se NULL, detecta pelo nome.
#'
#' @return tibble com os microdados, ou NULL se o arquivo estiver vazio/inválido.
#'
#' @examples
#' # Toy example com arquivo incluído no pacote (executa sem internet)
#' path_mov <- system.file("extdata", "CAGEDMOV202301_exemplo.7z", package = "datacaged")
#' df <- caged_parse(path_mov, type = "MOV")
#' head(df)
#'
#' \donttest{
#' # Arquivo real baixado do HuggingFace
#' df_mov <- caged_parse("~/Downloads/CAGEDMOV202301.7z")
#' dplyr::glimpse(df_mov)
#'
#' # Tipo detectado automaticamente
#' df_for <- caged_parse("~/Downloads/CAGEDFOR202301.7z")
#' }
#' @seealso [caged_parse_batch()] para processar múltiplos arquivos de uma vez.
#' @seealso [caged_to_duckdb()] para gravar o resultado no banco.
#' @export
caged_parse <- function(path, type = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  type <- type %||% .detect_type(path)

  # Tenta leitura via stream (sem extrair para disco) usando archive_read().
  # Isso evita criar um diretório temporário e escrever+ler o .txt,
  # reduzindo o I/O à metade. Fallback para extração em disco se falhar.
  dfs <- tryCatch({
    .parse_via_stream(path, type)
  }, error = function(e) {
    cli::cli_warn(
      "Stream falhou em {.path {basename(path)}}: {conditionMessage(e)}. Tentando extra\u00e7\u00e3o..."
    )
    .parse_via_extract(path, type)
  })

  if (is.null(dfs) || length(dfs) == 0) return(invisible(NULL))

  dplyr::bind_rows(purrr::compact(dfs))
}

#' Lê .7z via stream direto (sem extração para disco)
#' @noRd
.parse_via_stream <- function(path, type) {
  # Lista entradas do arquivo comprimido
  entries <- archive::archive(path)
  txts    <- entries$path[grepl("\\.(txt|csv)$", entries$path, ignore.case = TRUE)]

  if (length(txts) == 0) {
    cli::cli_warn("No .txt found in {.path {basename(path)}}. Skipping.")
    return(NULL)
  }

  purrr::map(txts, function(entry) {
    con <- archive::archive_read(path, file = entry)
    on.exit(try(close(con), silent = TRUE))
    .read_text_file(con, type = type)
  })
}

#' Fallback: extrai .7z para disco e lê os .txt
#' @noRd
.parse_via_extract <- function(path, type) {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp)

  txts <- tryCatch(
    .extract_7z(path, exdir = tmp),
    error = function(e) {
      cli::cli_abort("Falha ao extrair {.path {path}}: {conditionMessage(e)}")
    }
  )

  if (length(txts) == 0) {
    cli::cli_warn("Nenhum .txt encontrado em {.path {path}}. Pulando.")
    return(NULL)
  }

  purrr::map(txts, \(f) .read_text_file(f, type = type))
}

# -- Funcao publica: parse em lote ---------------------------------------------

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
#' \donttest{
#' # Ler todos os arquivos MOV de um diretório de cache
#' cache <- tools::R_user_dir("datacaged", "cache")
#' arquivos_mov <- list.files(
#'   file.path(cache, "NOVO_CAGED", "2023"),
#'   pattern    = "CAGEDMOV",
#'   full.names = TRUE
#' )
#' df <- caged_parse_batch(arquivos_mov)
#'
#' # Gravar no banco após parsear
#' caged_to_duckdb(df, db_path = file.path(tempdir(), "caged.duckdb"))
#' }
#'
#' @seealso [caged_parse()] para processar um arquivo individual.
#' @seealso [caged_to_duckdb()] para gravar o resultado no banco.
#' @export
caged_parse_batch <- function(paths, .progress = TRUE) {
  paths <- paths[file.exists(paths) & file.size(paths) > 0]

  if (length(paths) == 0) {
    cli::cli_inform("Nenhum arquivo valido para parsear.")
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

  # Valida que todos os arquivos sao do mesmo type
  tipos <- unique(vapply(dfs, \(d) unique(d$fonte_tipo)[1L], character(1L)))
  if (length(tipos) > 1) {
    cli::cli_abort(c(
      "Os arquivos sao de tipos diferentes: {.val {tipos}}.",
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

