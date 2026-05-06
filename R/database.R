# database.R
# Grava\u00E7\u00E3o no DuckDB, conex\u00E3o e utilit\u00E1rios de consulta

# \u2500\u2500 Conex\u00E3o \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Abre uma conexão com o banco DuckDB do CAGED
#'
#' Cria o arquivo `.duckdb` se ainda não existir. A conexão retornada é
#' compatível com `dplyr::tbl()`, `DBI::dbGetQuery()` e `dbplyr`.
#'
#' Lembre-se de fechar a conexão com [DBI::dbDisconnect()] ao terminar.
#'
#' @param db_path character. Caminho para o arquivo `.duckdb`.
#'   Use `":memory:"` para um banco temporário em memória.
#' @param read_only logical. Se TRUE, abre somente leitura. Default: FALSE.
#' @param quiet logical. Se TRUE, suprime a mensagem de conexão. Default: FALSE.
#'
#' @return objeto de conexão DBI (`duckdb_connection`).
#'
#' @examples
#' \dontrun{
#' con <- caged_connect("caged.duckdb")
#'
#' # Listar tabelas disponíveis
#' DBI::dbListTables(con)
#'
#' # Consultar com dplyr
#' dplyr::tbl(con, "caged_mov") |>
#'   dplyr::group_by(competenciamov) |>
#'   dplyr::summarise(saldo = sum(saldomovimentacao, na.rm = TRUE))
#'
#' # Sempre fechar ao terminar
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @seealso [caged_info()] para listar tabelas no banco.
#' @export
caged_connect <- function(db_path, read_only = FALSE, quiet = FALSE) {
  drv <- duckdb::duckdb(dbdir = db_path, read_only = read_only)
  con <- DBI::dbConnect(drv)
  if (!quiet) cli::cli_inform("Conectado a {.path {db_path}}")
  con
}

# \u2500\u2500 Escrita no DuckDB \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Grava um data.frame de microdados do CAGED em uma tabela DuckDB
#'
#' Usa `INSERT OR IGNORE` por competência para evitar duplicatas:
#' se uma competência já existe na tabela, ela é pulada (útil para
#' re-rodar o pipeline sem apagar dados anteriores).
#'
#' As tabelas criadas são:
#' - `caged_mov`, `caged_for`, `caged_exc` — para dados de 2020 em diante
#' - `caged_antigo` — para dados até 2019 (CAGED antigo)
#' - `caged_ajustes` — ajustes retroativos do CAGED antigo
#'
#' @param df data.frame ou tibble. Dados retornados por `caged_parse()`.
#' @param db_path character. Caminho para o arquivo `.duckdb`.
#' @param table character ou NULL. Nome da tabela de destino. Se NULL,
#'   detecta automaticamente pela coluna `fonte_tipo` do data.frame.
#' @param overwrite_competencies logical. Se TRUE, apaga registros da
#'   competência antes de inserir (evita duplicatas em re-runs).
#'   Default: FALSE.
#'
#' @return invisível: número de linhas inseridas.
#'
#' @examples
#' \dontrun{
#' # Parse e grava em um único fluxo
#' df <- caged_parse("CAGEDMOV202301.7z")
#' caged_to_duckdb(df, db_path = "caged.duckdb")
#'
#' # Regravar uma competência já existente
#' caged_to_duckdb(df, db_path = "caged.duckdb",
#'                 overwrite_competencies = TRUE)
#' }
#'
#' @seealso [caged_parse()] para gerar o data.frame de entrada.
#' @seealso [caged_load()] para o pipeline completo.
#' @export
caged_to_duckdb <- function(df,
                            db_path,
                            table = NULL,
                            overwrite_competencies = FALSE) {
  if (is.null(df) || nrow(df) == 0) {
    cli::cli_inform("data.frame vazio \u2014 nada a gravar.")
    return(invisible(0L))
  }

  table <- table %||% .detect_table(df)

  con <- caged_connect(db_path, quiet = TRUE)
  on.exit(.disconnect(con), add = TRUE)

  # Normaliza coluna de compet\u00EAncia \u2014 o nome varia por per\u00EDodo e formato:
  # Novo CAGED: "competenciamov" ou "competenciadec"
  # CAGED antigo (CAGEDEST): "competencia_declarada" (formato AAAAMM)
  # Vers\u00F5es antigas: "competencia"
  if (!"competencia" %in% names(df)) {
    # Seleciona a coluna de compet\u00EAncia com prioridade expl\u00EDcita:
    # competenciamov \u00E9 a chave correta para deduplica\u00E7\u00E3o (per\u00EDodo de refer\u00EAncia),
    # mesmo em arquivos FOR/EXC que tamb\u00E9m t\u00EAm competenciadec.
    # intersect() preserva a ordem do df, n\u00E3o da lista \u2014 por isso iteramos
    # na ordem de prioridade para garantir comportamento determin\u00EDstico.
    priority <- c("competenciamov", "competenciadec", "competencia_declarada")
    col_comp  <- NA_character_
    for (nm in priority) {
      if (nm %in% names(df)) { col_comp <- nm; break }
    }
    if (!is.na(col_comp)) {
      df$competencia <- df[[col_comp]]
    } else {
      cli::cli_abort(c(
        "Nenhuma coluna de compet\u00EAncia encontrada no data.frame.",
        "i" = "Colunas presentes: {.val {names(df)}}",
        "i" = "Esperado: {.col competencia}, {.col competenciamov},
               {.col competenciadec} ou {.col competencia_declarada}."
      ))
    }
  }
  df$competencia <- suppressWarnings(as.numeric(df$competencia))

  # Remove linhas com competencia inv\u00E1lida: NA, NaN e Inf
  # is.finite() retorna FALSE para NA, NaN e Inf \u2014 mais seguro que !is.na()
  n_antes <- nrow(df)
  df <- df[is.finite(df$competencia), ]
  if (nrow(df) < n_antes) {
    cli::cli_warn("{n_antes - nrow(df)} linha{?s} removida{?s} com competencia inv\u00E1lida (NA/Inf).")
  }

  if (nrow(df) == 0) {
    cli::cli_inform("data.frame vazio ap\u00F3s limpeza \u2014 nada a gravar.")
    return(invisible(0L))
  }

  # Cria tabela se n\u00E3o existir
  .create_table_if_needed(con, table, df)

  # Compet\u00EAncias presentes no df
  competencias <- unique(df$competencia)

  if (overwrite_competencies) {
    .delete_competencies(con, table, competencias)
    n_inserido <- .insert_rows(con, table, df)
  } else {
    # Filtra compet\u00EAncias j\u00E1 gravadas
    ja_gravadas <- .existing_competencies(con, table, competencias)
    novas       <- setdiff(competencias, ja_gravadas)

    if (length(ja_gravadas) > 0) {
      cli::cli_inform(c(
        "i" = "{length(ja_gravadas)} compet\u00EAncia{?s} j\u00E1 no banco \u2014 pulando.",
        "i" = "Use {.code overwrite_competencies = TRUE} para for\u00E7ar."
      ))
    }

    if (length(novas) == 0) {
      cli::cli_inform("Nenhum dado novo para inserir.")
      return(invisible(0L))
    }

    df_novo    <- dplyr::filter(df, competencia %in% novas)
    n_inserido <- .insert_rows(con, table, df_novo)
  }

  cli::cli_inform(c(
    "v" = "{format(n_inserido, big.mark = ',')} registro{?s} inserido{?s} em {.val {table}}"
  ))

  invisible(n_inserido)
}

# \u2500\u2500 Informa\u00E7\u00F5es do banco \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Lista tabelas e estatísticas do banco DuckDB do CAGED
#'
#' Exibe no console uma tabela com nome, número de registros,
#' competências disponíveis e tamanho estimado de cada tabela CAGED.
#'
#' @param db_path character. Caminho para o arquivo `.duckdb`.
#'
#' @return tibble invisível com as estatísticas.
#'
#' @examples
#' \dontrun{
#' caged_info("caged.duckdb")
#' }
#'
#' @seealso [caged_connect()] para obter uma conexão DBI com o banco.
#' @export
caged_info <- function(db_path) {
  if (db_path != ":memory:" && !file.exists(db_path)) {
    cli::cli_abort("Banco n\u00E3o encontrado: {.path {db_path}}")
  }

  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(.disconnect(con), add = TRUE)

  tabelas_caged <- intersect(
    DBI::dbListTables(con),
    c("caged_mov", "caged_for", "caged_exc", "caged_antigo", "caged_ajustes")
  )

  if (length(tabelas_caged) == 0) {
    cli::cli_inform("Nenhuma tabela CAGED encontrada em {.path {db_path}}.")
    return(invisible(tibble::tibble()))
  }

  stats <- purrr::map(tabelas_caged, function(tab) {
    n <- DBI::dbGetQuery(
      con, glue::glue("SELECT COUNT(*) AS n FROM {tab}")
    )$n

    comp_range <- tryCatch({
      r <- DBI::dbGetQuery(
        con,
        glue::glue(
          "SELECT MIN(competencia) AS min_c,
                   MAX(competencia) AS max_c FROM {tab}"
        )
      )
      if (is.na(r$min_c)) "(vazia)" else glue::glue("{r$min_c} \u2013 {r$max_c}")
    }, error = function(e) NA_character_)

    tibble::tibble(
      table       = tab,
      registros    = n,
      competencias = comp_range
    )
  }) |> dplyr::bind_rows()

  # Tamanho do arquivo
  tamanho_mb <- if (db_path == ":memory:") {
    NA_real_
  } else {
    round(file.size(db_path) / 1024^2, 1)
  }
  tamanho_str <- if (is.na(tamanho_mb)) "(em mem\u00F3ria)" else paste0(tamanho_mb, " MB")

  cli::cli_h1("datacaged \u2014 {.path {db_path}}")
  cli::cli_inform("Tamanho do arquivo: {tamanho_str}")
  cli::cli_h2("Tabelas")

  for (i in seq_len(nrow(stats))) {
    s <- stats[i, ]
    cli::cli_inform(c(
      "*" = "{.val {s$table}}",
      " " = "Registros   : {format(s$registros, big.mark = ',')}",
      " " = "Compet\u00EAncias: {s$competencias}"
    ))
  }

  invisible(stats)
}

# \u2500\u2500 Pipeline principal: caged_load \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Pipeline completo: download → parse → DuckDB
#'
#' Combina `caged_download()`, `caged_parse()` e `caged_to_duckdb()`
#' em um único comando. É o jeito mais simples de popular o banco local.
#'
#' @param years integer vector. Anos desejados.
#' @param months integer vector. Meses desejados (1–12). Default: `seq_len(12L)` (todos os meses).
#' @param states character vector ou NULL. **Ignorado** — o parâmetro é validado
#'   mas não filtra downloads de nenhuma série (Novo CAGED, antigo ou Ajustes),
#'   pois todos os arquivos são de âmbito nacional. Mantido por compatibilidade.
#' @param db_path character. Caminho para o arquivo `.duckdb`.
#'   Default: `"caged.duckdb"` no diretório de trabalho atual.
#' @param destdir character ou NULL. Diretório de cache dos `.7z`.
#' @param force_download logical. Re-baixa arquivos já em cache.
#'   Default: FALSE.
#' @param overwrite_competencies logical. Regrava competências já no banco.
#'   Default: FALSE.
#' @param timeout integer. Timeout por arquivo em segundos. Default: 300.
#'
#' @return invisível: tibble com estatísticas finais do banco (via `caged_info()`).
#'
#' @examples
#' \dontrun{
#' # Novo CAGED 2022-2023
#' caged_load(
#'   years   = 2022:2023,
#'   db_path = "caged.duckdb"
#' )
#'
#' # CAGED antigo nacional (2015-2019)
#' caged_load(
#'   years   = 2015:2019,
#'   db_path = "caged_historico.duckdb"
#' )
#'
#' # Conecta e consulta depois
#' con <- caged_connect("caged.duckdb")
#' dplyr::tbl(con, "caged_mov") |>
#'   dplyr::group_by(competencia, uf) |>
#'   dplyr::summarise(saldo = sum(saldomovimentacao, na.rm = TRUE))
#' }
#'
#' @seealso [caged_adjustments_load()] para o CAGED Ajustes (correções retroativas até 2019).
#' @seealso [caged_download()] para baixar sem gravar no banco.
#' @seealso [caged_info()] para inspecionar o banco após a carga.
#' @export
caged_load <- function(years,
                       months                    = seq_len(12L),
                       states                    = NULL,
                       db_path                   = "caged.duckdb",
                       destdir                   = NULL,
                       force_download            = FALSE,
                       overwrite_competencies    = FALSE,
                       timeout                   = 300) {

  # Valida inputs antes de qualquer output \u2014 evita "Anos: NA\u2013NA" no console
  years  <- .validate_years(years)
  months <- .validate_months(months)

  cli::cli_h1("caged_load")
  cli::cli_inform(c(
    "i" = "Anos  : {years[1]}\u2013{years[length(years)]}",
    "i" = "Meses : {months[1]}\u2013{months[length(months)]}",
    "i" = "Banco : {.path {db_path}}"
  ))

  # 1. Download
  cli::cli_h2("1/3  Download")
  manifest <- caged_download(
    years   = years,
    months  = months,
    states  = states,
    destdir = destdir,
    force   = force_download,
    timeout = timeout
  )

  # Arquivos dispon\u00EDveis (baixados ou em cache)
  arquivos_ok <- manifest |>
    dplyr::filter(status %in% c("baixado", "cache")) |>
    dplyr::pull(arquivo)

  if (length(arquivos_ok) == 0) {
    cli::cli_warn("Nenhum arquivo dispon\u00EDvel para processar.")
    return(invisible(NULL))
  }

  # 2. Parse \u2014 processa cada arquivo individualmente (um tipo por vez)
  # MOV, FOR e EXC v\u00E3o para tabelas separadas: caged_mov, caged_for, caged_exc
  cli::cli_h2("2/3  Parse e carga no DuckDB")

  manifest_ok <- dplyr::filter(manifest, status %in% c("baixado", "cache"))
  n_inserido_total <- 0L

  cli::cli_progress_bar(
    name   = "Arquivos",
    total  = nrow(manifest_ok),
    format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} [{cli::pb_elapsed}]"
  )

  for (i in seq_len(nrow(manifest_ok))) {
    arq  <- manifest_ok$arquivo[i]
    type <- manifest_ok$type[i]

    df <- tryCatch(
      caged_parse(arq, type = type),
      error = function(e) {
        cli::cli_warn("Erro em {.path {basename(arq)}}: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(df) && nrow(df) > 0) {
      n <- caged_to_duckdb(
        df,
        db_path                   = db_path,
        overwrite_competencies    = overwrite_competencies
      )
      n_inserido_total <- n_inserido_total + n
    }

    rm(df)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # 3. Resumo final
  cli::cli_h2("3/3  Conclu\u00EDdo")
  cli::cli_inform(c(
    "v" = "Total inserido: {format(n_inserido_total, big.mark = ',')} registros"
  ))

  info <- caged_info(db_path)
  invisible(info)
}

# \u2500\u2500 Helpers internos \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Detecta o nome da tabela DuckDB pela coluna fonte_tipo do df
#' Mapeamento:
#'   MOV     -> caged_mov     (movimentações declaradas no prazo)
#'   FOR     -> caged_for     (fora do prazo)
#'   EXC     -> caged_exc     (exclusões / retificações)
#'   ANTIGO  -> caged_antigo  (CAGED antigo até 2019)
#'   AJUSTES -> caged_ajustes (ajustes do CAGED antigo)
#' @noRd
.detect_table <- function(df) {
  if (!"fonte_tipo" %in% names(df)) {
    cli::cli_abort(
      c("Coluna {.col fonte_tipo} n\u00E3o encontrada.",
        "i" = "Informe {.arg table} manualmente.")
    )
  }
  tipos <- unique(df$fonte_tipo)
  if (length(tipos) > 1) {
    cli::cli_abort(
      c("O data.frame mistura tipos de CAGED: {.val {tipos}}.",
        "i" = "Processe e grave separadamente cada tipo.")
    )
  }
  switch(tipos,
    MOV     = "caged_mov",
    FOR     = "caged_for",
    EXC     = "caged_exc",
    ANTIGO  = "caged_antigo",
    AJUSTES = "caged_ajustes",
    cli::cli_abort("Tipo desconhecido: {.val {tipos}}")
  )
}

#' Cria tabela DuckDB a partir do schema do df, se não existir
#' @noRd
.create_table_if_needed <- function(con, table, df) {
  if (!DBI::dbExistsTable(con, table)) {
    DBI::dbWriteTable(con, table, df[0, ], overwrite = FALSE)
    cli::cli_inform("Tabela {.val {table}} criada.")
  }
}

#' Retorna competências já gravadas no banco
#' @noRd
.existing_competencies <- function(con, table, competencies) {
  if (!DBI::dbExistsTable(con, table)) return(numeric(0))

  # Verifica se a coluna competencia existe na tabela
  colunas <- DBI::dbListFields(con, table)
  if (!"competencia" %in% colunas) return(numeric(0))

  # Guarda: IN () \u00E9 SQL inv\u00E1lido \u2014 retorna vazio se n\u00E3o h\u00E1 compet\u00EAncias
  if (length(competencies) == 0L) return(numeric(0))

  # Query parametrizada com um placeholder por valor para evitar SQL bruto
  placeholders <- paste(rep("?", length(competencies)), collapse = ", ")
  sql <- sprintf(
    "SELECT DISTINCT competencia FROM %s WHERE competencia IN (%s)",
    table, placeholders
  )
  tryCatch({
    rs  <- DBI::dbSendQuery(con, sql)
    DBI::dbBind(rs, as.list(as.numeric(competencies)))
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    res$competencia
  }, error = function(e) numeric(0))
}

#' Deleta registros de competências específicas
#' @noRd
.delete_competencies <- function(con, table, competencies) {
  if (length(competencies) == 0L) return(invisible(0L))

  # Query parametrizada \u2014 evita IN () inv\u00E1lido e constru\u00E7\u00E3o de SQL bruto
  placeholders <- paste(rep("?", length(competencies)), collapse = ", ")
  sql <- sprintf(
    "DELETE FROM %s WHERE competencia IN (%s)",
    table, placeholders
  )
  rs <- DBI::dbSendStatement(con, sql)
  DBI::dbBind(rs, as.list(as.numeric(competencies)))
  DBI::dbClearResult(rs)
  invisible(NULL)
}

#' Insere df na tabela via DBI::dbAppendTable
#' @return número de linhas inseridas
#' @noRd
.insert_rows <- function(con, table, df) {
  DBI::dbAppendTable(con, table, df)
  nrow(df)
}

#' Fecha conexão silenciosamente
#' @noRd
.disconnect <- function(con) {
  tryCatch(
    DBI::dbDisconnect(con, shutdown = TRUE),
    error = function(e) invisible(NULL)
  )
}

