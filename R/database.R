# database.R
# Gravacao no DuckDB, conexao e utilitarios de consulta

# -- Conexao -------------------------------------------------------------------

#' Abre uma conexão com o banco DuckDB do CAGED
#'
#' Cria o arquivo `.duckdb` se ainda não existir. A conexão retornada é
#' compatível com `dplyr::tbl()`, `DBI::dbGetQuery()` e `dbplyr`.
#'
#' Lembre-se de fechar a conexão com [DBI::dbDisconnect()] ao terminar.
#'
#' @param db_path character. Path to the file `.duckdb`.
#'   Use `":memory:"` para um banco temporário em memória.
#' @param read_only logical. If TRUE, abre somente leitura. Default: FALSE.
#' @param quiet logical. If TRUE, suppresses the connection message. Default: FALSE.
#'
#' @return objeto de conexão DBI (`duckdb_connection`).
#'
#' @examples
#' \donttest{
#' con <- caged_connect(file.path(tempdir(), "caged.duckdb"))
#'
#' # Listar tabelas disponíveis
#' DBI::dbListTables(con)
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
  if (!quiet) cli::cli_inform("Connected to {.path {db_path}}")
  con
}

# -- Escrita no DuckDB ---------------------------------------------------------

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
#' @param df data.frame or tibble. Data returned by `caged_parse()`.
#' @param db_path character or NULL. Path to the file `.duckdb`.
#'   Pode ser NULL se `.con` for fornecido.
#' @param table character or NULL. Name of the destination table. If NULL,
#'   detecta automaticamente pela coluna `fonte_tipo` do data.frame.
#' @param overwrite_competencies logical. If TRUE, deletes records for the
#'   competency before inserting (avoids duplicates on re-runs).
#'   Default: FALSE.
#' @param .con a DBI connection object (optional). If provided, `db_path` is
#'   ignored and the existing connection is reused. The caller is responsible
#'   for closing the connection.
#'
#' @return invisible: number of rows inserted.
#'
#' @examples
#' \dontrun{
#' # Parse e grava em um único fluxo (requer arquivo baixado)
#' df <- caged_parse("CAGEDMOV202301.7z")
#' caged_to_duckdb(df, db_path = file.path(tempdir(), "caged.duckdb"))
#'
#' # Regravar uma competência já existente
#' caged_to_duckdb(df, db_path = file.path(tempdir(), "caged.duckdb"),
#'                 overwrite_competencies = TRUE)
#' }
#'
#' @seealso [caged_parse()] para gerar o data.frame de entrada.
#' @seealso [caged_load()] para o pipeline completo.
#' @export
caged_to_duckdb <- function(df,
                            db_path  = NULL,
                            table    = NULL,
                            overwrite_competencies = FALSE,
                            .con     = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    cli::cli_inform("Empty data.frame - nothing to write.")
    return(invisible(0L))
  }

  table <- table %||% .detect_table(df)

  # Aceita conexão existente (.con=) ou abre uma nova (db_path=)
  if (!is.null(.con)) {
    con         <- .con
    fechar_con  <- FALSE
  } else {
    if (is.null(db_path)) cli::cli_abort("{.arg db_path} or {.arg .con} must be provided.")
    con         <- caged_connect(db_path, quiet = TRUE)
    fechar_con  <- TRUE
    on.exit(if (fechar_con) .disconnect(con), add = TRUE)
  }

  # Normaliza coluna de competencia - o nome varia por periodo e formato:
  # Novo CAGED: "competenciamov" ou "competenciadec"
  # CAGED antigo (CAGEDEST): "competencia_declarada" (formato AAAAMM)
  # Versoes antigas: "competencia"
  if (!"competencia" %in% names(df)) {
    # Seleciona a coluna de competencia com prioridade explicita:
    # competenciamov e a chave correta para deduplicacao (periodo de referencia),
    # mesmo em arquivos FOR/EXC que tambem tem competenciadec.
    # intersect() preserva a ordem do df, nao da lista - por isso iteramos
    # na ordem de prioridade para garantir comportamento deterministico.
    priority <- c("competenciamov", "competenciadec", "competencia_declarada")
    col_comp  <- NA_character_
    for (nm in priority) {
      if (nm %in% names(df)) { col_comp <- nm; break }
    }
    if (!is.na(col_comp)) {
      df$competencia <- df[[col_comp]]
    } else {
      cli::cli_abort(c(
        "Nenhuma coluna de competencia encontrada no data.frame.",
        "i" = "Colunas presentes: {.val {names(df)}}",
        "i" = "Esperado: {.col competencia}, {.col competenciamov},
               {.col competenciadec} ou {.col competencia_declarada}."
      ))
    }
  }
  df$competencia <- suppressWarnings(as.numeric(df$competencia))

  # Remove linhas com competencia invalida: NA, NaN e Inf
  # is.finite() retorna FALSE para NA, NaN e Inf - mais seguro que !is.na()
  n_antes <- nrow(df)
  df <- df[is.finite(df$competencia), ]
  if (nrow(df) < n_antes) {
    cli::cli_warn("{n_antes - nrow(df)} row{?s} with invalid competency removed (NA/Inf).")
  }

  if (nrow(df) == 0) {
    cli::cli_inform("Empty data.frame after cleaning - nothing to write.")
    return(invisible(0L))
  }

  # Cria tabela se nao existir
  .create_table_if_needed(con, table, df)

  # Competencias presentes no df
  competencias <- unique(df$competencia)

  if (overwrite_competencies) {
    .delete_competencies(con, table, competencias)
    n_inserido <- .insert_rows(con, table, df)
  } else {
    # Filtra competencias ja gravadas
    ja_gravadas <- .existing_competencies(con, table, competencias)
    novas       <- setdiff(competencias, ja_gravadas)

    if (length(ja_gravadas) > 0) {
      cli::cli_inform(c(
        "i" = "{length(ja_gravadas)} competencia{?s} ja no banco - pulando.",
        "i" = "Use {.code overwrite_competencies = TRUE} para forcar."
      ))
    }

    if (length(novas) == 0) {
      cli::cli_inform("No new data to insert.")
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

# -- Informacoes do banco ------------------------------------------------------

#' Lista tabelas e estatísticas do banco DuckDB do CAGED
#'
#' Exibe no console uma tabela com nome, número de registros,
#' competências disponíveis e tamanho estimado de cada tabela CAGED.
#'
#' @param db_path character. Path to the file `.duckdb`.
#'
#' @return tibble invisível com as estatísticas.
#'
#' @examples
#' \donttest{
#' caged_info(file.path(tempdir(), "caged.duckdb"))
#' }
#'
#' @seealso [caged_connect()] para obter uma conexão DBI com o banco.
#' @export
caged_info <- function(db_path) {
  if (db_path != ":memory:" && !file.exists(db_path)) {
    cli::cli_abort("Database not found: {.path {db_path}}")
  }

  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(.disconnect(con), add = TRUE)

  tabelas_caged <- intersect(
    DBI::dbListTables(con),
    c("caged_mov", "caged_for", "caged_exc", "caged_antigo", "caged_ajustes")
  )

  if (length(tabelas_caged) == 0) {
    cli::cli_inform("No CAGED table found in {.path {db_path}}.")
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
      if (is.na(r$min_c)) "(vazia)" else glue::glue("{r$min_c} - {r$max_c}")
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
  tamanho_str <- if (is.na(tamanho_mb)) "(em memoria)" else paste0(tamanho_mb, " MB")

  cli::cli_h1("datacaged - {.path {db_path}}")
  cli::cli_inform("File size: {tamanho_str}")
  cli::cli_h2("Tables")

  for (i in seq_len(nrow(stats))) {
    s <- stats[i, ]
    cli::cli_inform(c(
      "*" = "{.val {s$table}}",
      " " = "Records     : {format(s$registros, big.mark = ',')}",
      " " = "Periods     : {s$competencias}"
    ))
  }

  invisible(stats)
}

# -- Pipeline principal: caged_load --------------------------------------------

#' Pipeline completo: download → parse → DuckDB
#'
#' Combina `caged_download()`, `caged_parse()` e `caged_to_duckdb()`
#' em um único comando. É o jeito mais simples de popular o banco local.
#'
#' @param years integer vector. Desired years.
#' @param months integer vector. Desired months (1–12). Default: `seq_len(12L)` (todos os meses).
#' @param states character vector or NULL. **Ignored** — the parameter is validated
#'   but does not filter downloads for any series (Novo CAGED, antigo or Ajustes),
#'   as all files are national in scope. Kept for backwards compatibility.
#' @param db_path character. Path to the file `.duckdb`.
#'   Default: `file.path(tempdir(), "caged.duckdb")`.
#' @param destdir character or NULL. Cache directory for `.7z` files.
#' @param force_download logical. Re-downloads files already in cache.
#'   Default: FALSE.
#' @param overwrite_competencies logical. Overwrites competencies already in the database.
#'   Default: FALSE.
#' @param timeout integer. Timeout per file in seconds. Default: 300.
#' @param workers integer. Number of parallel downloads. Default: 3.
#'   Use `1` for sequential mode. Controlled via
#'   `options(datacaged.workers = N)`.
#'
#' @return invisible: tibble with final database statistics (via `caged_info()`).
#'
#' @examples
#' \dontrun{
#' # Download real — exemplos nao executados automaticamente (requerem rede e tempo)
#'
#' # Novo CAGED: 1 mes recente
#' caged_load(
#'   years   = 2024,
#'   months  = 1,
#'   db_path = file.path(tempdir(), "caged.duckdb")
#' )
#'
#' # CAGED antigo: 1 ano
#' caged_load(
#'   years   = 2019,
#'   months  = seq_len(12L),
#'   db_path = file.path(tempdir(), "caged_historico.duckdb")
#' )
#'
#' # Conecta e consulta
#' con <- caged_connect(file.path(tempdir(), "caged.duckdb"))
#' if ("caged_mov" %in% DBI::dbListTables(con)) {
#'   dplyr::tbl(con, "caged_mov") |>
#'     dplyr::group_by(competencia, uf) |>
#'     dplyr::summarise(saldo = sum(saldomovimentacao, na.rm = TRUE)) |>
#'     dplyr::collect()
#' }
#' DBI::dbDisconnect(con, shutdown = TRUE)
#' }
#'
#' @seealso [caged_adjustments_load()] para o CAGED Ajustes (correções retroativas até 2019).
#' @seealso [caged_download()] para baixar sem gravar no banco.
#' @seealso [caged_info()] para inspecionar o banco após a carga.
#' @export
caged_load <- function(years,
                       months                    = seq_len(12L),
                       states                    = NULL,
                       db_path = file.path(tempdir(), "caged.duckdb"),
                       destdir                   = NULL,
                       force_download            = FALSE,
                       overwrite_competencies    = FALSE,
                       timeout                   = 300,
                       workers = getOption("datacaged.workers",
                                           min(3L, future::availableCores()))) {

  # Valida inputs antes de qualquer output - evita "Anos: NA-NA" no console
  years  <- .validate_years(years)
  months <- .validate_months(months)

  cli::cli_h1("caged_load")
  cli::cli_inform(c(
    "i" = "Years : {years[1]}-{years[length(years)]}",
    "i" = "Months: {months[1]}-{months[length(months)]}",
    "i" = "DB    : {.path {db_path}}"
  ))

  # 1. Download
  cli::cli_h2("1/3  Download")
  manifest <- caged_download(
    years   = years,
    months  = months,
    states  = states,
    destdir = destdir,
    force   = force_download,
    timeout = timeout,
    workers = workers
  )

  # Arquivos disponiveis (baixados ou em cache)
  arquivos_ok <- manifest |>
    dplyr::filter(status %in% c("baixado", "cache")) |>
    dplyr::pull(arquivo)

  if (length(arquivos_ok) == 0) {
    cli::cli_warn("No files available to process.")
    return(invisible(NULL))
  }

  # 2. Parse - processa cada arquivo individualmente (um tipo por vez)
  # MOV, FOR e EXC vao para tabelas separadas: caged_mov, caged_for, caged_exc
  cli::cli_h2("2/3  Parse and load into DuckDB")

  manifest_ok <- dplyr::filter(manifest, status %in% c("baixado", "cache"))
  n_inserido_total <- 0L

  # Abre UMA conexão para todo o loop — evita open/close por arquivo
  con_load <- caged_connect(db_path, quiet = TRUE)
  on.exit(.disconnect(con_load), add = TRUE)

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
        cli::cli_warn("Error in {.path {basename(arq)}}: {conditionMessage(e)}")
        NULL
      }
    )

    if (!is.null(df) && nrow(df) > 0) {
      n <- caged_to_duckdb(
        df,
        .con                      = con_load,
        overwrite_competencies    = overwrite_competencies
      )
      n_inserido_total <- n_inserido_total + n
    }

    rm(df)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # 3. Resumo final
  cli::cli_h2("3/3  Done")
  cli::cli_inform(c(
    "v" = "Total inserted: {format(n_inserido_total, big.mark = ',')} records"
  ))

  info <- caged_info(db_path)
  invisible(info)
}

# -- Helpers internos ----------------------------------------------------------

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
      c("Coluna {.col fonte_tipo} nao encontrada.",
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

  # Guarda: IN () e SQL invalido - retorna vazio se nao ha competencias
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

  # Query parametrizada - evita IN () invalido e construcao de SQL bruto
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
  # Adicionar colunas ausentes na tabela antes de inserir (schema evolution)
  existing_cols <- DBI::dbListFields(con, table)
  new_cols <- setdiff(names(df), existing_cols)

  if (length(new_cols) > 0) {
    for (col in new_cols) {
      # Detectar tipo da coluna
      col_type <- if (is.numeric(df[[col]])) "DOUBLE" else "VARCHAR"
      sql <- sprintf(
        "ALTER TABLE %s ADD COLUMN %s %s",
        DBI::dbQuoteIdentifier(con, table),
        DBI::dbQuoteIdentifier(con, col),
        col_type
      )
      DBI::dbExecute(con, sql)
    }
  }

  # Reordenar colunas do df para coincidir com a tabela
  all_cols <- DBI::dbListFields(con, table)
  df_aligned <- df[, intersect(all_cols, names(df)), drop = FALSE]
  # Colunas na tabela mas nao no df — preencher com NA
  missing <- setdiff(all_cols, names(df))
  for (col in missing) {
    df_aligned[[col]] <- NA
  }
  df_aligned <- df_aligned[, all_cols, drop = FALSE]

  DBI::dbAppendTable(con, table, df_aligned)
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


# -- Atualização incremental ---------------------------------------------------

#' Atualiza o banco com as competências mais recentes disponíveis
#'
#' Consulta o HuggingFace para descobrir as competências disponíveis,
#' compara com o que já existe no banco e baixa apenas o que está faltando.
#' É o modo mais prático de manter o banco atualizado sem re-baixar tudo.
#'
#' @param db_path character. Path to the file `.duckdb`.
#' @param series character vector. Series to update: `"novo"`, `"antigo"`,
#'   `"ajustes"` ou qualquer combinação. Default: `"novo"`.
#' @param destdir character or NULL. Cache directory for `.7z` files.
#' @param workers integer. Number of parallel downloads. Default: 3.
#' @param timeout integer. Timeout por arquivo em segundos. Default: 300.
#' @param verbose logical. Displays details in the console. Default: TRUE.
#'
#' @return invisível: tibble com estatísticas do banco após a atualização,
#'   ou NULL se o banco já estiver atualizado.
#'
#' @examples
#' \dontrun{
#' # Atualizar Novo CAGED com as competências mais recentes
#' caged_update(db_path = file.path(tempdir(), "caged.duckdb"))
#'
#' # Atualizar todas as séries
#' caged_update(
#'   db_path = file.path(tempdir(), "caged.duckdb"),
#'   series  = c("novo", "antigo", "ajustes")
#' )
#' }
#'
#' @seealso [caged_load()] para carga inicial completa.
#' @seealso [caged_info()] para inspecionar o banco.
#' @seealso [caged_hf_files()] para listar competências disponíveis.
#' @export
caged_update <- function(db_path,
                         series  = "novo",
                         destdir = NULL,
                         workers = getOption("datacaged.workers",
                                             min(3L, future::availableCores())),
                         timeout = 300,
                         verbose = TRUE) {

  series <- match.arg(series, c("novo", "antigo", "ajustes"), several.ok = TRUE)

  if (verbose) cli::cli_h1("caged_update")

  # Mapeia série -> tabelas DuckDB relevantes
  tabelas_por_serie <- list(
    novo    = c("caged_mov", "caged_for", "caged_exc"),
    antigo  = "caged_antigo",
    ajustes = "caged_ajustes"
  )

  # Descobre max(competencia) de cada tabela no banco
  max_no_banco <- .max_competencias(db_path)

  resultados <- list()

  for (serie in series) {
    if (verbose) cli::cli_h2("Series: {serie}")

    # Competências disponíveis no HuggingFace
    disponiveis <- tryCatch(
      caged_hf_files(type = serie, n = Inf, verbose = FALSE, timeout = timeout),
      error = function(e) {
        cli::cli_warn("Failed to query HuggingFace for '{serie}': {conditionMessage(e)}")
        NULL
      }
    )

    if (is.null(disponiveis) || nrow(disponiveis) == 0) {
      if (verbose) cli::cli_inform("  Nenhuma competencia encontrada no HuggingFace.")
      next
    }

    # Determina a competência máxima já no banco para esta série
    tabs <- tabelas_por_serie[[serie]]
    tabs_presentes <- tabs[tabs %in% names(max_no_banco)]
    max_banco <- if (length(tabs_presentes) == 0) {
      0L
    } else {
      max(unlist(max_no_banco[tabs_presentes]), na.rm = TRUE)
    }
    if (is.infinite(max_banco) || is.na(max_banco)) max_banco <- 0L

    # Filtra apenas competências novas (> max no banco)
    novas <- disponiveis[as.integer(disponiveis$competencia) > max_banco, ]

    if (nrow(novas) == 0) {
      if (verbose) {
        cli::cli_inform(c(
          "v" = "Database already up to date.",
          "i" = "Max in DB: {max_banco} | Available: {max(as.integer(disponiveis$competencia))}"
        ))
      }
      next
    }

    if (verbose) {
      cli::cli_inform(c(
        "i" = "{nrow(novas)} new competenc{?y/ies} found",
        "i" = "Period: {min(novas$competencia)} - {max(novas$competencia)}"
      ))
    }

    years  <- sort(unique(novas$ano))
    months <- sort(unique(novas$mes))

    # Chama o pipeline adequado para cada série
    res <- tryCatch({
      if (serie == "ajustes") {
        caged_adjustments_load(
          years   = years,
          months  = months,
          db_path = db_path,
          destdir = destdir,
          timeout = timeout
        )
      } else {
        caged_load(
          years   = years,
          months  = months,
          db_path = db_path,
          destdir = destdir,
          timeout = timeout,
          workers = workers
        )
      }
    }, error = function(e) {
      cli::cli_warn("Error updating '{serie}': {conditionMessage(e)}")
      NULL
    })

    resultados[[serie]] <- res
  }

  if (length(resultados) == 0) {
    if (verbose) cli::cli_inform("Database already up to date for all series.")
    return(invisible(NULL))
  }

  invisible(caged_info(db_path))
}

#' Retorna a competência máxima por tabela no banco
#' @noRd
.max_competencias <- function(db_path) {
  if (!file.exists(db_path)) return(list())

  con <- tryCatch(
    caged_connect(db_path, read_only = TRUE, quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(con)) return(list())
  on.exit(.disconnect(con), add = TRUE)

  tabelas <- intersect(
    DBI::dbListTables(con),
    c("caged_mov", "caged_for", "caged_exc", "caged_antigo", "caged_ajustes")
  )

  if (length(tabelas) == 0) return(list())

  result <- list()
  for (tab in tabelas) {
    colunas <- DBI::dbListFields(con, tab)
    col_comp <- if ("competencia" %in% colunas) "competencia" else NA_character_
    if (is.na(col_comp)) next

    max_val <- tryCatch(
      DBI::dbGetQuery(con, glue::glue("SELECT MAX(competencia) AS m FROM {tab}"))$m,
      error = function(e) NA_real_
    )
    result[[tab]] <- if (is.na(max_val)) 0L else as.integer(max_val)
  }

  result
}

# -- Exportação para Parquet ---------------------------------------------------

#' Exporta tabelas do banco DuckDB para arquivos Parquet
#'
#' Usa o mecanismo nativo `COPY TO ... (FORMAT PARQUET)` do DuckDB para
#' exportação eficiente. Significativamente mais rápido que `collect()` +
#' `arrow::write_parquet()` para grandes volumes.
#'
#' @param db_path character. Path to the file `.duckdb`.
#' @param output_dir character. Output directory for `.parquet` files.
#'   Criado automaticamente se não existir.
#' @param tables character vector or NULL. Tables to export. If NULL,
#'   exporta todas as tabelas CAGED presentes no banco.
#' @param partition_by character or NULL. Column for partitioning
#'   (ex: `"uf"` ou `"competencia"`). If NULL, generates one file per table.
#' @param overwrite logical. Overwrite existing files. Default: FALSE.
#'
#' @return invisível: tibble com `tabela`, `arquivo` e `registros` exportados.
#'
#' @examples
#' \dontrun{
#' db <- file.path(tempdir(), "caged.duckdb")
#' caged_load(years = 2023, months = 1, db_path = db)
#'
#' # Exportar todas as tabelas
#' caged_to_parquet(db, output_dir = tempdir())
#'
#' # Exportar só caged_mov, particionado por UF
#' caged_to_parquet(
#'   db,
#'   output_dir   = tempdir(),
#'   tables       = "caged_mov",
#'   partition_by = "uf"
#' )
#' }
#'
#' @seealso [caged_info()] para listar tabelas disponíveis.
#' @export
caged_to_parquet <- function(db_path,
                              output_dir,
                              tables       = NULL,
                              partition_by = NULL,
                              overwrite    = FALSE) {

  if (!file.exists(db_path)) {
    cli::cli_abort("Database not found: {.path {db_path}}")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cli::cli_inform("Directory created: {.path {output_dir}}")
  }

  con <- caged_connect(db_path, read_only = TRUE, quiet = TRUE)
  on.exit(.disconnect(con), add = TRUE)

  tabelas_disponiveis <- intersect(
    DBI::dbListTables(con),
    c("caged_mov", "caged_for", "caged_exc", "caged_antigo", "caged_ajustes")
  )

  if (length(tabelas_disponiveis) == 0) {
    cli::cli_abort("No CAGED table found in {.path {db_path}}")
  }

  tabelas_exportar <- if (is.null(tables)) {
    tabelas_disponiveis
  } else {
    invalidas <- setdiff(tables, tabelas_disponiveis)
    if (length(invalidas) > 0) {
      cli::cli_abort(c(
        "Table{?s} not found: {.val {invalidas}}",
        "i" = "Available: {.val {tabelas_disponiveis}}"
      ))
    }
    tables
  }

  cli::cli_h1("Exportando para Parquet")
  cli::cli_inform(c(
    "i" = "{length(tabelas_exportar)} table{?s} to export",
    "i" = "Output: {.path {output_dir}}"
  ))

  resultados <- purrr::map(tabelas_exportar, function(tab) {
    n_registros <- DBI::dbGetQuery(
      con, glue::glue("SELECT COUNT(*) AS n FROM {tab}")
    )$n

    if (!is.null(partition_by)) {
      # Exportação particionada: cria subpastas por valor da coluna
      destino <- file.path(output_dir, tab)

      if (overwrite && dir.exists(destino)) {
        unlink(destino, recursive = TRUE)
      }
      if (!dir.exists(destino)) dir.create(destino, recursive = TRUE)

      sql <- glue::glue(
        "COPY (SELECT * FROM {tab}) TO '{destino}' ",
        "(FORMAT PARQUET, PARTITION_BY ({partition_by}), OVERWRITE_OR_IGNORE TRUE)"
      )
    } else {
      # Exportação simples: um arquivo por tabela
      destino <- file.path(output_dir, paste0(tab, ".parquet"))

      if (file.exists(destino) && !overwrite) {
        cli::cli_inform(c("i" = "{tab}: already exists, skipping. Use {.code overwrite = TRUE}."))
        return(tibble::tibble(tabela = tab, arquivo = destino, registros = NA_integer_,
                              status = "pulado"))
      }

      sql <- glue::glue(
        "COPY (SELECT * FROM {tab}) TO '{destino}' (FORMAT PARQUET)"
      )
    }

    tryCatch({
      DBI::dbExecute(con, sql)
      cli::cli_inform(c(
        "v" = "{tab}: {format(n_registros, big.mark = ',')} registros \u2192 {.path {basename(destino)}}"
      ))
      tibble::tibble(
        tabela    = tab,
        arquivo   = destino,
        registros = n_registros,
        status    = "exportado"
      )
    }, error = function(e) {
      cli::cli_warn("Error exporting {tab}: {conditionMessage(e)}")
      tibble::tibble(tabela = tab, arquivo = destino, registros = NA_integer_,
                     status = "erro")
    })
  }) |> dplyr::bind_rows()

  cli::cli_h2("Done")
  cli::cli_inform(c(
    "v" = "{sum(resultados$status == 'exportado')} table{?s} exported",
    "i" = "Output: {.path {output_dir}}"
  ))

  invisible(resultados)
}
