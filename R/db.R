db.resolve_environment <- function(environment = NULL) {
  valid_envs <- c("development", "production")

  if (is.null(environment)) {
    readRenviron(".Renviron")
    environment <- Sys.getenv("ENVIRONMENT", unset = "development")
  }

  environment <- tolower(environment)
  if (!environment %in% valid_envs) {
    stop(sprintf("ENVIRONMENT must be one of: %s", paste(valid_envs, collapse = ", ")))
  }

  environment
}

db.get_pg_url <- function(environment = NULL) {
  env <- db.resolve_environment(environment = environment)
  readRenviron(".Renviron")
  if (env == "production") {
    Sys.getenv("FOSSIL_DB_PRODUCTION")
  } else {
    Sys.getenv("FOSSIL_DB_DEVELOPMENT")
  }
}

db.pg_select <- function(sql) {
  url <- db.get_pg_url()
  if (!nzchar(url)) {
    stop("FOSSIL_DB_DEVELOPMENT or FOSSIL_DB_PRODUCTION must be configured before running SQL queries")
  }

  log_level(REQUEST, "Querying Postgres")
  db <- dbx::dbxConnect(adapter = "postgres", url = url)
  on.exit(dbx::dbxDisconnect(db), add = TRUE)
  dbx::dbxSelect(db, sql)
}

db.upload_flows_to_postgres <- function(pipeline_flows) {
  log_level(UPLOAD, "Uploading flows to postgres")

  p <- pipeline_flows %>%
    select(
      commodity,
      departure_iso2,
      destination_iso2,
      date,
      value_tonne,
      value_mwh,
      value_m3,
      entry_mode,
      process
    )

  p$updated_on <- lubridate::now()

  db <- dbx::dbxConnect(adapter = "postgres", url = db.get_pg_url())
  chunk_size <- 1000
  pbapply::pblapply(split(p, (seq(nrow(p)) - 1) %/% chunk_size), function(chunk) {
    dbx::dbxUpsert(db, "pipelineflow", chunk, where_cols = c("commodity", "date", "departure_iso2", "destination_iso2", "entry_mode"))
    # And remove 0 values for query answers not to exceed max_size (32MB)
    dbx::dbxDelete(db, "pipelineflow", where = data.frame(value_tonne = 0))
  })

  dbx::dbxDisconnect(db)
}

db.upload_scenario_names <- function(scenario_names) {
  log_level(UPLOAD, "Uploading scenario names")

  db <- dbx::dbxConnect(adapter = "postgres", url = db.get_pg_url())
  dbx::dbxUpsert(db, "price_scenario", as.data.frame(scenario_names),
    where_cols = c("id")
  )
  dbx::dbxDisconnect(db)
}


db.rebuild_price_table <- function(p, table = "price") {
  log_level(UPLOAD, "Rebuilding prices")

  p <- p_bkp
  p$updated_on <- lubridate::now()

  list_cols_bigint <- c("departure_port_ids")
  list_cols <- c(
    "destination_iso2s",
    "departure_port_ids",
    "ship_owner_iso2s",
    "ship_insurer_iso2s"
  )

  list_cols_text <- setdiff(list_cols, list_cols_bigint)

  prepare_for_db <- function(p) {
    format_array <- function(list) {
      if (length(list) == 0 | all(is.na(list))) {
        return("{NULL}")
      }
      # if(length(list)==0 | all(is.na(list))){return(NA)}
      unlist(list) %>%
        paste0(., collapse = ", ") %>%
        paste0("{", ., "}")
    }


    # Replacing list(NULL) with something dbx can upload
    # We'll replace it by NULL below
    for (col in list_cols) {
      log_debug("Replacing any list(NULL) in {col} with NULL")
      p[[col]] <- unlist(lapply(p[[col]], format_array))
    }
    return(p)
  }

  chunk_size <- 10000
  p <- prepare_for_db(p)

  db <- dbx::dbxConnect(adapter = "postgres", url = db.get_pg_url())

  RPostgres::dbBegin(db)
  RPostgres::dbExecute(db, glue("TRUNCATE TABLE {table}"))
  pbapply::pblapply(
    split(p, (seq(nrow(p)) - 1) %/% chunk_size),
    function(p_chunk) {
      RPostgres::dbWriteTable(db, table, as.data.frame(p_chunk), append = T)
    }
  )
  RPostgres::dbCommit(db)
}


db.upload_prices_to_posgres <- function(prices, rebuild = F, buffer_days = 60,
                                        table = "price") {
  log_level(UPLOAD, "Uploading prices")

  prices$updated_on <- lubridate::now()


  list_cols <- c(
    "destination_iso2s",
    "departure_port_ids",
    "ship_owner_iso2s",
    "ship_insurer_iso2s"
  )

  list_cols_bigint <- c("departure_port_ids")
  list_cols_text <- setdiff(list_cols, list_cols_bigint)

  # TEMPORARY, before updating constraint
  if (!"departure_port_ids" %in% names(prices)) {
    prices <- prices %>%
      rowwise() %>%
      mutate(departure_port_ids = list(NULL)) %>%
      ungroup()
  }

  format_array <- function(list) {
    if (length(list) == 0 | all(is.na(list))) {
      return("{NULL}")
    }
    unlist(list) %>%
      paste0(., collapse = ", ") %>%
      paste0("{", ., "}")
  }

  # Replacing list(NULL) with something dbx can upload
  # We'll replace it by NULL below
  for (col in list_cols) {
    prices[[col]] <- unlist(lapply(prices[[col]], format_array))
  }

  db <- dbx::dbxConnect(adapter = "postgres", url = db.get_pg_url())

  if (!rebuild) {
    max_date <- dbx::dbxSelect(db, glue("SELECT max(date) from {table};"))
    prices <- prices %>%
      filter(date >= as.Date(max_date$max) - lubridate::days(buffer_days))
  }


  dbx::dbxUpsert(db, table, as.data.frame(prices),
    batch_size = 10000,
    where_cols = c(
      "commodity", "date", "scenario",
      "destination_iso2s", "departure_port_ids",
      "ship_insurer_iso2s", "ship_owner_iso2s"
    )
  )


  # Remove old pricing that may not have been erased by the upsert
  if (rebuild) {
    dbx::dbxExecute(db, glue("DELETE FROM {table} p USING (SELECT max(updated_on) as updated_on FROM {table}) p2 WHERE p.updated_on < p2.updated_on"))
  }

  dbx::dbxDisconnect(db)
}
