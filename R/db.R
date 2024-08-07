db.get_pg_url <- function(production){
  readRenviron(".Renviron")
  if(production){
    Sys.getenv("FOSSIL_DB_PRODUCTION")
  }else{
    Sys.getenv("FOSSIL_DB_DEVELOPMENT")
  }
}
db.get_collection <- function(collection_name){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::mongo(collection=collection_name, db="russian_fossil", url=connection_string)
}

db.get_gridfs <- function(){
  readRenviron(".Renviron")
  connection_string=Sys.getenv("CREA_MONGODB_URL")
  mongolite::gridfs(db="russian_fossil", prefix="flows", url=connection_string)
}

db.get_counter_collection <- function(test=F){
  if(test){
    db.get_collection("counter_test")
  }else{
    db.get_collection("counter")
  }
}

db.get_counter_prices_collection <- function(test=F){
  if(test){
    db.get_collection("counter_prices_test")
  }else{
    db.get_collection("counter_prices")
  }
}

db.get_unique_columns_counter <- function(){
  c("date")
}

db.get_unique_columns_counter_prices <- function(){
  c("date","commodity")
}

db.get_unique_columns_flows <- function(){
  c("source")
}

db.create_index <- function(collection_name, columns, index_name, unique=T){
  cmd <- sprintf('{"createIndexes":"%s",
        "indexes":[{"key":{%s},
        "name":"%s","unique": %s}]}',
                 collection_name,
                 paste(sprintf("\"%s\":1",columns), collapse=","),
                 index_name,
                 ifelse(unique, "true","false"))

  m <- db.get_collection(collection_name)
  m$run(cmd)
}

db.setup_db <- function(){
  db.create_index(collection_name="flows.files",
                  columns=paste0("metadata.", db.get_unique_columns_flows()),
                  index_name="flows_unique_index",
                  unique=T)

  db.create_index(collection_name="counter",
                  columns=db.get_unique_columns_counter(),
                  index_name="flows_unique_index",
                  unique=T)

  db.create_index(collection_name="counter_prices",
                  columns=db.get_unique_columns_counter_prices(),
                  index_name="flows_unique_index",
                  unique=T)
}

db.download_counter <- function(test=F){
  db.get_counter_collection(test=test)$find()
}

db.update_counter <- function(counter_data, test=F){
  log_level(UPLOAD, "Update counter")
  if(!all(c("date","oil_eur","gas_eur","coal_eur","total_eur") %in% names(counter_data))){
    stop("Missing columns")
  }

  col <- db.get_counter_collection(test=test)
  for(i in seq(nrow(counter_data))){
    record <- as.list(counter_data[i,])
    filter <- jsonlite::toJSON(record['date'], auto_unbox = T)
    data <- jsonlite::toJSON(list(`$set`=record[setdiff(names(record),"date")]), auto_unbox = T)
    col$update(filter, data, upsert = TRUE)
  }
}


db.upload_flows_to_postgres <- function(pipeline_flows, production=F){
  db_env <- ifelse(production,"production","development")
  log_level(UPLOAD, "Uploading flows to postgres ({db_env})")

  p <- pipeline_flows %>%
    select(commodity, departure_iso2, destination_iso2, date, value_tonne, value_mwh, value_m3)

  p$updated_on <- lubridate::now()

  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))
  chunk_size <- 1000
  pbapply::pblapply(split(p, (seq(nrow(p))-1) %/% chunk_size), function(chunk){
    dbx::dbxUpsert(db, "pipelineflow", chunk, where_cols=c("commodity", "date", "departure_iso2", "destination_iso2"))
    # And remove 0 values for query answers not to exceed max_size (32MB)
    dbx::dbxDelete(db, "pipelineflow", where=data.frame(value_tonne=0))
  })

  dbx::dbxDisconnect(db)
}


db.update_counter_prices <- function(prices, test=F){

  log_level(UPLOAD, "Uploading counter prices")
  p <- prices %>%
    filter(date>="2021-01-01") %>%
    group_by(date, source, commodity, transport, unit) %>%
    summarise_at(c("value","value_eur"), sum, na.rm=T) %>%
    filter(unit=="tonne") %>%
    filter(!is.na(source)) %>%
    mutate(eur_per_tonne=value_eur / value) %>%
    ungroup()


  if(!all(c("date","source","commodity","unit","value","value_eur","eur_per_tonne") %in% names(p))){
    stop("Missing columns")
  }

  col <- db.get_counter_prices_collection(test=test)
  for(i in seq(nrow(p))){
    record <- as.list(p[i,])
    filter <- jsonlite::toJSON(record[db.get_unique_columns_counter_prices()], auto_unbox = T)
    data <- jsonlite::toJSON(list(`$set`=record[setdiff(names(record),"date")]), auto_unbox = T)
    col$update(filter, data, upsert = TRUE)
  }


  # print("=== Update counter prices (Postgres) ===")
  # p_postgres <- p %>%
  #   select(date, commodity, eur_per_tonne) %>%
  #   mutate(commodity=recode(commodity,
  #                           oil="crude_oil",
  #                           oil_others="oil_products"))
  # db.upload_prices_to_posgres(prices=p_postgres)
}

db.upload_scenario_names <- function(scenario_names, production=T){
  db_env <- ifelse(production,"production","development")
  log_level(UPLOAD, "Uploading scenario names ({db_env})")

  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))
  dbx::dbxUpsert(db, "price_scenario", as.data.frame(scenario_names),
                 where_cols=c("id"))
  dbx::dbxDisconnect(db)
}


db.rebuild_price_table <- function(p, production=F, table='price'){
  db_env <- ifelse(production,"production","development")
  log_level(UPLOAD, "Rebuilding prices ({db_env})")

  p <- p_bkp
  p$updated_on <- lubridate::now()

  list_cols_bigint <- c('departure_port_ids')
  list_cols <- c('destination_iso2s',
                 'departure_port_ids',
                 'ship_owner_iso2s',
                 'ship_insurer_iso2s')

  list_cols_text <- setdiff(list_cols, list_cols_bigint)

  prepare_for_db <- function(p){

    format_array <- function(list) {
      if(length(list)==0 | all(is.na(list))){return('{NULL}')}
      # if(length(list)==0 | all(is.na(list))){return(NA)}
      unlist(list) %>%
        paste0(., collapse = ", ") %>%
        paste0('{', ., "}")
    }


    # Replacing list(NULL) with something dbx can upload
    # We'll replace it by NULL below
    for(col in list_cols){
      log_debug("Replacing any list(NULL) in {col} with NULL")
      p[[col]] = unlist(lapply(p[[col]], format_array))
    }
    return(p)
  }

  chunk_size=10000
  p <- prepare_for_db(p)

  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))

  RPostgres::dbBegin(db)
  RPostgres::dbExecute(db,  glue("TRUNCATE TABLE {table}"))
  pbapply::pblapply(split(p, (seq(nrow(p))-1) %/% chunk_size),
                    function(p_chunk){
                      RPostgres::dbWriteTable(db, table, as.data.frame(p_chunk), append=T)
                    })
  RPostgres::dbCommit(db)

}



db.upload_prices_to_posgres <- function(prices, rebuild=F, production=F, buffer_days=60,
                                        table='price'){
  db_env <- ifelse(production,"production","development")
  log_level(UPLOAD, "Uploading prices ({db_env})")

  prices$updated_on <- lubridate::now()


  list_cols <- c('destination_iso2s',
                 'departure_port_ids',
                 'ship_owner_iso2s',
                 'ship_insurer_iso2s')

  list_cols_bigint <- c('departure_port_ids')
  list_cols_text <- setdiff(list_cols, list_cols_bigint)

  # TEMPORARY, before updating constraint
  if(! 'departure_port_ids' %in% names(prices)){
    prices <- prices %>%
      rowwise() %>%
      mutate(departure_port_ids = list(NULL)) %>%
      ungroup()
  }

  format_array <- function(list) {
    if(length(list)==0 | all(is.na(list))){return('{NULL}')}
    unlist(list) %>%
      paste0(., collapse = ", ") %>%
      paste0('{', ., "}")
  }

  # Replacing list(NULL) with something dbx can upload
  # We'll replace it by NULL below
  for(col in list_cols){
    prices[[col]] = unlist(lapply(prices[[col]], format_array))
  }

  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))

  if(!rebuild){
    max_date <- dbx::dbxSelect(db, glue("SELECT max(date) from {table};"))
    prices <- prices %>%
      filter(date >= as.Date(max_date$max) - lubridate::days(buffer_days))
  }



  dbx::dbxUpsert(db, table, as.data.frame(prices),
                 batch_size = 10000,
                 where_cols=c("commodity", "date", "scenario",
                              "destination_iso2s", "departure_port_ids",
                              "ship_insurer_iso2s", "ship_owner_iso2s"))


  # lapply(list_cols_bigint, function(col){
  #   dbx::dbxExecute(db, sprintf("UPDATE price SET %s = NULL WHERE %s = array[NULL::bigint]", col, col))})
  #
  # lapply(list_cols_text, function(col){
  #   dbx::dbxExecute(db, sprintf("UPDATE price SET %s = NULL WHERE %s = array[NULL::varchar]", col, col))})

  # Remove old pricing that may not have been erased by the upsert
  if(rebuild){
    dbx::dbxExecute(db, glue("DELETE FROM {table} p USING (SELECT max(updated_on) as updated_on FROM {table}) p2 WHERE p.updated_on < p2.updated_on"))
  }

  dbx::dbxDisconnect(db)
}

db.upload_flows <- function(flows,
                            source){

  log_level(UPLOAD, "Uploading flows")
  fs <- db.get_gridfs()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "flows.RDS")
  saveRDS(flows, filepath)

  metadata <- list(source=source)

  # Remove first if exists
  filter <- metadata[db.get_unique_columns_flows()]
  names(filter) <- paste0("metadata.", names(filter))
  found <- fs$find(jsonlite::toJSON(filter,auto_unbox=T))
  if(nrow(found)>0){
    log_info("Flows already exist. Replacing them")
    fs$remove(paste0("id:", found$id))
  }

  # And then upload
  fs$upload(filepath,
            name=basename(filepath), content_type=NULL,
            metadata=jsonlite::toJSON(metadata, auto_unbox=T))
}


db.find_flows <- function(source=NULL){
  fs <- db.get_gridfs()

  filter <- list(metadata.source=source)
  filter <- filter[!unlist(lapply(filter, is.null))]
  fs$find(jsonlite::toJSON(filter,auto_unbox=T))
}


db.remove_flows <- function(source){
  fs <- db.get_gridfs()
  found <- db.find_flows(source=source)
  if(nrow(found)>0) fs$remove(paste0("id:", found$id))
  log_info("{nrow(found)} row(s) removed")
}


db.available_sources <- function(){
  found <- db.find_flows()
  sources <- unlist(lapply(found$metadata, function(x) jsonlite::fromJSON(x)$source))
  return(sources)
}


db.download_flows <- function(source){
  fs <- db.get_gridfs()
  found <- db.find_flows(source=source)
  if(nrow(found)==0) return(NULL)

  result <- lapply(found$metadata, function(x) as.data.frame(jsonlite::fromJSON(x))) %>%
    do.call(bind_rows, .)

  ids <- paste0("id:",found$id)
  flows <- lapply(ids, function(id){
    filepath <- tempfile()
    fs$download(id, filepath)
    flows <- readRDS(filepath)
    file.remove(filepath)
    return(flows)
  })

  do.call(bind_rows, flows)
}
