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
  print("=== Update counter ===")
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

  p <- pipeline_flows %>%
    select(commodity, departure_iso2, destination_iso2, date, value_tonne, value_mwh, value_m3)

  p$updated_on <- lubridate::now()

  print(sprintf("=== Upload flows to postgres (%s) ===", ifelse(production,"production","development")))

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

  print("=== Update counter prices ===")
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

db.upload_portprices_to_posgres <- function(portprices, production=F){

  print(sprintf("=== Uploading portprices (%s) ===", ifelse(production,"production","development")))
  portprices$updated_on <- lubridate::now()
  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))
  dbx::dbxUpsert(db, "portprice", portprices, where_cols=c("port_id", "commodity", "date", "scenario"))
  dbx::dbxDisconnect(db)
}

db.upload_prices_to_posgres <- function(prices, production=F){
  print(sprintf("=== Uploading prices (%s) ===", ifelse(production,"production","development")))

  p <- prices %>%
    select(country_iso2, date, commodity, eur_per_tonne, scenario) %>%
    mutate(date = lubridate::date(date))

  p$updated_on <- lubridate::now()

  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))

  # Do in two times

  # Values with non null country values can be upserted properly
  p_w_country <- p %>% filter(!is.na(country_iso2) & !is.null(country_iso2))
  p_wo_country <- p %>% filter(is.na(country_iso2) | is.null(country_iso2))

  ps <- split(p_w_country, (seq(nrow(p_w_country))-1) %/% 10000)

  pbapply::pblapply(ps, function(p){
    dbx::dbxUpsert(db, "price", p %>% filter(!is.na(country_iso2)), where_cols=c("country_iso2", "commodity", "date", "scenario"))
  })

  # For those with null country, the unique constraint doesn't work
  # so we first remove existing records and replace. During that time,
  # counter calculations would be wrong, that's why we try to minimise the time
  dbx::dbxExecute(db, sprintf("DELETE FROM price WHERE country_iso2 is null AND scenario = ANY(ARRAY[\'%s\'])",
                              paste0(unique(prices$scenario), collapse="','")))
  dbx::dbxInsert(db, "price", p_wo_country)
  dbx::dbxDisconnect(db)
}

db.upload_prices_new_to_posgres <- function(prices, production=F){
  print(sprintf("=== Uploading prices new (%s) ===", ifelse(production,"production","development")))

  p <- prices
  p$updated_on <- lubridate::now()

  list_cols <- c('destination_iso2s',
                 'departure_port_ids',
                 'ship_owner_iso2s',
                 'ship_insurer_iso2s')

  list_cols_bigint <- c('departure_port_ids')
  list_cols_text <- setdiff(list_cols, list_cols_bigint)

  format_array <- function(list) {
    if(length(list)==0 | all(is.na(list))){return('{NULL}')}
    unlist(list) %>%
      paste0(., collapse = ", ") %>%
      paste0('{', ., "}")
  }


  # Replacing list(NULL) with something dbx can upload
  # We'll replace it by NULL below
  for(col in list_cols){
    p[[col]] = unlist(lapply(p[[col]], format_array))
  }

  db <- dbx::dbxConnect(adapter="postgres", url=db.get_pg_url(production=production))
  dbx::dbxUpsert(db, "price_new", as.data.frame(p),
                 batch_size = 10000,
                 where_cols=c("commodity", "date", "scenario",
                              "destination_iso2s", "departure_port_ids",
                              "ship_insurer_iso2s", "ship_owner_iso2s"))


  lapply(list_cols_bigint, function(col){
    dbx::dbxExecute(db, sprintf("UPDATE price_new SET %s = NULL WHERE %s = array[NULL::bigint]", col, col))})

  lapply(list_cols_text, function(col){
    dbx::dbxExecute(db, sprintf("UPDATE price_new SET %s = NULL WHERE %s = array[NULL::varchar]", col, col))})

  # Remove old pricing that may not have been erased by the upsert
  dbx::dbxExecute(db, "DELETE FROM price_new p USING (SELECT max(updated_on) as updated_on FROM price_new) p2 WHERE p.updated_on < p2.updated_on")

  dbx::dbxDisconnect(db)
}

db.upload_flows <- function(flows,
                            source){
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
    print("Flows already exist. Replacing them")
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
  print(sprintf("%d row(s) removed", nrow(found)))
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
