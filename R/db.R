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
