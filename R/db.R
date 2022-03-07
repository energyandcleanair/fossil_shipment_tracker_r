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

db.get_unique_columns <- function(){
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
                  columns=paste0("metadata.", db.get_unique_columns()),
                  index_name="flows_unique_index",
                  unique=T)
}


db.upload_flows <- function(flows,
                            source){
  fs <- db.get_gridfs()
  tmpdir <- tempdir()
  filepath <- file.path(tmpdir, "flows.RDS")
  saveRDS(flows, filepath)

  metadata <- list(source=source)

  # Remove first if exists
  filter <- metadata[db.get_unique_columns()]
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
