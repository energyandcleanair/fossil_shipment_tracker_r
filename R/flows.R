update_flows <- function(source, use_cache=F, date_from=NULL){

  if(length(source)!=1){
    stop("update_flows only supports one source at a time")
  }

  collect_fn <- get(sprintf("%s.get_flows", source))
  flows <- collect_fn(use_cache=use_cache)

  if(!all(c("date","country","unit","commodity","source","partner","value") %in% names(flows))){
    stop("Missing columns")
  }

  # if(!all(flows$commodity %in% commodities)){
  #   stop("Unknown commodity")
  # }

  if(!is.null(date_from)){
    flows <- flows %>% filter(date >= date_from)
  }

  print("Uploading")
  db.upload_flows(flows, source)
  return(flows)
}


