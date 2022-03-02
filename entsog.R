api_req <- function(url, params=list(), limit=-1){

  params['limit'] <- limit
  message(url, " ...")
  req <- httr::GET(url, query=params)

  if(httr::status_code(req) != "200"){
    print(sprintf("Failed (Code %s) for: ", httr::status_code(req)))
    print(url)
    print(params)
    warning(httr::content(req, encoding = "UTF-8"))
    return(NULL)
  }

  en_cont <- httr::content(req, as="text", encoding = "UTF-8")
  res <- jsonlite::fromJSON(en_cont)

  if(res$meta$total > res$meta$count * 1.01){ #+2: for some reason, sometimes total is +1 or +2
    warning(sprintf("More data available (%s/%s). Increase limit or implement a loop here...", res$meta$count, res$meta$total))
  }

  return(res)
}

api_req_safe <- function(...) {
  purrr:::capture_error(api_req(...), otherwise, quiet)
}


entsog.balancing_zones <- function(){
  url <- "https://transparency.entsog.eu/api/v1/balancingZones"
  d <- api_req(url)
  d$balancingZones
}


entsog.operators <- function(){
  url <- "https://transparency.entsog.eu/api/v1/operators"
  d <- api_req(url)
  d$operators
}


entsog.interconnections <- function(from_operator_key=NULL){
  url <- "https://transparency.entsog.eu/api/v1/interconnections"
  params <- list()

  if(!is.null(operator_key)){
    params['fromOperatorKey'] <- paste(from_operator_key, sep=",", collapse=",")
  }

  d <- api_req(url, params = params)
  d$interconnections
}


entsog.physical_flows <- function(operator_key, point_key, direction, date_from="2019-01-01", date_to=lubridate::today(), limit=-1){

  url <- "https://transparency.entsog.eu/api/v1/operationalData"

  if(!is.null(point_key) && is.null(operator_key)){
    stop("Needs to specify operator_key when point_key is given.")
  }

  # Can only do one operator at a time
  if(!is.null(operator_key) && length(unique(operator_key))>1){
    message("Splitting by operator")
    operator_points <- split(point_key, operator_key)
    return(lapply(names(operator_points), function(operator){
      entsog.physical_flows(operator_key=operator,
                            point_key = operator_points[[operator]],
                            direction=direction,
                            date_from=date_from,
                            date_to=date_to,
                            limit=limit)
    }) %>%
      do.call(bind_rows, .))
  }

  # Can only do limited days per call. Doing a call per semester
  dates <- seq(as.Date(date_from), as.Date(date_to), by="day")
  dates_group <- lubridate::year(dates)
  if(length(unique(dates_group))>1){
    message("Splitting by dates")
    return(lapply(split(dates, dates_group), function(dates){
      entsog.physical_flows(operator_key=operator_key,
                            point_key = point_key,
                            direction=direction,
                            date_from=min(dates),
                            date_to=max(dates),
                            limit=limit)
    }) %>%
      do.call(bind_rows, .))
  }

  params <- list(
    indicator="Physical Flow",
    periodType="day",
    timezone="CET"
  )

  if(!is.null(operator_key)){
    params['operatorKey'] <- paste(operator_key, sep=",", collapse=",")
  }

  if(!is.null(point_key)){
    params['pointKey'] <- paste(point_key, sep=",", collapse=",")
  }

  if(!is.null(date_from)){
    params['from'] <- strftime(as.Date(date_from),"%Y-%m-%d")
  }

  if(!is.null(date_to)){
    params['to'] <- strftime(as.Date(date_to),"%Y-%m-%d")
  }

  if(!is.null(direction)){
    params['directionKey'] <- direction
  }

  d <- api_req(url, params=params, limit=limit)

  if(is.null(d)){return(NULL)}

  d$operationalData %>%
    mutate_at("value", as.numeric) %>%
    mutate_at(c("isCmpRelevant","isCamRelevant"), as.logical) %>%
    mutate_at(c("periodFrom","periodTo"), as.POSIXct, tz="CET") %>%
    mutate(date=as.Date(periodFrom))
}

