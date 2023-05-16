
get_site_dataperiod <- function(site_id, start_date , end_date, time_unit, return_format,...){

  # assertions

  # set path
  path <- glue::glue("/site/{site_id}/energy")

  # make url
  url <- get_url(path)

  # query
  query <- list( startDate  = start_date,
                 endDate    = end_date ,
                 timeUnit   = time_unit )

  # results
  result <- get_request(url, query, return_format,...)

  return(result)
}


get_site_energy <- function(site_id, start_date , end_date, return_format,...){

  # assertions

  # set path
  path <- glue::glue("/site/{site_id}/timeFrameEnergy")

  # make url
  url <- get_url(path)

  # query
  query <- list( startDate  = start_date,
                 endDate    = end_date)

  # results
  result <- get_request(url, query, return_format,...)

  return(result)
}




get_site_power <- function(site_id, start_date , end_date, return_format,...){

  # assertions
  # max 30 days


  # set path
  path <- glue::glue("/site/{site_id}/power")

  # make url
  url <- get_url(path)

  # query
  query <- list( startTime  = start_date,
                 endTime    = end_date )

  # results
  result <- get_request(url, query, return_format,...)

  return(result)
}

get_site_power_detail <- function(site_id, start_time , end_time, meters, return_format,...){

  # assertions
  # max 30 days


  # set path
  path <- glue::glue("/site/{site_id}/powerDetails")

  # make url
  url <- get_url(path)

  # query
  query <- list( startTime  = start_time,
                 endTime    = end_time )

  # results
  result <- get_request(url, query, return_format,...)

  return(result)
}

