get_site_details <- function(site_id ){

  # set path
  path <- glue::glue("site/{site_id}/details")

  # make url
  url <- get_url(path)

  # results
  result <- get_request(url)

  return(result)
}

get_site_dataperiod <- function(site_id){

  # set path
  path <- glue::glue("/site/{site_id}/dataPeriod")

  # make url
  url <- get_url(path)

  # results
  result <- get_request(url)

  return(result)
}


get_site_overview <- function(site_id, tidy = TRUE ){

  # set path
  path <- glue::glue("/site/{site_id}/overview")

  # make url
  url <- get_url(path)

  # results

  result <- get_request(url, tidy = tidy)

  if(tidy){

   result <-  result$overview |>
              purrr::list_flatten(name_spec = "{outer}_{inner}") |>
              tibble::as_tibble() |>
              dplyr::mutate( site_id = site_id ) |>
              janitor::clean_names() |>
              dplyr::mutate_if(is.numeric, as.double)

  }

  return(result)
}




