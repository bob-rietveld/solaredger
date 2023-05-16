#' Get Api Key
#'
#' @return
#' @keywords internal
#' @examples
get_api_key <- function() {

  # check for credentials
  solar_token = Sys.getenv('SOLAR_API_KEY')

  # check for path
  if (solar_token == '') stop(sprintf('variable %s missing from file ~/.Renviron', 'SOLAR_API_KEY'))


  return(solar_token)

}


#' Get SolarEdge index url
#'
#' @param set_path
#'
#' @return
#' @keywords internal
#'
#' @examples
get_url <- function( set_path = NA) {

  # check for credentials
  solar_base = Sys.getenv('SOLAR_BASE_URL')

  # check for path
  if (solar_base == '') stop(sprintf('variable %s missing from file ~/.Renviron', 'SOLAR_BASE_URL'))

  # set url
  solar_api_url <- glue::glue("{solar_base}")

  if(!is.na(set_path)){

    # create url
    solar_api_url <- httr::modify_url(solar_api_url, path = set_path)

    # print
    print(solar_api_url)
  }

  return(solar_api_url)

}


get_request <- function( url, query = list() , return_format = "json", ... ){

  # make assertions


  # get key
  api_key <- get_api_key()

  # prep query params
  query <-  c(query, api_key = api_key)

  # set return type
  if(return_format == "csv")
    {
      return_type <- httr::accept("text/csv")
    }
  else
    {
      return_type <- httr::accept_json()
    }

  # get response
  response <- httr::GET( url,
                         query = query ,
                         return_type)

  # parse result
  result <- handle_respons(response, ...)


  return(result)
}

#' Handle the API Response
#'
#' @param response
#' @param tidy
#' @keywords internal
#' @return
#'
#' @examples
handle_respons <- function( response , tidy = FALSE ){

  # set response object
  result <- structure( list( http = response,
                             content = NULL,
                             status_code = NULL))

  # handle result
  result$status_code <- httr::status_code(response)

  if (result$status_code != 200) {

    return(result)
  }
  else {

    # set the results
    result$content <- httr::content(response)
  }

  # simple_results
  if(tidy)
  {

    result <- result$content

    # handle result
    #result <-  result |>
    #           dplyr::filter(! value == "null") |>
    #           dplyr::mutate( value = as.double(value))
  }

  return(result)
}

