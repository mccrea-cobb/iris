#' Get refuge survey data from PRIMR using web services
#'
#' @description Gets data from the FWS Refuge PRIMR database using a web service API (https://my.usgs.gov/confluence/display/FEAH/Survey+Web+Service+API)
#'
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#'
#' @param max The number of rows to return. Default is \code{1000}.
#' @param response_order The row order of the returned data frame. Default is \code{"id"}.
#' @param cost_code The unique refuge cost code. Use \%25 as a wild card (e.g., FF07\%25 for all Region 7 surveys)
#' @param type An exact matching (case insensitive) survey type name. Accepted values include:
#' \itemize{
#'  \item{Inventory}{}
#'  \item{Coop Inventory}{}
#'  \item{Baseline Monitoring}{}
#'  \item{Coop Baseline Monitoring}{}
#'  \item{Monitoring to Inform Management}{}
#'  \item{Coop Monitoring to Inform Management}{}
#'  \item{Research}{}
#'  \item{Coop Research}{}
#'  }
#' @param status An exact matching survey status name. Accepted values include:
#' \itemize{
#'  \item{Current}{}
#'  \item{Expected}
#'  \item{Historic}
#'  \item{Future}
#' }
#' @param species the species name, matched against any listed or non-listed species scientific name or common name, case insensitively
#' @param response_order The order of the returned data frame By default, response_order=id.
#' @param output The format of the data to be returned. Acceptable values include:
#' \itemize{
#'  \item{df}{Results will be returned as a flattened data frame}
#'  \item{list}{Results will be returned as a list of data frames for each refuge}
#'  }
#' @param ... arguments to pass
#'
#' @return a data frame containing FWS refuge survey metadata
#'
#' @importFrom httr GET
#' @importFrom httr http_error
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#' primr_get(cost_code = "FF01RTBL00")
#' }

primr_get <- function(max = 1000,
                      response_order = "id",
                      cost_code = NULL,
                      type = NULL,
                      status = NULL,
                      species = NULL,
                      output = "list",
                      ...){
  # Query parameters
  query_params <- list(max = max,
                       order = response_order,
                       ccc = cost_code,
                       type = type,
                       status = status,
                       species = species)

  # Make a GET request
  response <- httr::GET(url = "https://ecos.fws.gov/primr/api/survey/list",
                  query = query_params)

  # Is there an HTTP error?
  if(httr::http_error(response)){
    # Throw an error
    stop("The request failed")
  } else {
    # Return the response's content
    json_content <- httr::content(response, as="text")
  }

  df_content <- jsonlite::fromJSON(json_content, simplifyDataFrame = TRUE, flatten = TRUE)$data

  return(df_content)
}




##----


#' Get PRIMR data for Alaska Refuges using web services
#'
#' @description Gets survey data for Alaska Refuges from the FWS Refuge PRIMR database using an API
#'
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#'
#' @param status the status of the surveys to return. Accepted values include:
#' \itemize{
#'  \item{Current (the default value)}
#'  \item{Expected}
#'  \item{Historic}
#'  \item{Future}
#' }
#'
#' @param saveit a logical value indicating whether the returned data frame be saved
#' @param filedir a connection or the name of the file where the data will be saved
#' @param ... inherited arguments
#'
#' @return a data frame containing PRIMR survey data for the Alaska region
#'
#' @importFrom naniar replace_with_na
#' @importFrom dplyr mutate
#' @importFrom purrr map_if
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' get_primr_ak(status = "Current", saveit = TRUE)
#' }

primr_get_ak <- function(status = NULL,
                         saveit = FALSE,
                         filedir,
                         ...){
  # Create a cost code dataframe
  ccs <- get_cost_codes()

  # Filter out Kanuti, the problem child in PRIMR..
  ccs <- ccs %>%
    dplyr::filter(refuge != "Kanuti") %>%
    droplevels()

  # Get the survey data from the PRIMR API (requires internet connectivity)
  survey_data <- ccs %>%
    apply(1, function(x) primr_get(cost_code = x[2], status = status))

  # Deal with not having protocol-related variables when a station has no protocol docs
  survey_data <- survey_data %>%
    purrr::map_if(~!("protocol" %in% names(.x)), ~.x %>% dplyr::mutate(protocol=NA), .depth = 1) %>%
    purrr::map_if(~!("protocolUsed" %in% names(.x)), ~.x %>% dplyr::mutate(protocolUsed=NA), .depth = 1) %>%
    purrr::map_if(~!("protocol.servCatId" %in% names(.x)), ~.x %>% dplyr::mutate(protocol.servCatId=NA), .depth = 1) %>%
    purrr::map_if(~!("protocol.servCatTitle" %in% names(.x)), ~.x %>% dplyr::mutate(protocol.servCatTitle=NA), .depth = 1) %>%
    purrr::map_if(~!("protocol.servCatVersion" %in% names(.x)), ~.x %>% dplyr::mutate(protocol.servCatVersion=NA), .depth = 1) %>%
    purrr::map_if(~!("protocol.referenceType" %in% names(.x)), ~.x %>% dplyr::mutate(protocol.referenceType=NA), .depth = 1) %>%
    purrr::reduce(rbind)  # Reduce the list of data frames into a single data frame

  # Format the dataset
  survey_data <- survey_data %>%
    dplyr::mutate(surveyId = as.factor(surveyId),
           name = as.factor(name)) %>%
    naniar::replace_with_na(replace = list(startYear = "Future/TBD",  # Replace values with NAs
                                           endYear = c("Future/TBD",
                                                       "Indefinite")))

  if(saveit == TRUE) {
    save(survey_data, file = filedir)
  }

  return(survey_data)
}
