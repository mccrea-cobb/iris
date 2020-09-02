#' Get refuge survey data from PRIMR using web services
#'
#' @description Gets data from the FWS Refuge PRIMR database using a web service API (https://my.usgs.gov/confluence/display/FEAH/Survey+Web+Service+API)
#'
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#'
#' @param cost_code The unique refuge cost code. Use "%25" as a wild card (e.g., "FF07%25" for all Region 7 surveys)
#' @param type An exact matching (case insensitive) survey type name. Accepted values include:
#' #' #' \itemize{
#' \item Inventory
#' \item Coop Inventory
#' \item Baseline Monitoring
#' \item Coop Baseline Monitoring
#' \item Monitoring to Inform Management
#' \item Coop Monitoring to Inform Management
#' \item Research
#' \item Coop Research
#' @param status An exact matching survey status name. Accepted values include:
#' #' \itemize{
#' \item Current
#' \item Expected
#' \item Historic
#' \item Future
#' }
#' @param species the species name, matched against any listed or non-listed species scientific name or common name, case insensitively
#' @param response_order The order of the returned data frame By default, response_order=id.
#' @param as_df The format of the data to be returned. Acceptable values include:
#' #' #' \itemize{
#' \item df - Results will be returned as a flattened data frame
#' \item list - Results will be returned as a list of data frames for each refuge
#'
#' @return a data frame containing FWS refuge survey metadata
#'
#' @export
#'
#' @example
#' \dontrun{
#' primr_get(cost_code = "FF01RTBL00")
#' }

primr_get <- function(max = 1000,
                      response_order = "id",
                      cost_code = NULL,
                      type = NULL,
                      status = NULL,
                      species = NULL,
                      output = "list"){
  # require(httr)
  # require(jsonlite)

  # Query parameters
  query_params <- list(max = max,
                       order = response_order,
                       ccc = cost_code,
                       type = type,
                       status = status,
                       species = species)

  # Make a GET request
  response <- GET(url = "https://ecos.fws.gov/primr/api/survey/list",
                  query = query_params)

  # Is there an HTTP error?
  if(http_error(response)){
    # Throw an error
    stop("The request failed")
  } else {
    # Return the response's content
    json_content <- content(response, as="text")
  }

  df_content <- fromJSON(json_content, simplifyDataFrame = TRUE, flatten = TRUE)$data

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
#' \item Current (the default value)
#' \item Expected
#' \item Historic
#' \item Future
#' }
#'
#' @param saveit a logical value indicating whether the returned data frame be saved
#' @param filedir a connection or the name of the file where the data will be saved
#' @param ... inherited arguments
#'
#' @return a data frame containing PRIMR survey data for the Alaska region
#'
#' @export
#'
#' @example
#' \dontrun{
#' get_primr_ak(status = "Current", saveit = TRUE)
#' }

primr_get_ak <- function(status = NULL,
                         saveit = FALSE,
                         filedir,
                         ...){

  # Required packages and functions
  library(tidyverse)
  library(naniar)  # To deal with missing values
  source("./code/functions/primr_get.R")
  source("./code/functions/cost_codes.R")

  # Create a cost code dataframe
  ccs <- cost_codes()

  # Filter out Kanuti, the problem child in PRIMR..
  ccs <- ccs %>%
    filter(refuge != "Kanuti") %>%
    droplevels()

  # Get the survey data from the PRIMR API (requires internet connectivity)
  survey_data <- ccs %>%
    apply(1, function(x) primr_get(cost_code = x[2], status = status))

  # Deal with not having protocol-related variables when a station has no protocol docs.
  survey_data <- survey_data %>%
    map_if(~!("protocol" %in% names(.x)), ~.x %>% mutate(protocol=NA), .depth = 1) %>%
    map_if(~!("protocolUsed" %in% names(.x)), ~.x %>% mutate(protocolUsed=NA), .depth = 1) %>%
    map_if(~!("protocol.servCatId" %in% names(.x)), ~.x %>% mutate(protocol.servCatId=NA), .depth = 1) %>%
    map_if(~!("protocol.servCatTitle" %in% names(.x)), ~.x %>% mutate(protocol.servCatTitle=NA), .depth = 1) %>%
    map_if(~!("protocol.servCatVersion" %in% names(.x)), ~.x %>% mutate(protocol.servCatVersion=NA), .depth = 1) %>%
    map_if(~!("protocol.referenceType" %in% names(.x)), ~.x %>% mutate(protocol.referenceType=NA), .depth = 1) %>%
    reduce(rbind)  # Reduce the list of dataframes into a single dataframe

  # Format the dataset
  survey_data <- survey_data %>%
    mutate(surveyId = as.factor(surveyId),
           name = as.factor(name)) %>%
    naniar::replace_with_na(replace = list(startYear = "Future/TBD",  # Replace values with NAs
                                           endYear = c("Future/TBD",
                                                       "Indefinite")))

  if(saveit == TRUE) {
    save(survey_data, file = filedir)
  }

  return(survey_data)
}
