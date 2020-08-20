# The problem is with: https://ecos.fws.gov/primr/api/survey/FF07RKAN00-020



#' Get PRIMR data for Alaska Refuges
#' 
#' @description Gets survey data for Alaska Refuges from the FWS Refuge PRIMR database using an API
#' 
#' @author McCrea Cobb \email{mccrea_cobb@fws.gov}
#' 
#' @param status The status of the surveys to return. Accepted values include:
#' \itemize{
#' \item Current (the default value)
#' \item Expected
#' \item Historic
#' \item Future
#' }
#' 
#' @param saveit should the returned dataframe be saved
#' @param filedir a connection or the name of the file where the data will be saved
#' @param ... 
#'
#' @return a dataframe containing PRIMR survey data for the Alaska region
#' @export
#'
#' @example get_primr_ak(status = "Current", saveit = TRUE)

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
