#' Get refuge survey data from PRIMR
#' 
#' @description Gets data from the FWS Refuge PRIMR database using a web service API (https://my.usgs.gov/confluence/display/FEAH/Survey+Web+Service+API)
#' 
#' @author McCrea Cobb \email{mccrea_cobb@fws.gov}
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
#' @param response_order The order of the returned dataframe By default, response_order=id.
#' @param as_df The format of the data to be returned. Acceptable values include:
#' #' #' \itemize{
#' \item df - Results will be returned as a flattened dataframe
#' \item list - Results will be returned as a list of dataframes for each refuge
#'
#' @return a dataframe containing FWS refuge survey metadata
#' 
#' @export
#'
#' @example primr_get(cost_code = "FF01RTBL00")

primr_get <- function(max=1000,
                      response_order="id", 
                      cost_code=NULL, 
                      type=NULL, 
                      status=NULL, 
                      species=NULL,
                      output="list"){
  require(httr)
  require(jsonlite)
  
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

