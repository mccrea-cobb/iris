
#' Read a table from IRIS
#'
#' @description Reads in a SQL table from the FWS IRIS Data Warehouse as a tibble
#'
#' @return A dataframe
#' @export
#'
#' @examples read_iris("Refuges", "DimSurvey")

read_iris <- function(schema,
                      tbl_name){  # Make sure to VPN in first

  library(DBI)
  library(odbc)
  library(tidyverse)
  source("R/query_iris.R")

  df <- query_iris(schema, tbl_name)


}
