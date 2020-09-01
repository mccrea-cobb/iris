
#' Read a table from IRIS
#'
#' @description Reads in a SQL table from the FWS IRIS Data Warehouse as a tibble
#'
#' @param region A character string representing the legacy FWS region of interest (e.g., Alaska = "FF07")
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

  con <- dbConnect(odbc(),
                   Driver = "ODBC Driver 17 for SQL Server",
                   Server = "ifw9ecos-bvdb11.fws.doi.net\\ecos_beta",
                   Database = "IRIS_DataWarehouse",
                   Trusted_Connection = "yes",
                   Port = 1433)

  df <- query_iris(con, schema, tbl_name)


}
