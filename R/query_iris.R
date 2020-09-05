#' Connect to and query the FWS IRIS Warehouse
#'
#' @description Returns a query of the IRIS Data Warehouse with columns \code{column_size==0} ordered last. \strong{Requires a VPN connection.}
#' This allow the table to be read into R using ODBC/DBI and addresses the "Invalid descriptor error" (https://github.com/r-dbi/odbc/issues/309).
#'
#' @author McCrea Cobb \email{mccrea_cobb@@fws.gov}
#'
#' @param schema The schema of the database in the IRIS Warehouse
#' @param tbl_name The name of the desired table in the IRIS Warehouse
#' @param ... inherited arguments
#'
#' @return A SQL query list returned from \code{dplyr::tbl}
#'
#' @importFrom DBI dbConnect
#' @import odbc
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr tbl
#' @importFrom tidyselect all_of
#' @importFrom dbplyr in_schema
#' @export
#'
#' @examples
#' \dontrun{
#' query_iris("Refuges", "DimSurvey")
#' }

query_iris <- function(schema,
                       tbl_name,
                       ...) {

  con <- DBI::dbConnect(odbc(),
                   Driver = "ODBC Driver 17 for SQL Server",
                   Server = "ifw9ecos-bvdb11.fws.doi.net\\ecos_beta",
                   Database = "IRIS_DataWarehouse",
                   Trusted_Connection = "yes",
                   Port = 1433)

  # grab column names
  all_cols <- odbc::odbcConnectionColumns(con,
                                          name = tbl_name,
                                          schema_name = schema)
  # extract long column names
  long_cols <- all_cols %>%
    dplyr::filter(column_size == 0) %>%
    dplyr::pull(name)
  # extract the remaining column names
  other_cols <- all_cols %>%
    dplyr::filter(column_size > 0) %>%
    dplyr::pull(name)
  # put long columns at the end
  ordered_cols <- unique(c(other_cols, long_cols))

  df <- dplyr::tbl(con, dbplyr::in_schema(schema, tbl_name)) %>%
    dplyr::select(tidyselect::all_of(ordered_cols))
}
