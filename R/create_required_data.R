#' Get refuge cost codes
#' @description Creates a data frame of cost codes for refuges in the Alaska Region.
#' These are used for querying station-specific data.
#'
#' @param saveit a logical value indicating whether the the returned data frame should be saved
#' @param filedir a connection or the name of the file where the data will be saved
#'
#' @return a data frame containing refuge names and their associated cost codes for Alaska refuges
#'
#' @examples
#' \dontrun{
#' get_cost_codes(TRUE, "cc.Rdata")
#' }

get_cost_codes <- function(saveit = FALSE,
                       filedir){
  df <- data.frame(
  refuge = c("Alaska Maritime",
             "Arctic",
             "Becharof",
             "Innoko",
             "Izembek",
             "Kanuti",
             "Kenai",
             "Kodiak",
             "Koyukuk",
             "Nowitna",
             "Selawik",
             "Tetlin",
             "Togiak",
             "Yukon Delta",
             "Yukon Flats"),
  ccc = c("FF07RAM000",
          "FF07RARC00",
          "FF07RAPB00",
          "FF07RINN00",
          "FF07RIZM00",
          "FF07RKAN00",
          "FF07RKNA00",
          "FF07RKDK00",
          "FF07RKUK00",
          "FF07RKUN00",
          "FF07RSWK00",
          "FF07RTET00",
          "FF07RTGK00",
          "FF07RYKD00",
          "FF07RYKF00"))

  if(saveit == TRUE) {
  save(df, file = paste0(filedir, ".RData"))
  }

  return(df)
}
