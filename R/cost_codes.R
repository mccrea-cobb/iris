
#' Get Alaska refuge cost codes
#' @description Creates a dataframe of cost codes for the Alaska Refuges
#' 
#' @param saveit should the returned dataframe be saved
#' @param filedir a connection or the name of the file where the data will be saved 
#' 
#' @return a dataframe of cost codes for Alaska refuges
#' 
#' @export
#'
#' @examples cost_codes()

cost_codes <- function(saveit=FALSE, filedir){
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
