
## Create and save cost code info:
cost_codes <- data.frame(
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
save(cost_codes, file = "./data/cost_codes.RData")


FF07RAM000 <- "Alaska Maritime"
FF07RARC00 <- "Arctic"
FF07RAPB00 <- "Becharof"
FF07RINN00 <- "Innoko"
FF07RIZM00 <- "Izembek"
FF07RKAN00 <- "Kanuti"
FF07RKNA00 <- "Kenai"
FF07RKDK00 <- "Kodiak"
FF07RKUK00 <- "Koyukuk"
FF07RKUN00 <- "Nowitna"
FF07RSWK00 <- "Selawik"
FF07RTET00 <- "Tetlin"
FF07RTGK00 <- "Togiak"
FF07RYKD00 <- "Yukon Delta"
FF07RYKF00 <- "Yukon Flats"


#primr_summarize


#----
# The problem is with: 
https://ecos.fws.gov/primr/api/survey/FF07RKAN00-020
#----


source("code/functions/primr_get.R")
load("./data/cost_codes.RData")

# Loop through each refuge:
survey_data <- apply(refuges, 1, function(x) 
  primr_get(cost_code = x[2], status = "Current"))

# Convert jsons to dataframes (something is wrong with Kanuti)
survey_data_df <- lapply(survey_data, function(x) fromJSON(x, simplifyVector = TRUE)$data)

test <- primr_get(cost_code = "FF07RKAN00", status = "Current")



json <- do.call(rbind, 
                lapply(paste(readLines(test, warn=FALSE),
                             collapse=""), 
                       jsonlite::fromJSON))

foo <- fromJSON(test, flatten = TRUE)
