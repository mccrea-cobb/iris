library("jsonlite")
library("httr")
# send the url request to the application
get_list <- GET("https://ecos.fws.gov/primr/api/survey/list?order=id&ccc=FF06RBNL00")

# convert the json response to a text format
get_text <- content(get_list, "text")

# flatten the json text
get_json <- fromJSON(get_text, flatten = TRUE)

# get the node names example
get_names <- names(get_json$data)

# get the specific data example...bioticGroupLevel2
get_obj <- get_json$data$bioticGroupLevel2[4]

# convert the json to a data frame
get_df <- as.data.frame(get_json)





