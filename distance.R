library(readr)
library(tidyverse)
library(httr)
library(jsonlite)
hosp_coordinates <- read_csv("~/Dropbox/Hospital Problem/hosp_coordinates.csv")
url_part_1 <- "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins="
url_part_2 <- "&destinations="
api_key <- "&key=<insert your api key>"

hosp_coordinates<-hosp_coordinates %>%
  unite(coordinate, c(lat,lon), sep = ",")


get_distance <- function(origin_id , coordinates, distance = T ){
  destination <- coordinates[!coordinates$id == origin_id,]
  origin <- coordinates[coordinates$id == origin_id,]
  url <- paste0(url_part_1,
                origin$coordinate,
                url_part_2,
                paste0(destination$coordinate,collapse = "|"),
                api_key,collapse = "")
    
  response <- GET(url)
  resp_json <- fromJSON(content(response, as="text") )
  resp_df <- resp_json[[3]][[1]][[1]]
  if(distance) return( resp_df[[1]][2]$value )
  else return(resp_df[[2]][2]$value)
}

debug(get_distance)
get_distance(3,hosp_coordinates)


