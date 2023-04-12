
#Import libraries

library(httr)
library(glue)
library(rrapply)
library(tidyr)

dir.create("raw_data", recursive = TRUE, showWarnings = FALSE)

#### Define functions ####

allMerge <- function(x, y) merge(x, y, all = TRUE)

nullToNA <- function(x){
  if (is.null(x)) NA else x
}

#### 

START_YEAR = 2015
END_YEAR = 2022

weather_stations = read.csv(text = 
"city,station
Annecy,FR33203
Lyon,FR20062"
)

pollutants = read.csv(text = 
"id,pollutant
03,dioxyde_azote
24,PM10
02,monoxyde_azote",
colClasses = "character")

years <- data.frame(year = START_YEAR:END_YEAR)

pollute_map <- Reduce(allMerge, list(weather_stations, pollutants, years))

pollute_list <- vector(mode = "list", nrow(pollute_map))

for(pollute_index in seq_len(nrow(pollute_map))){
  
  
  pollute_list[[pollute_index]] <- 
    with(pollute_map[pollute_index,],{
      
      message(glue("[{Sys.time()}] Station: {station} | Pollutant: {pollutant} | Year : {year}"))
      
      url <- glue("https://www.atmo-auvergnerhonealpes.fr/dataviz/dataviz/mesures/{station}/{id}/{year}-01-01/{year}-12-31")
      
      resp_list <- content(GET(url))
      resp_list <- rrapply(resp_list,is.null, \(x) NA)
      df_list <- lapply(resp_list, as.data.frame)
      out <- do.call(rbind, df_list)
      
      
      out$city <- city
      out$station <- station
      out$pollutant_level <- out$pollutant
      out$pollutant_name <- pollutant
      
      out <- out[,c("date", "city", "station", "pollutant_name", "pollutant_level")]
      
      pollute_list[[pollute_index]] <- out
      
      
    }
       
       
       )
  
}

final_data <- Reduce(allMerge, pollute_list)

# convert millisecond timestamp to date
final_data$date <- as.Date(as.POSIXct(final_data$date/1000, origin="1970-01-01"))

final_data %>%
  pivot_wider(id_cols = date:station, names_from = pollutant_name, values_from = pollutant_level) ->
  wide_data


write.csv(wide_data, "raw_data/annecy_airquality.csv", row.names = FALSE)
