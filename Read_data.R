
#Import libraries

library(httr)
library(glue)

dir.create("raw_data", recursive = TRUE, showWarnings = FALSE)


WEATHER_STATION = "FR33203"
START_DATE = "2020-01-01"
END_DATE = "2021-12-31"

pollute_map <- c("03" = "dioxyde_azote",
                 "24" = "PM10",
                 "02" = "monoxyde_azote"
                 )

pollute_list <- vector(mode = "list", length(pollute_map))

for(pollute_id in seq_along(pollute_map)){
  
  pollute_num <- names(pollute_map)[pollute_id]
  pollute_name <- pollute_map[pollute_id]
  
  url <- glue("https://www.atmo-auvergnerhonealpes.fr/dataviz/dataviz/mesures/{WEATHER_STATION}/{pollute_num}/{START_DATE}/{END_DATE}")
  
  resp_list <- content(GET(url))
  df_list <- lapply(resp_list, as.data.frame)
  out <- do.call(rbind, df_list)
  
  out <- out[,c("date", "pollutant")]
  names(out) <- c("date", pollute_name)
  
  pollute_list[[pollute_id]] <- out
  
}

final_data <- Reduce(\(x, y) merge(x, y, all = TRUE), pollute_list)

final_data$date <- as.Date(as.POSIXct(final_data$date/1000, origin="1970-01-01"))

write.csv(final_data, "raw_data/annecy_airquality.csv", row.names = FALSE)
