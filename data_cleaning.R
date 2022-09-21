library(dplyr)
library(openxlsx)
library(lubridate)
library(hms)





getwd()

temp <- list.files(pattern = "*.csv")
myfiles <- lapply(temp, read.csv)


bike_share <- bind_rows(myfiles, .id = "column_label")

bike_share$ride_length <- 0

bike_share$started_at_time <-
  lubridate::ymd_hms(bike_share$started_at)

bike_share$ended_at_time <- lubridate::ymd_hms(bike_share$ended_at)

bike_share$ride_length <-
  bike_share$ended_at_time - bike_share$started_at_time



bike_share$ride_length_formatted <-
  seconds_to_period(bike_share$ride_length)

bike_share$ride_length_f2 <-
  as.POSIXct(bike_share$ride_length_formatted, format = "%H:%M:%S")

bike_share$ride_length_correct <-
  sprintf(
    '%02d:%02d:%02d',
    bike_share$ride_length_formatted@hour,
    minute(bike_share$ride_length_formatted),
    second(bike_share$ride_length_formatted)
  )

bike_share$day_of_the_week <- weekdays(bike_share$started_at_time)


