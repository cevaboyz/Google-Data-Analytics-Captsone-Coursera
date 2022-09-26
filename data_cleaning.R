library(dplyr)
library(openxlsx)
library(lubridate)
library(hms)
library(chron)





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


average_ride <- mean(times(bike_share$ride_length_correct))



##convert ride_length from diff time to numeric and find average

average_number <- unlist(bike_share$ride_length)

average_number_2 <- as.numeric(average_number)

average_ride <- mean(average_number_2, na.rm = TRUE) / 60

##Average ride in Minutes

print(average_ride)

max_ride <- max(average_number_2, na.rm = TRUE)

max_ride_days <- max_ride / 60

max_ride_days_2 <- max_ride_days / 60

max_ride_days_3 <- max_ride_days_2 / 24

print(max_ride)

max_index <- which.max(average_number_2)


##example of multiple days ride
maximum_ride <- bike_share[1839499,]

#number of days of the maximum bike ride
(unlist(as.numeric(maximum_ride$ride_length / 60) / 60)) / 24


#median of the days


bike_share$numeric_week_days <-
  ifelse(
    bike_share$day_of_the_week == "Monday",
    1,
    ifelse(
      bike_share$day_of_the_week == "Tuesday",
      2,
      ifelse(
        bike_share$day_of_the_week == "Wednesday",
        3,
        ifelse(
          bike_share$day_of_the_week == "Thursday",
          4,
          ifelse(
            bike_share$day_of_the_week == "Friday",
            5,
            ifelse(
              bike_share$day_of_the_week == "Saturday",
              6,
              ifelse(bike_share$day_of_the_week == "Sunday", 7, "Error")
            )
          )
        )
      )
    )
  )


mode <- bike_share$numeric_week_days


Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

print(Mode(mode))

#The result is 6 = Saturday

###TODO
