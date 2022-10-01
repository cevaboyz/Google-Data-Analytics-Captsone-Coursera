library(dplyr)
library(openxlsx)
library(lubridate)
library(hms)
library(chron)
library(ggplot2)





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

summary_stat_members_vs_casual <-
  as.data.frame(table(bike_share$member_casual))

total_rides <- sum(summary_stat_members_vs_casual$Freq)

print(total_rides)

casual_share <-
  (summary_stat_members_vs_casual[1, 2] * 100) / total_rides

member_share <-
  (summary_stat_members_vs_casual[2, 2] * 100) / total_rides

print(
  paste0(
    "The total number of rides for the 2021 is: ",
    total_rides,
    " . Of all of these rides the ",
    round(casual_share, 2),
    "% were made by casual users ",
    "and the percentage of members was ",
    round(member_share, 2),
    "%."
  )
)


##Only members

bike_share_members <-
  bike_share %>% filter(member_casual == "member")

##convert ride_length from diff time to numeric and find average member ride

average_number_member <- unlist(bike_share_members$ride_length)

average_number_2_member <- as.numeric(average_number_member)

average_ride_member <-
  mean(average_number_2_member, na.rm = TRUE) / 60


print(paste0(
  "The average ride length for members is: " ,
  round(average_ride_member, 0),
  " minutes"
))

##Only casual

bike_share_casuals <-
  bike_share %>% filter(member_casual == "casual")

##convert ride_length from diff time to numeric and find average member ride

average_number_casual <- unlist(bike_share_casuals$ride_length)

average_number_2_casual <- as.numeric(average_number_casual)

average_ride_casual <-
  mean(average_number_2_casual, na.rm = TRUE) / 60


print(paste0(
  "The average ride length for casuals is: " ,
  round(average_ride_casual, 0),
  " minutes"
))

##

bike_share$numeric_time <- as.numeric(bike_share$ride_length)

##Manual Day

#Monday
bike_share_monday <-
  bike_share %>% filter(day_of_the_week == "Monday")


average_number_monday <- unlist(bike_share_monday$ride_length)

average_number_2_monday <- as.numeric(average_number_monday)

average_ride_monday <-
  mean(average_number_2_monday, na.rm = TRUE) / 60


#Tuesday
bike_share_tuesday <-
  bike_share %>% filter(day_of_the_week == "Tuesday")


average_number_tuesday <- unlist(bike_share_tuesday$ride_length)

average_number_2_tuesday <- as.numeric(average_number_tuesday)

average_ride_tuesday <-
  mean(average_number_2_tuesday, na.rm = TRUE) / 60

#Wednesday

bike_share_wednesday <-
  bike_share %>% filter(day_of_the_week == "Wednesday")


average_number_wednesday <- unlist(bike_share_wednesday$ride_length)

average_number_2_wednesday <- as.numeric(average_number_wednesday)

average_ride_wednesday <-
  mean(average_number_2_wednesday, na.rm = TRUE) / 60


#Thursday

bike_share_thursday <-
  bike_share %>% filter(day_of_the_week == "Thursday")


average_number_thursday <- unlist(bike_share_thursday$ride_length)

average_number_2_thursday <- as.numeric(average_number_thursday)

average_ride_thursday <-
  mean(average_number_2_thursday, na.rm = TRUE) / 60

#Friday

bike_share_friday <-
  bike_share %>% filter(day_of_the_week == "Friday")


average_number_friday <- unlist(bike_share_friday$ride_length)

average_number_2_friday <- as.numeric(average_number_friday)

average_ride_friday <-
  mean(average_number_2_friday, na.rm = TRUE) / 60

#Saturday
bike_share_saturday <-
  bike_share %>% filter(day_of_the_week == "Saturday")


average_number_saturday <- unlist(bike_share_saturday$ride_length)

average_number_2_saturday <- as.numeric(average_number_saturday)

average_ride_saturday <-
  mean(average_number_2_saturday, na.rm = TRUE) / 60


#Sunday

bike_share_sunday <-
  bike_share %>% filter(day_of_the_week == "Sunday")


average_number_sunday <- unlist(bike_share_sunday$ride_length)

average_number_2_sunday <- as.numeric(average_number_sunday)

average_ride_sunday <-
  mean(average_number_2_sunday, na.rm = TRUE) / 60

###


days <-
  c("Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday")

ride_length_by_days <-
  c(
    round(average_ride_monday),
    round(average_ride_tuesday),
    round(average_ride_wednesday),
    round(average_ride_thursday),
    round(average_ride_friday),
    round(average_ride_saturday),
    round(average_ride_sunday)
  )

week_rides_length <- data.frame(days, ride_length_by_days)

print(week_rides_length)


##Number of Rides By Days
monday <- unique(bike_share_monday$ride_id)
monday_u <- unlist(monday)
monday_l <- length(monday_u)


tuesday <- unique(bike_share_tuesday$ride_id)
tuesday_u <- unlist(tuesday)
tuesday_l <- length(tuesday_u)

wednesday <- unique(bike_share_wednesday$ride_id)
wednesday_u <- unlist(wednesday)
wednesday_l <- length(wednesday_u)

thursday <- unique(bike_share_thursday$ride_id)
thursday_u <- unlist(thursday)
thursday_l <- length(thursday_u)

friday <- unique(bike_share_friday$ride_id)
friday_u <- unlist(friday)
friday_l <- length(friday_u)

saturday <- unique(bike_share_saturday$ride_id)
saturday_u <- unlist(saturday)
saturday_l <- length(saturday_u)


sunday <- unique(bike_share_sunday$ride_id)
sunday_u <- unlist(sunday)
sunday_l <- length(sunday_u)

days_n <-
  c("Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday")

number_of_rides_per_day <- c(monday_l, tuesday_l, wednesday_l, thursday_l, friday_l, saturday_l, sunday_l)

number_of_rides_per_day_df <- data.frame(days_n,as.numeric(number_of_rides_per_day))


