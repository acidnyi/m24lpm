rm(list= ls())
getwd()
setwd("git/m24lpm/")

library(tidyverse)
library(magrittr)

# WEATHER DATA:
weather <- read_csv("data/weather_raw.csv")
weather

# problem with the Time, Rainfall and WindDirection 
view(weather)
glimpse(weather)

# RAINFALL:
# has no variability, so it would not be useful for the model
weather %>%
  select(Rainfall) %>%
  unique()

weather$Rainfall <- NULL

weather

# WIND DIRECTION:
# the direction is stored in the degrees
# the problem is that 1 and 359 degrees is seemed to be very far away from each other
# however its not.
# So, we added the sin and cos data of the direction to represent it.

# we need to convert the degree into the radian
help(sin)

# call the added columns from the small letter, to easily visually understand where
# is the columns added by us
weather %<>%
  select(everything()) %>%
  mutate(
    windSin = sin(WindDirection * pi / 180),
    windCos = cos(WindDirection * pi / 180),
  ) 

weather

# remove the WindDirection due to the updated style of keeping this data
weather$WindDirection <- NULL

# TIME
# this variable is not in the right format
# so, convert it to the "seconds from the start" format

convert_time <- function(time) {
  # Cut the part on the beginning, as I'm sure that there will be only 0 days
  time <- str_replace(time, "0 days ", "")
  
  time <- str_split(time, ":")
  
  time <- unlist(time)
  
  time <- as.numeric(time)
  
  seconds <- time[3]
  
  seconds <- seconds + time[2] * 60
  
  seconds <- seconds + time[1] * 3600
  
  return(seconds)
}

convert_time(weather$Time[4])

weather %<>%
  select(everything()) %>%
  mutate(
   Time = map_dbl(Time, convert_time)
  )

weather

# check than there are digits after the dot
print(weather$Time[4], digits=10)

glimpse(weather)

summary(weather)

# The weather data is must be tidy now

# LAPS DATA

laps <- read_csv("data/laps_raw.csv")
laps

view(laps)

glimpse(laps)

laps %>% summary

# There are only 4 laps data that were artificially created
# So, we decided remove them
laps %>% select(everything()) %>%
  filter(FastF1Generated == T)

laps %<>% select(everything()) %>%
  filter(FastF1Generated != T)

laps$FastF1Generated <- NULL

laps

# Deleted Laps
# There are 5 laps of 1233 that were deleted due to the rule break out
# To not train the model on the invalid laps, and due to there are small amount of these invalid laps
# we decided to remove them

laps %>% select(Deleted) %>%
  filter(Deleted==T)

laps %<>% select(everything()) %>%
  filter(Deleted == F)

laps$Deleted <- NULL
laps$DeletedReason <- NULL

summary(laps)

# TIME features
# convert them to the seconds
# NOTE: some values are the time in seconds from the start of the Session, not the Race for example Time
laps %<>% select(everything()) %>%
  mutate(
    across(
      contains("Time"),
      ~map_dbl(., convert_time),
      .names = "{.col}"
    )
  )

summary(laps)

# COMPOUND TYPE
# There are 3 types on the tyres
# Lets split them into each column

laps %>% select(Compound) %>%
  unique()

