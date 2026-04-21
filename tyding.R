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
# Due to the problems with the linear regression during if we split this feature into 3 variables
# We decided to keep the Compound type as factor, so R can split them when using the model

laps %>% select(Compound) %>%
  unique()

laps$Compound <- as.factor(laps$Compound)

summary(laps)

# TRACK STATUS
# Here the digit is representing the status of the track during the lap,
# for example in the code: 1254:
# 1 - green flag
# 2 - yellow flag
# 4 - safety car
# 5 - virtual safety car
# Digit 3 is a red flag, but we don't have there any red flag during 

laps %>% select(TrackStatus) %>% 
  unique()

# make track status character for the easier work
laps %<>% select(everything()) %>%
  mutate(
    TrackStatus = as.character(TrackStatus)
  ) 

help(str_detect)

laps %<>% select(everything()) %>%
  mutate(
    green_flag = str_detect(TrackStatus, "1"),
    yellow_flag = str_detect(TrackStatus, "2"),
    safety_car = str_detect(TrackStatus, "4"),
    virtual_safety_car = str_detect(TrackStatus, "5"),
  ) 

laps$TrackStatus <- NULL

summary(laps)

# green flag is always true, so it's not bringing any new information
laps$green_flag <- NULL

summary(laps)

# Is Accurate 
# there are 45 of 1228 (3.6 %) of the inaccurate rows in the dataset
# due to the accurate data is crucial for the right prediction, we decided to remove the inaccurate rows

laps %<>% select(everything()) %>%
  filter(IsAccurate != F)

laps$IsAccurate <- NULL

# After removing the inaccurate data, the laps with the pitstops, vsc and safery car  were removed also,
# now they have only NA, so we need to remove it too
summary(laps)  
  
laps$PitOutTime <- NULL
laps$PitInTime <- NULL
laps$virtual_safety_car <- NULL
laps$safety_car <- NULL

summary(laps)
glimpse(laps)

# Sectors 
# Sector1Time + Sector2Time + Sector3Time = true lap time, so target leakage
# SectorXSessionTime not providing the direct explanatory value, beyond the variables as 
# lap number and tyre life

laps %<>% select(!contains("Sector"))

summary(laps)

# LapStartDate
# not providing a meaningful information to the model
# LapStartTime will be used for joining the weather with the laps 

laps$LapStartDate <- NULL

summary(laps)

# IsPersonalBest
# providing information about how this lap was finished, which can affect the models prediction
laps$IsPersonalBest <- NULL

summary(laps)

# Driver and Team
# we need to convert them into factors

laps$Driver <- as.factor(laps$Driver)
laps$Team <- as.factor(laps$Team)

# Driver Number not providing additional info - remove it
laps$DriverNumber <- NULL

summary(laps)

# COMBINING LAPS WITH WEATHER
weather
laps

range(weather$Time)
range(laps$LapStartTime)
range(laps$Time)

