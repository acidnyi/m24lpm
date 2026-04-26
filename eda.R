rm(list= ls())
getwd()
setwd("git/m24lpm/")

library(tidyverse)
library(magrittr)

data <- read_csv("data.csv")
data

glimpse(data)
summary(data)

# Hypothesis:
# Lap and weather features contain sufficient information to distinguish
# whether a driver is in a Top 10 position

# Make the Driver, Team and Compound features as factors
data$Driver <- as.factor(data$Driver)
data$Team <- as.factor(data$Team)
data$Compound <- as.factor(data$Compound)

# Stint is a categorical value
data %>% select(Stint) %>%
  unique()

data$Stint <- as.factor(data$Stint)
# Remove Time and LapStartTime, weather_time columns, for not containing useful information
# for classification, just showing the sequence of the events

data$Time <- NULL
data$LapStartTime <- NULL
data$weather_time <- NULL

# Remove LapTime as a data leakage feature, for clear connection with the top10 
data$LapTime <- NULL

summary(data)

# creating a response variable
data %<>% select(everything()) %>%
  mutate(
    top10 = ifelse(Position <= 10, T, F)
  ) 

# remove the position to prevent data leakage
data$Position <- NULL

# There is a class imbalance, with the 63.48% - laps in the top 10
# and 36.52 % - laps outside the top 10
summary(data)

# EDA

# Independence 
# Observations are not fully independent, due to repeated laps per driver

# Colinearity

data %>% select(where(is.numeric)) %>%
  cor(use = "complete.obs")

# High correlation ( > 0.7):

# LapNumber:
# TyreLife - 0.7674
# Pressure - -0.8855
# TrackTemp - -0.9869

# TyreLife:
# LapNumber - 0.7674
# Pressure - -0.7078
# TrackTemp - -0.7509

# Pressure:
# LapTime - -0.8855
# TyreLife - -0.7078
# TrackTemp - 0.8612

# TrackTemp:
# LapNumber - -0.9869
# TyreLife - -0.7509
# Pressure - 0.8612

# Correlation with the TyreLife is logically, due to the
# number of the laps and the temperature of the track is clearly connected with the 
# tyres degradation. Besides, TyreLife containing important information about the preformance on this lap.

# The biggest correaltions between:
# LapNumber, Pressure and TrackTemp
# We'll manage this by keeping only the one feature from this set,
# LapNumber is just a laps counter, while Pressure and TrackTemp providing physical information about the track.
# How ever the Pressure feature is only in range from 1018 to 1019, what making this feature mostly constant, while
# the range of the TrackTemp is from 40.50 to 49.20, so the TrackTemp was chosen as the representator.
summary(data)

data$LapNumber <- NULL
data$Pressure <- NULL

summary(data)

# Normality
# For most models, except of the LDA the normality is not required, so let's do a quick check on the Speed data.
# The deeper normality analysis will be done, if the LDA model will be used.

data %>% select(where(is.numeric)) %>% colnames()

# SpeedI1

ggplot(data, aes(x = SpeedI1)) +
  geom_histogram(bins=30, fill = "red", color="black")

ggplot(data, aes( sample = SpeedI1)) +
  stat_qq(color = "red") +
  stat_qq_line()

# Due to the big number of the observations may not show the correct result
shapiro.test(data$SpeedI1)

# SpeedI2

ggplot(data, aes(x = SpeedI2)) +
  geom_histogram(bins=30, fill = "blue", color="black")

ggplot(data, aes( sample = SpeedI2)) +
  stat_qq(color = "blue") +
  stat_qq_line()

# Due to the big number of the observations may not show the correct result
shapiro.test(data$SpeedI2)


# SpeedFL

ggplot(data, aes(x = SpeedFL)) +
  geom_histogram(bins=30, fill = "yellow", color="black")

ggplot(data, aes( sample = SpeedFL)) +
  stat_qq(color = "yellow") +
  stat_qq_line()

# Due to the big number of the observations may not show the correct result
shapiro.test(data$SpeedFL)

# SpeedST

ggplot(data, aes(x = SpeedST)) +
  geom_histogram(bins=30, fill = "green", color="black")

ggplot(data, aes( sample = SpeedST)) +
  stat_qq(color = "green") +
  stat_qq_line()

# Due to the big number of the observations may not show the correct result
shapiro.test(data$SpeedST)

# All speed features are not following the Normal Distribution

# To check if the feature are good for distinguish
#ggplot(data, aes(x = SpeedI1, fill=top10)) +
#  geom_histogram(bins=30, alpha= 0.6, position="identity")

# Feature Scaling
# Features are not of the same scale, but the feature scaling is needed for the SVM and LDA/QDA,
# so features will be scaled if these models will be used.
summary(data)

# Outliers

outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm=T)
  q3 <- quantile(x, 0.75, na.rm=T)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5*iqr
  upper <- q3 + 1.5*iqr
  
  sum(x < lower | x > upper, na.rm = T)
}

data %>% select(where(is.numeric)) %>%
 summarise(
   across(
     everything(),
     outliers
   )
 )

# box plots on the features with the most outliers
ggplot(data, aes(x=SpeedI1, fill = top10)) +
  geom_boxplot()

ggplot(data, aes(x=SpeedFL, fill = top10)) +
  geom_boxplot()

ggplot(data, aes(x=SpeedST, fill = top10)) +
  geom_boxplot()

ggplot(data, aes(x=WindSpeed, fill = top10)) +
  geom_boxplot()

# For now outliers (in most situation low speed) were kept in the dataset, due to no connection
# with a specific condition such as yellow_flag, the outliers are in the both situation when yellow flag is true
# and not, also the pitstops were removed from the dataset, other idea that it could be traffic can't be proved from the data.
# So, we decide to keep them from now, with the possiblity to remove them later and check the model performance, to better understand if they
# contains a meaningful information
ggplot(data, aes(x=SpeedFL, fill = yellow_flag)) +
  geom_boxplot()

ggplot(data, aes(x=SpeedI1, fill = yellow_flag)) +
  geom_boxplot()

ggplot(data, aes(x=SpeedST, fill = yellow_flag)) +
  geom_boxplot()

ggplot(data, aes(x=WindSpeed, fill = yellow_flag)) +
  geom_boxplot()

# Homoscedasticity
# Not required for the most models (required for the LDA/QDA), so if the LDA/QDA model will be chosen there
# will be analysis of this aspect

# Linear Relationship
data %>% select(where(is.numeric)) %>%
  colnames()

# Numeric values

# Not linear
ggplot(data, aes(x = SpeedI1, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# ~ Linear
ggplot(data, aes(x = SpeedI2, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not linear
ggplot(data, aes(x = SpeedFL, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not linear
ggplot(data, aes(x = SpeedST, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Linear
ggplot(data, aes(x = TyreLife, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not useful
ggplot(data, aes(x = AirTemp, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not useful 
ggplot(data, aes(x = Humidity, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not useful
ggplot(data, aes(x = TrackTemp, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not useful
ggplot(data, aes(x = WindSpeed, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not useful
ggplot(data, aes(x = windSin, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Not useful
ggplot(data, aes(x = windCos, y = as.numeric(top10))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", color = "blue")

# Categorical values
data %>% select(!where(is.numeric)) %>%
  colnames()

# Leakage feature - can't be used as predictor
ggplot(data, aes(x = Driver, fill = top10)) +
  geom_bar(position = "fill")

# Maybe depends on the position
ggplot(data, aes(x = Stint, fill = top10)) +
  geom_bar(position = "fill")

# Ok
ggplot(data, aes(x = Compound, fill = top10)) +
  geom_bar(position = "fill")

# Strong dependence on the position
ggplot(data, aes(x = FreshTyre, fill = top10)) +
  geom_bar(position = "fill")

# Leakage 
ggplot(data, aes(x = Team, fill = top10)) +
  geom_bar(position = "fill")

# Not useful
ggplot(data, aes(x = yellow_flag, fill = top10)) +
  geom_bar(position = "fill")
