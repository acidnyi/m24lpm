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

# There are very few observation of the soft tyres
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

# Feature set
# SpeedI2
# TyreLife
# Stint
# SpeedI1
# SpeedFL
# SpeedST
# Compound

# SCENARIO 1
# To compare the models we will use the one feature set:
# SpeedI2
# TyreLife
# Stint
# SpeedI1
# SpeedFL
# Compound 
# SpeedST was removed due to the a lot of outliers (60), unstable results in normality check 

data_sc1 <- data %>% select(SpeedI1, SpeedI2, SpeedFL, TyreLife, Stint, Compound, top10)

# Train/Val/Test sets
library(rsample)

set.seed(13)

data_sc1$top10 <- as.factor(data_sc1$top10)

split <- initial_split(data_sc1, prop = 0.8, strata = top10)

train_val_sc1 <- training(split)
test_sc1 <- testing(split)

set.seed(13)

val_split <- initial_split(train_val_sc1, prop = 0.75, strata = top10)
train_sc1 <- training(val_split)
val_sc1 <- testing(val_split)

# Check the proportions of the top10 in the both sets
train_sc1
summary(train_sc1)

val_sc1
summary(val_sc1)

test_sc1
summary(test_sc1)

# Logistic Regression
# Independence: repeated laps per driver is a limitation of the dataset
# Colinearity: no severe colinearity
# Outliers: ??
# Linearity: some predictors shown non-linear relationship with a target 
# Sample size: 708 lap-level observations with the 6 parameters + dummy encodings of the Stint and Compound
# is enough for fitting a logistic regression

lg_model <- glm(data=train_sc1,
  family = binomial,
  top10 ~ SpeedI2 + TyreLife + Stint + SpeedI1 + SpeedFL + Compound
)

summary(lg_model)

# Feature Importance
# SpeedI2 - very significant 
# TyreLife - significant
# Stint3 - very significant
# Stint4 - big errors, due to the small number of observations
# SpeedI1 - not significant
# SpeedFL - significant
# CompoundMedium - not significant
# CompoundSoft - big errors, due to the small number of observations

# Threshold tuning
thresholds <- seq(0.05, 0.95, by=0.05)

lg_val_probs <- predict(lg_model, newdata = val_sc1, type="response")

results <- lapply(thresholds, function(t) {
  
  pred <- ifelse(lg_val_probs >= t, TRUE, FALSE)
  actual <- val_sc1$top10
  
  TP <- sum(pred == TRUE & actual == TRUE)
  TN <- sum(pred == FALSE & actual == FALSE)
  FP <- sum(pred == TRUE & actual == FALSE)
  FN <- sum(pred == FALSE & actual == TRUE)
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  data.frame(
    threshold = t,
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = (sensitivity + specificity) / 2
  )
})

results <- bind_rows(results)

results %>% arrange(desc(balanced_accuracy))

# Testing

lg_probs <- predict(lg_model, newdata = test_sc1, type = "response")

# use the threshold based on the threshold tuning
lg_preds <- ifelse(lg_probs > 0.6, T, F)

# use the standard threshold of 0.5 for comparison
lg_st_preds <- ifelse(lg_probs > 0.5, T, F)

library(caret)

confusionMatrix(factor(lg_preds), factor(test_sc1$top10), positive = "TRUE")

confusionMatrix(factor(lg_st_preds), factor(test_sc1$top10), positive = "TRUE")

# However the 0.65 threshold showing a better specificity and balanced accuracy,
# as mentioned before finding laps in top 10 is more important, so sensitivity playing a crucial role, and 
# 0.5 threshold shows 92% against 84% of 0.6 threshold. So, the threshold of 0.5 is more suitable for the 
# our goal.

# Random Forest
# Independence: repeated laps per driver is a limitation of the dataset
# Linearity: some predictors shown non-linear relationship with a target 
# Sample size: 708 lap-level observations with the 6 parameters + dummy encodings of the Stint and Compound
# must be enough for fitting a random forest

library(randomForest)
set.seed(13)

# Parameter tuning
# for the random forest in our case will be enough to tune only the 
# ntree, mtry, nodesize
# maxnodes are additional control of the tree depth, but our dataset is not
# that big and there are not many parametrs, so nodesize must be enough

rf_grid <- expand.grid(
  ntree = seq(100, 500, 100),
  mtry = seq(2, 5, 1),
  nodesize = seq(3, 10, 1)
)

rf_results <- lapply(1:nrow(rf_grid), function(i) {
  # to ensure reproducebility, alongside randomness during model building
  set.seed(13 + i)
  
  model <- randomForest(
    top10 ~ SpeedI2 + TyreLife + Stint + SpeedI1 + SpeedFL + Compound,
    data = train_sc1,
    ntree = rf_grid$ntree[i],
    mtry = rf_grid$mtry[i],
    nodesize = rf_grid$nodesize[i],
    replace = TRUE,
    importance = TRUE
  )
  
  pred <- predict(model, newdata = val_sc1)
  actual <- val_sc1$top10
  
  TP <- sum(pred == TRUE & actual == TRUE)
  TN <- sum(pred == FALSE & actual == FALSE)
  FP <- sum(pred == TRUE & actual == FALSE)
  FN <- sum(pred == FALSE & actual == TRUE)
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  data.frame(
    ntree = rf_grid$ntree[i],
    mtry = rf_grid$mtry[i],
    nodesize = rf_grid$nodesize[i],
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = (sensitivity + specificity) / 2
  )
})

rf_results <- bind_rows(rf_results)

rf_results %>%
  arrange(desc(balanced_accuracy))

# find the number of the row with the best accuracy to get the number of the seed
# in our situation its 16
rf_results

# Testing

set.seed(13 + 16)

rf_model <- randomForest(
  top10 ~ SpeedI2 + TyreLife + Stint + SpeedI1 + SpeedFL + Compound,
  data = train_sc1,
  ntree = 100,
  mtry = 5,
  nodesize = 3,
  replace = TRUE,
  importance = TRUE
)

rf_model$importance

# Feature Importance
# SpeedI2 - very significant
# TyreLife - significant
# Stint - significant
# SpeedI1 - significant
# SpeedFL - very significant
# Compound - not significant

rf_preds <- predict(rf_model, newdata = test_sc1)

confusionMatrix(rf_preds, test_sc1$top10, positive = "TRUE")

# SVM
# Independence: repeated laps per driver is a limitation of the dataset
# Colinearity: no severe colinearity
# Outliers: ??
# Feature scaling: numeric features will be scaled
# Linearity: some predictors shown non-linear relationship with a target 
# Sample size: 708 lap-level observations with the 6 parameters + dummy encodings of the Stint and Compound
# is enough for fitting a svm

# Feature scaling

num_features <- c("SpeedI2", "SpeedI1", "SpeedFL", "TyreLife")

train_means <- train_sc1 %>%
  summarise(
    across(
      where(is.numeric),
      mean,
      na.rm = T
    )
  )

train_sds <- train_sc1 %>%
  summarise(
    across(
      where(is.numeric),
      sd,
      na.rm = T
    )
  )

scale <- function(data, means, sds, features) {
  scaled <- data
  
  for (f in features) {
    scaled[[f]] <- (data[[f]] - means[[f]]) / sds[[f]]
  }
  
  scaled
}

# scale all sets based on the train set, to prevent leakage
train_svm <- scale(train_sc1, train_means, train_sds, num_features)
val_svm <- scale(val_sc1, train_means, train_sds, num_features)
test_svm <- scale(test_sc1, train_means, train_sds, num_features)

# Parameter Tuning
library(e1071)
set.seed(13)

svm_grid <- expand.grid(
  cost = seq(0.5, 10, 0.5),
  gamma = seq(0.005, 1, 0.05)
)

svm_grid

svm_results <- lapply(1:nrow(svm_grid), function(i) {

   model <- svm(
    top10 ~ SpeedI2 + TyreLife + Stint + SpeedI1 + SpeedFL + Compound,
    data = train_svm,
    kernel = "radial",
    cost = svm_grid$cost[i],
    gamma = svm_grid$gamma[i],
  )
  
  pred <- predict(model, newdata = val_svm)
  actual <- val_svm$top10
  
  TP <- sum(pred == TRUE & actual == TRUE)
  TN <- sum(pred == FALSE & actual == FALSE)
  FP <- sum(pred == TRUE & actual == FALSE)
  FN <- sum(pred == FALSE & actual == TRUE)
  
  sensitivity <- TP / (TP + FN)
  specificity <- TN / (TN + FP)
  
  data.frame(
    cost = svm_grid$cost[i],
    gamma = svm_grid$gamma[i],
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = (sensitivity + specificity) / 2
  )
})

summary(svm_model)

svm_results <- bind_rows(svm_results)

svm_results %>%
  arrange(desc(balanced_accuracy))

# Testing
svm_model <- svm(
  top10 ~ SpeedI2 + TyreLife + Stint + SpeedI1 + SpeedFL + Compound,
  data = train_svm,
  kernel = "radial",
  cost = 9,
  gamma = 0.955,
  probability = TRUE
)

# No check of the Feature Importance due to the radial kernel chosen
# these option doesn't provide a clear data about the significance of the features

svm_preds <- predict(svm_model, newdata = val_svm)

confusionMatrix(svm_preds, val_svm$top10, positive = "TRUE")

# Model comparison
# ROC

lg_probs

rf_probs <- predict(rf_model, newdata = test_sc1, type="prob")[, "TRUE"]
rf_probs

svm_probs <- predict(svm_model, newdata = test_svm, probability = TRUE)
svm_probs <- attr(svm_probs, "probabilities")[, "TRUE"]
svm_probs

library(pROC)

roc_lg <- roc(test_sc1$top10, lg_probs)
roc_rf <- roc(test_sc1$top10, rf_probs)
roc_svm <- roc(test_svm$top10, svm_probs)

auc(roc_lg)
auc(roc_rf)
auc(roc_svm)

roc_df <- bind_rows(
  data.frame(
    specificity = roc_lg$specificities,
    sensitivity = roc_lg$sensitivities,
    model = "Logistic Regression"
  ),
  data.frame(
    specificity = roc_rf$specificities,
    sensitivity = roc_rf$sensitivities,
    model = "Random Forest"
  ),
  data.frame(
    specificity = roc_svm$specificities,
    sensitivity = roc_svm$sensitivities,
    model = "SVM"
  )
) %>%
  mutate(
    false_positive_rate = 1 - specificity
  )

ggplot(roc_df, aes(x = false_positive_rate, y = sensitivity, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_abline(linetype = "dashed") +
  labs(
    title = "ROC Curves for Classification Models",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)",
    color = "Model"
  ) 

# Scenario 3

# Use the Stepwise Logistic Regression for algorithmic feature selection
# based on the results we should remove the SpeedI1
# intersting that Compound stayed, however being not significant, the possible reason
# is improved results in combination with other features
# Important note: these feature selection method is based on the logistic regression
# so with random forest may not work good
step_model <- step(
  lg_model,
  direction = "both",
  trace=TRUE
)

# Logistic Regression

# Full set of features

lg_probs <- predict(lg_model, newdata = test_sc1, type = "response")

roc_lg <- roc(test_sc1$top10, lg_probs)

lg_preds <- ifelse(lg_probs > 0.5, T, F)

# Metrics
confusionMatrix(factor(lg_preds), factor(test_sc1$top10), positive = "TRUE")
AIC(lg_model)
auc(roc_lg)

# Reduced by the stepwise
# without SpeedI1

lg_step_model <- glm(data=train_sc1,
                family = binomial,
                top10 ~ SpeedI2 + TyreLife + Stint + SpeedFL + Compound
)

lg_step_probs <- predict(lg_step_model, newdata = test_sc1, type="response")

roc_step_lg <- roc(test_sc1$top10, lg_step_probs)

lg_step_preds <- ifelse(lg_step_probs > 0.5, T, F)

confusionMatrix(factor(lg_step_preds), factor(test_sc1$top10), positive ="TRUE")
AIC(lg_step_model)
auc(roc_step_lg)

# Reduce by the significance of the features
# The full model, insignificant:
# SpeedI1, CompoundSoft, CompoundMedium and Stint4
summary(lg_model)

# Results on the model without SpeedI1, insignificant:
# Stint4, CompoundMedium, CompoundSoft
# Remove the Compound on this step, because Stint and Compound are factors,
# but Stint3 is significant, while both Compounds are insignificant
summary(lg_step_model)

lg_reduced_model <- glm(data=train_sc1,
                        family = binomial,
                        top10 ~ SpeedI2 + TyreLife + Stint + SpeedFL
)

summary(lg_reduced_model)

lg_reduced_probs <- predict(lg_reduced_model, newdata = test_sc1, type="response")

roc_reduced_lg <- roc(test_sc1$top10, lg_reduced_probs)

lg_reduced_preds <- ifelse(lg_reduced_probs > 0.5, T, F)

confusionMatrix(factor(lg_reduced_preds), factor(test_sc1$top10), positive ="TRUE")
AIC(lg_reduced_model)
auc(roc_reduced_lg)

# Random Forest

# Full set of features

rf_preds <- predict(rf_model, newdata = test_sc1)

confusionMatrix(rf_preds, test_sc1$top10, positive = "TRUE")

# RFE 
x_train <- train_sc1 %>% select(-top10)
y_train <- train_sc1$top10

ctrl <- rfeControl(
  functions = rfFuncs,
  method = "cv",
  number = 5
)

set.seed(29)
rfe_model <- rfe(
  x = x_train,
  y = y_train,
  sizes = c(2,3,4,5,6),
  ntree = 100,
  mtry = 5,
  nodesize = 3,
  rfeControl = ctrl
)

rfe_model
# RFE keep all predictors
predictors(rfe_model)

# Remove the least significant predictor
# least improtant predictor is Compound, let's remove it as model shows
# and see the resutl
rf_model$importance

rf_reduced_model <- randomForest(
  top10 ~ SpeedI2 + TyreLife + Stint + SpeedI1 + SpeedFL,
  data = train_sc1,
  ntree = 100,
  mtry = 5,
  nodesize = 3,
  replace = TRUE,
  importance = TRUE
)


rf_reduced_preds <- predict(rf_reduced_model, newdata = test_sc1)

confusionMatrix(rf_reduced_preds, test_sc1$top10, positive = "TRUE")

# After the removing of the Compound model become worse.

# Save models for shiny app
saveRDS(lg_model, "models/lg_model.rds")
saveRDS(lg_step_model, "models/lg_step_model.rds")
saveRDS(lg_reduced_model, "models/lg_reduced_model.rds")
saveRDS(rf_model, "models/rf_model.rds")
saveRDS(rf_reduced_model, "models/rf_reduced_model.rds")

saveRDS(svm_model, "models/svm_model.rds")
saveRDS(train_means, "models/train_means.rds")
saveRDS(train_sds, "models/train_sds.rds")

data
