rm(list= ls())
getwd()
setwd("git/oznal/project/")

library(tidyverse)
library(magrittr)

data <- read_csv("monaco_2024_laps.csv")

glimpse(data)

data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count") %>%
  filter(na_count > 0) %>%
  arrange(desc(na_count)) %>%
  print()

data
data_clean <- data %>%
  filter(
    is.na(pit_in_time),
    is.na(pit_out_time),
    !is.na(lap_time),
    deleted == FALSE | is.na(deleted),
    lap_number > 1,
    track_status == "1" | is.na(track_status)
  )


data_clean <- data_clean %>%
  group_by(driver) %>%
  mutate(
    speed_i1 = coalesce(speed_i1, median(speed_i1, na.rm = TRUE)),
    speed_i2 = coalesce(speed_i2, median(speed_i2, na.rm = TRUE)),
    speed_fl = coalesce(speed_fl, median(speed_fl, na.rm = TRUE)),
    speed_st = coalesce(speed_st, median(speed_st, na.rm = TRUE))
  ) %>%
  ungroup()
  

data_clean %>%
  arrange(driver, lap_number) %>%
  group_by(driver)

data_model <- data_clean %>%
  arrange(driver, lap_number) %>%
  group_by(driver) %>%
  mutate(
    # Stint
    stint = cumsum(lag(tyre_life, default = first(tyre_life)) > tyre_life) + 1,
    stint_lap = row_number(),
    
    # Tire
    tyre_life_sq = tyre_life^2,
    is_fresh_tyre = as.integer(tyre_life <= 3),
    
    # Compound encoding
    compound_soft = as.integer(compound == "SOFT"),
    compound_medium = as.integer(compound == "MEDIUM"),
    compound_hard = as.integer(compound == "HARD"),
    compound_num = case_when(
      compound == "SOFT" ~ 1,
      compound == "MEDIUM" ~ 2,
      compound == "HARD" ~ 3,
      TRUE ~ NA_real_
    ),
    
    # Lap progression
    prev_lap_time = lag(lap_time),
    lap_time_delta = lap_time - prev_lap_time,
    
    # Personal bests
    best_s1_so_far = cummin(sector1_time),
    best_s2_so_far = cummin(sector2_time),
    best_s3_so_far = cummin(sector3_time),
    theoretical_best = best_s1_so_far + best_s2_so_far + best_s3_so_far,
    gap_to_theoretical = lap_time - theoretical_best,
    
    # Position
    position_change = lag(position, default = first(position)) - position,
    is_top_5 = as.integer(position <= 5),
    is_top_10 = as.integer(position <= 10)
  ) %>%
  ungroup() %>%
  mutate(
    # Team/Driver encoding
    team_encoded = as.integer(factor(team)),
    driver_encoded = as.integer(factor(driver)),
    
    # Race progress
    race_progress = lap_number / max(lap_number),
    is_early_race = as.integer(lap_number <= 15),
    is_late_race = as.integer(lap_number >= max(lap_number) - 10),
    
    # Weather
    temp_diff = track_temp - air_temp,
    
    # Interaction
    compound_tyre_interaction = compound_num * tyre_life
  ) %>%
  filter(!is.na(prev_lap_time))

data_model

data_clean

feature_cols <- c(
  "lap_number", "sector1_time", "sector2_time", "sector3_time",
  "speed_i1", "speed_i2", "speed_fl", "speed_st",
  "tyre_life", "tyre_life_sq", "is_fresh_tyre",
  "compound_soft", "compound_medium", "compound_hard", "compound_num",
  "stint", "stint_lap",
  "position", "position_change", "is_top_5", "is_top_10",
  "driver_encoded", "team_encoded",
  "race_progress", "is_early_race", "is_late_race",
  "prev_lap_time", "lap_time_delta",
  "best_s1_so_far", "best_s2_so_far", "best_s3_so_far",
  "theoretical_best", "gap_to_theoretical",
  "air_temp", "track_temp", "humidity", "wind_speed", "temp_diff",
  "compound_tyre_interaction"
)

data_model %>%
  summarise(
    n = n(),
    mean = mean(lap_time),
    sd = sd(lap_time),
    min = min(lap_time),
    median = median(lap_time),
    max = max(lap_time)
  ) %>%
  print()

data_model %>%
  group_by(compound) %>%
  summarise(
    n = n(),
    mean_lap = mean(lap_time),
    sd_lap = sd(lap_time),
    .groups = "drop"
  ) %>%
  arrange(mean_lap) %>%
  print()

data_model %>%
  group_by(team) %>%
  summarise(mean_lap = mean(lap_time), .groups = "drop") %>%
  arrange(mean_lap) %>%
  print()

data_model %>%
  select(where(is.numeric)) %>%
  cor() %>%
  as_tibble(rownames = "var") %>%
  select(var, lap_time) %>%
  filter(var != "lap_time") %>%
  mutate(abs_cor = abs(lap_time)) %>%
  arrange(desc(abs_cor)) %>%
  head(10) %>%
  print()


model_data <- data_model %>%
  select(
    lap_time,
    lap_number, tyre_life, tyre_life_sq, is_fresh_tyre,
    compound_soft, compound_medium,
    stint, stint_lap, position, position_change,
    is_top_5, is_top_10, driver_encoded, team_encoded,
    race_progress, is_early_race, is_late_race,
    prev_lap_time,
    air_temp, track_temp, humidity, wind_speed, temp_diff,
    compound_tyre_interaction
  ) %>%
  drop_na()


set.seed(42)
n <- nrow(model_data)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))

train_data <- model_data %>% slice(train_idx)
test_data <- model_data %>% slice(-train_idx)

lm_model <- lm(lap_time ~ ., data = train_data)

lm_model

lm_model %>%
  broom::tidy() %>%
  mutate(
    estimate = round(estimate, 4),
    std.error = round(std.error, 4),
    statistic = round(statistic, 2),
    p.value = round(p.value, 4)
  ) %>%
  print(n = 30)

lm_model %>%
  broom::glance() %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df, nobs) %>%
  print()

predictions <- test_data %>%
  mutate(
    predicted = predict(lm_model, newdata = test_data),
    residual = lap_time - predicted
  )

predictions

r2 <- predictions %$% cor(lap_time, predicted)^2
rmse <- predictions %$% sqrt(mean(residual^2))
mae <- predictions %$% mean(abs(residual))

cat("R²:", round(r2, 4), "\n")
cat("RMSE:", round(rmse, 4), "seconds\n")
cat("MAE:", round(mae, 4), "seconds\n")

predictions %>%
  summarise(
    mean_resid = mean(residual),
    sd_resid = sd(residual),
    min_resid = min(residual),
    max_resid = max(residual)
  ) %>%
  print()

lm_model %>%
  broom::tidy() %>%
  filter(p.value < 0.05) %>%
  arrange(p.value) %>%
  select(term, estimate, p.value) %>%
  print(n = 20)

p <- predictions %>%
  ggplot(aes(x = lap_time, y = predicted)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Linear Regression: Actual vs Predicted Lap Times",
    subtitle = paste0("R² = ", round(r2, 4), " | RMSE = ", round(rmse, 2), "s"),
    x = "Actual Lap Time (seconds)",
    y = "Predicted Lap Time (seconds)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40")
  )

p

predictions %>% select(lap_time, predicted)
