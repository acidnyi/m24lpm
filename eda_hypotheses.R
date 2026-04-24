suppressPackageStartupMessages({
  library(tidyverse)
  library(car)
  library(lmtest)
  library(caret)   
})

options(scipen = 999)

# Project data notes:
# - data.csv is whitespace-delimited even though it has a .csv extension
# - the first column in each data row is just the row index, so we treat it as row.names

race_data <- read.table(
  "data.csv",
  header = TRUE,
  row.names = 1,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  quote = "\"",
  comment.char = ""
)

race_data <- race_data %>%
  as_tibble() %>%
  mutate(
    Driver      = as.factor(Driver),
    Compound    = as.factor(Compound),
    FreshTyre   = as.logical(FreshTyre),
    Team        = as.factor(Team),
    yellow_flag = as.logical(yellow_flag)
  )

race_data <- race_data %>%
  mutate(
    top_10 = factor(
      if_else(Position <= 10, "top_10", "not_top_10"),
      levels = c("not_top_10", "top_10")
    )
  )

cat("Dataset shape:", nrow(race_data), "rows x", ncol(race_data), "columns\n\n")


# ═══════════════════════════════════════════════════════════════
# HYPOTHESES
# ═══════════════════════════════════════════════════════════════

# Linear regression hypothesis
# H0: Lap time has no meaningful linear relationship with lap speeds, tyre variables, position, and weather variables.
# H1: Lap time has a meaningful linear relationship with lap speeds, tyre variables, position, and weather variables.

# Classification hypothesis
# "Target: top_10 (whether a lap was completed while the driver was in the top 10 positions)
# "NOTE: Position is excluded from the classifier because top_10 is derived directly from Position.
# "      Including Position would constitute data leakage.
# "NOTE: Team is excluded from the classifier to prevent complete separation.
# "      Team is a near-perfect proxy for Position, causing unreliable coefficients.
# "H0: The selected race and weather variables do not help predict whether the driver is in the top 10.
# "H1: The selected race and weather variables help predict whether the driver is in the top 10.


# ═══════════════════════════════════════════════════════════════
# EDA — EXPLORATORY DATA ANALYSIS
# ═══════════════════════════════════════════════════════════════

# "Structure
glimpse(race_data)

# Summary statistics
print(summary(race_data))

missing_summary <- tibble(
  column         = names(race_data),
  missing_values = map_int(race_data, ~ sum(is.na(.x))),
  missing_share  = round(missing_values / nrow(race_data), 4)
) %>%
  arrange(desc(missing_values))

# Missing values by column
print(missing_summary)

categorical_summary <- list(
  driver      = count(race_data, Driver,      sort = TRUE),
  compound    = count(race_data, Compound,    sort = TRUE),
  fresh_tyre  = count(race_data, FreshTyre,   sort = TRUE),
  team        = count(race_data, Team,        sort = TRUE),
  yellow_flag = count(race_data, yellow_flag, sort = TRUE),
  top_10      = count(race_data, top_10,      sort = TRUE)
)

# Categorical distributions
print(categorical_summary$driver)
print(categorical_summary$compound)
print(categorical_summary$fresh_tyre)
print(categorical_summary$team)
print(categorical_summary$yellow_flag)
print(categorical_summary$top_10)

numeric_correlation <- race_data %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")

# Correlation with LapTime
lap_time_correlations <- tibble(
  variable    = rownames(numeric_correlation),
  correlation = numeric_correlation[, "LapTime"]
) %>%
  filter(variable != "LapTime") %>%
  arrange(desc(abs(correlation)))
print(lap_time_correlations)

lap_time_plot <- ggplot(race_data, aes(x = LapTime)) +
  geom_histogram(bins = 30, fill = "#2A6F97", color = "white") +
  labs(title = "Distribution of Lap Time",
       x = "Lap Time (seconds)", y = "Count") +
  theme_minimal(base_size = 12)
print(lap_time_plot)

compound_plot <- ggplot(race_data, aes(x = Compound, y = LapTime, fill = Compound)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4) +
  labs(title = "Lap Time by Tyre Compound",
       x = "Compound", y = "Lap Time (seconds)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")
print(compound_plot)

speed_plot <- ggplot(race_data, aes(x = SpeedI2, y = LapTime, color = Compound)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 0.9) +
  labs(title = "Lap Time vs SpeedI2",
       x = "SpeedI2", y = "Lap Time (seconds)") +
  theme_minimal(base_size = 12)
print(speed_plot)


# ═══════════════════════════════════════════════════════════════
# HYPOTHESIS TESTING — SIMPLE PRE-MODEL TESTS
# ═══════════════════════════════════════════════════════════════

# ── 1. T-TEST: FreshTyre vs LapTime ──────────────────────────
# T-test: LapTime by FreshTyre
# H0: No difference in mean LapTime between fresh and used tyres
# H1: Mean LapTime differs

t_test_fresh <- t.test(LapTime ~ FreshTyre, data = race_data)
print(t_test_fresh)

if (t_test_fresh$p.value < 0.05) {
  cat("Conclusion: Reject H0 — tyre freshness significantly affects LapTime.\n\n")
} else {
  cat("Conclusion: Fail to reject H0 — no significant difference.\n\n")
}


# ── 2. ANOVA: Compound vs LapTime ────────────────────────────
# ANOVA: LapTime by Compound
# H0: All tyre compounds have equal mean LapTime
# H1: At least one compound differs

anova_model <- aov(LapTime ~ Compound, data = race_data)
anova_result <- summary(anova_model)
print(anova_result)

anova_p <- anova_result[[1]]$`Pr(>F)`[1]

if (anova_p < 0.05) {
  cat("Conclusion: Reject H0 — compound significantly affects LapTime.\n\n")
} else {
  cat("Conclusion: Fail to reject H0 — no significant effect.\n\n")
}


# ── 3. Correlation test: SpeedI2 vs LapTime ──────────────────
# Correlation test: SpeedI2 vs LapTime
# H0: No linear relationship
# H1: Linear relationship exists

cor_test <- cor.test(race_data$SpeedI2, race_data$LapTime)
print(cor_test)

if (cor_test$p.value < 0.05) {
  cat("Conclusion: Reject H0 — SpeedI2 is significantly correlated with LapTime.\n\n")
} else {
  cat("Conclusion: Fail to reject H0 — no significant correlation.\n\n")
}


# ── 4. Chi-square test: top_10 vs Compound ───────────────────
# Chi-square test: top_10 vs Compound
# H0: top_10 is independent of Compound
# H1: top_10 depends on Compound

chi_table <- table(race_data$top_10, race_data$Compound)
chi_test <- chisq.test(chi_table)

print(chi_test)

if (chi_test$p.value < 0.05) {
  cat("Conclusion: Reject H0 — compound is associated with top_10 outcome.\n\n")
} else {
  cat("Conclusion: Fail to reject H0 — no association.\n\n")
}


# ═══════════════════════════════════════════════════════════════
# TRAIN / TEST SPLIT  (80 / 20)
# ═══════════════════════════════════════════════════════════════

set.seed(42)
train_index <- createDataPartition(race_data$LapTime, p = 0.8, list = FALSE)
train_data  <- race_data[train_index, ]
test_data   <- race_data[-train_index, ]

# Train/test split
cat("Train rows:", nrow(train_data), "| Test rows:", nrow(test_data), "\n\n")


# ═══════════════════════════════════════════════════════════════
# LINEAR REGRESSION
# ═══════════════════════════════════════════════════════════════

# LM Pre-1: Independence
# Laps are sequentially ordered within driver stints.
# Strict independence is not met — structural limitation

# LM Pre-3: Multicollinearity — predictor correlation matrix
# Pairs with |r| > 0.8 may indicate multicollinearity.

lm_pred_num <- train_data %>%
  select(LapTime, SpeedI1, SpeedI2, SpeedFL, SpeedST,
         TyreLife, Position, AirTemp, Humidity,
         Pressure, TrackTemp, WindSpeed) %>%
  select_if(is.numeric)

pred_cor       <- cor(lm_pred_num, use = "complete.obs")
high_cor_pairs <- which(abs(pred_cor) > 0.8 & upper.tri(pred_cor), arr.ind = TRUE)

if (nrow(high_cor_pairs) > 0) {
  cat("Pairs with |r| > 0.8:\n")
  for (i in seq_len(nrow(high_cor_pairs))) {
    rn <- rownames(pred_cor)[high_cor_pairs[i, 1]]
    cn <- colnames(pred_cor)[high_cor_pairs[i, 2]]
    cat(sprintf("  %s — %s : r = %.3f\n", rn, cn,
                pred_cor[high_cor_pairs[i, 1], high_cor_pairs[i, 2]]))
  }
} else {
  cat("No predictor pairs exceed |r| = 0.8.\n")
}

# LM Pre-5: Outliers (predictor space — |z| > 3)
lm_num_train  <- train_data %>% select_if(is.numeric)
out_counts_lm <- sapply(lm_num_train, function(x) sum(abs(scale(x)) > 3, na.rm = TRUE))
out_counts_lm <- out_counts_lm[out_counts_lm > 0]
if (length(out_counts_lm) > 0) {
  cat("Columns with z-score outliers:\n")
  print(out_counts_lm)
} else {
  cat("No predictor columns have z-score outliers.\n")
}

# ── FIT LINEAR MODEL ──────────────────────────────────────────
linear_model <- lm(
  LapTime ~ SpeedI1 + SpeedI2 + SpeedFL + SpeedST + Compound +
    TyreLife + FreshTyre + Team + Position + AirTemp +
    Humidity + Pressure + TrackTemp + WindSpeed + yellow_flag,
  data = train_data
)

# Linear regression model summary
print(summary(linear_model))

lm_test_pred <- predict(linear_model, newdata = test_data)
lm_rmse      <- sqrt(mean((test_data$LapTime - lm_test_pred)^2))
lm_r2        <- cor(test_data$LapTime, lm_test_pred)^2

# Linear model test-set performance
cat("RMSE:", round(lm_rmse, 4), "\n")
cat("R-squared (test):", round(lm_r2, 4), "\n\n")


# ═══════════════════════════════════════════════════
# LINEAR REGRESSION — POST-FITTING ASSUMPTION CHECKS
# ═══════════════════════════════════════════════════

linear_diagnostics <- tibble(
  fitted                 = fitted(linear_model),
  residuals              = resid(linear_model),
  standardized_residuals = rstandard(linear_model)
)

# ── LM Post-1: Multicollinearity (VIF) ───────────────────────
# Threshold: VIF > 10 = severe multicollinearity.\n")
vif_lm     <- vif(linear_model)
print(vif_lm)
high_vif_lm <- vif_lm[vif_lm > 10]
if (length(high_vif_lm) > 0) {
  cat("WARNING: predictors with VIF > 10:\n")
  print(high_vif_lm)
  cat("Likely cause: Team correlates strongly with Position.\n")
} else {
  cat("No predictors exceed VIF = 10.\n")
}

# ── LM Post-3: Homoscedasticity ───────────────────────────────

residuals_plot <- ggplot(linear_diagnostics, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, color = "#2A6F97") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.9) +
  labs(title = "Residuals vs Fitted Values",
       subtitle = "Homoscedasticity check — spread should be constant",
       x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 12)
print(residuals_plot)


# ═══════════════════════════════════════════════════════════════
# LOGISTIC REGRESSION (CLASSIFICATION)
# ═══════════════════════════════════════════════════════════════

# ── GLM Pre-1: Independence ───────────────────────────────────
# Same structural limitation as linear model.
# Laps are sequentially ordered within driver stints.

# ── GLM Pre-3: Multicollinearity (correlation matrix) ────────
glm_pred_num <- train_data %>%
  select(SpeedI1, SpeedI2, SpeedFL, SpeedST, TyreLife,
         AirTemp, Humidity, Pressure, TrackTemp, WindSpeed) %>%
  select_if(is.numeric)

glm_pred_cor  <- cor(glm_pred_num, use = "complete.obs")
high_cor_glm  <- which(abs(glm_pred_cor) > 0.8 & upper.tri(glm_pred_cor), arr.ind = TRUE)
if (nrow(high_cor_glm) > 0) {
  cat("Pairs with |r| > 0.8:\n")
  for (i in seq_len(nrow(high_cor_glm))) {
    rn <- rownames(glm_pred_cor)[high_cor_glm[i, 1]]
    cn <- colnames(glm_pred_cor)[high_cor_glm[i, 2]]
    cat(sprintf("  %s — %s : r = %.3f\n", rn, cn,
                glm_pred_cor[high_cor_glm[i, 1], high_cor_glm[i, 2]]))
  }
} else {
  cat("No predictor pairs exceed |r| = 0.8.\n")
}
# NOTE: Formal VIF computed after fitting.

# ── GLM Pre-5: Outliers in predictors ────────────────────────
glm_num_train  <- train_data %>%
  select(SpeedI1, SpeedI2, SpeedFL, SpeedST, TyreLife,
         AirTemp, Humidity, Pressure, TrackTemp, WindSpeed)
out_counts_glm <- sapply(glm_num_train, function(x) sum(abs(scale(x)) > 3, na.rm = TRUE))
out_counts_glm <- out_counts_glm[out_counts_glm > 0]
if (length(out_counts_glm) > 0) {
  cat("Columns with z-score outliers:\n")
  print(out_counts_glm)
} else {
  cat("No predictor columns have z-score outliers.\n")
}



# ── FIT LOGISTIC MODEL ────────────────────────────────────────
# Team excluded: near-perfect proxy for Position -> complete separation.
# Position excluded: data leakage (top_10 derived from Position).

classification_model <- glm(
  top_10 ~ SpeedI1 + SpeedI2 + SpeedFL + SpeedST + Compound +
    TyreLife + FreshTyre + AirTemp +
    Humidity + Pressure + TrackTemp + WindSpeed + yellow_flag,
  data   = train_data,
  family = binomial()
)

# Classification model summary
print(summary(classification_model))

class_test_prob <- predict(classification_model, newdata = test_data, type = "response")
class_test_pred <- factor(
  if_else(class_test_prob >= 0.5, "top_10", "not_top_10"),
  levels = c("not_top_10", "top_10")
)

# Classification confusion matrix (test set)
cm <- table(Actual = test_data$top_10, Predicted = class_test_pred)
print(cm)

TP <- cm["top_10",     "top_10"]
TN <- cm["not_top_10", "not_top_10"]
FP <- cm["not_top_10", "top_10"]
FN <- cm["top_10",     "not_top_10"]

accuracy  <- (TP + TN) / sum(cm)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1        <- 2 * precision * recall / (precision + recall)

# Test-set classification metrics
cat("Accuracy: ",  round(accuracy,  4), "\n")
cat("Precision:",  round(precision, 4), "\n")
cat("Recall:   ",  round(recall,    4), "\n")
cat("F1 Score: ",  round(f1,        4), "\n\n")


# ═════════════════════════════════════════════════════
# LOGISTIC REGRESSION — POST-FITTING ASSUMPTION CHECKS
# ═════════════════════════════════════════════════════

# ── GLM Post-1: Multicollinearity (VIF formal) ───────────────
vif_glm      <- vif(classification_model)
print(vif_glm)
high_vif_glm <- vif_glm[vif_glm > 10]
if (length(high_vif_glm) > 0) {
  cat("WARNING: VIF > 10 for:", paste(names(high_vif_glm), collapse = ", "), "\n")
} else {
  cat("No predictors exceed VIF = 10. Multicollinearity is acceptable.\n")
}
