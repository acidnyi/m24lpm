suppressPackageStartupMessages({
  library(tidyverse)
  library(car)     # vif()
  library(lmtest)  # bptest()
  library(caret)   # createDataPartition()
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

cat("Linear regression hypothesis\n")
cat("H0: Lap time has no meaningful linear relationship with lap speeds, tyre variables, position, and weather variables.\n")
cat("H1: Lap time has a meaningful linear relationship with lap speeds, tyre variables, position, and weather variables.\n\n")

cat("Classification hypothesis\n")
cat("Target: top_10 (whether a lap was completed while the driver was in the top 10 positions)\n")
cat("NOTE: Position is excluded from the classifier because top_10 is derived directly from Position.\n")
cat("      Including Position would constitute data leakage.\n")
cat("NOTE: Team is excluded from the classifier to prevent complete separation.\n")
cat("      Team is a near-perfect proxy for Position, causing unreliable coefficients.\n")
cat("H0: The selected race and weather variables do not help predict whether the driver is in the top 10.\n")
cat("H1: The selected race and weather variables help predict whether the driver is in the top 10.\n\n")


# ═══════════════════════════════════════════════════════════════
# EDA — EXPLORATORY DATA ANALYSIS
# ═══════════════════════════════════════════════════════════════

cat("Structure\n")
glimpse(race_data)
cat("\n")

cat("Summary statistics\n")
print(summary(race_data))
cat("\n")

missing_summary <- tibble(
  column         = names(race_data),
  missing_values = map_int(race_data, ~ sum(is.na(.x))),
  missing_share  = round(missing_values / nrow(race_data), 4)
) %>%
  arrange(desc(missing_values))

cat("Missing values by column\n")
print(missing_summary)
cat("\n")

# yellow_flag has only 15 TRUE values (~1.3%). Its coefficient
# should be interpreted with caution in both models.

categorical_summary <- list(
  driver      = count(race_data, Driver,      sort = TRUE),
  compound    = count(race_data, Compound,    sort = TRUE),
  fresh_tyre  = count(race_data, FreshTyre,   sort = TRUE),
  team        = count(race_data, Team,        sort = TRUE),
  yellow_flag = count(race_data, yellow_flag, sort = TRUE),
  top_10      = count(race_data, top_10,      sort = TRUE)
)

cat("Categorical distributions\n")
print(categorical_summary$driver)
print(categorical_summary$compound)
print(categorical_summary$fresh_tyre)
print(categorical_summary$team)
print(categorical_summary$yellow_flag)
print(categorical_summary$top_10)
cat("\n")

numeric_correlation <- race_data %>%
  select_if(is.numeric) %>%
  cor(use = "complete.obs")

cat("Correlation with LapTime\n")
lap_time_correlations <- tibble(
  variable    = rownames(numeric_correlation),
  correlation = numeric_correlation[, "LapTime"]
) %>%
  filter(variable != "LapTime") %>%
  arrange(desc(abs(correlation)))
print(lap_time_correlations)
cat("\n")

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

track_temp_plot <- ggplot(race_data, aes(x = TrackTemp, fill = top_10)) +
  geom_density(alpha = 0.45) +
  labs(title = "Track Temperature by Top 10 vs Not Top 10",
       x = "Track Temperature", y = "Density") +
  theme_minimal(base_size = 12)
print(track_temp_plot)


# ═══════════════════════════════════════════════════════════════
# HYPOTHESIS TESTING — SIMPLE PRE-MODEL TESTS
# ═══════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════\n")
cat("HYPOTHESIS TESTING — SIMPLE TESTS (PRE-MODEL)\n")
cat("══════════════════════════════════════════════════════\n\n")


# ── 1. T-TEST: FreshTyre vs LapTime ──────────────────────────
cat("T-test: LapTime by FreshTyre\n")
cat("H0: No difference in mean LapTime between fresh and used tyres\n")
cat("H1: Mean LapTime differs\n")

t_test_fresh <- t.test(LapTime ~ FreshTyre, data = race_data)
print(t_test_fresh)

if (t_test_fresh$p.value < 0.05) {
  cat("Conclusion: Reject H0 — tyre freshness significantly affects LapTime.\n\n")
} else {
  cat("Conclusion: Fail to reject H0 — no significant difference.\n\n")
}


# ── 2. ANOVA: Compound vs LapTime ────────────────────────────
cat("ANOVA: LapTime by Compound\n")
cat("H0: All tyre compounds have equal mean LapTime\n")
cat("H1: At least one compound differs\n")

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
cat("Correlation test: SpeedI2 vs LapTime\n")
cat("H0: No linear relationship\n")
cat("H1: Linear relationship exists\n")

cor_test <- cor.test(race_data$SpeedI2, race_data$LapTime)
print(cor_test)

if (cor_test$p.value < 0.05) {
  cat("Conclusion: Reject H0 — SpeedI2 is significantly correlated with LapTime.\n\n")
} else {
  cat("Conclusion: Fail to reject H0 — no significant correlation.\n\n")
}


# ── 4. Chi-square test: top_10 vs Compound ───────────────────
cat("Chi-square test: top_10 vs Compound\n")
cat("H0: top_10 is independent of Compound\n")
cat("H1: top_10 depends on Compound\n")

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
# Done before assumption checks so that VIF and outlier checks
# are computed on training data only — no test leakage.
# ═══════════════════════════════════════════════════════════════

set.seed(42)
train_index <- createDataPartition(race_data$LapTime, p = 0.8, list = FALSE)
train_data  <- race_data[train_index, ]
test_data   <- race_data[-train_index, ]

cat("Train/test split\n")
cat("Train rows:", nrow(train_data), "| Test rows:", nrow(test_data), "\n\n")


# ═══════════════════════════════════════════════════════════════
# LINEAR REGRESSION
#
# Correct ordering of assumption checks (slide 17):
#
#  BEFORE fitting — checked on training data:
#    Pre-1  Independence        (data structure)
#    Pre-2  Normality           (not required — slide 17 green)
#    Pre-3  Multicollinearity   (predictor correlation matrix)
#    Pre-4  Feature Scaling     (not required — slide 17)
#    Pre-5  Outliers            (z-scores of raw predictors)
#    Pre-6  Sample Size         (obs-per-predictor ratio)
#
#  AFTER fitting — require model residuals:
#    Post-1  Multicollinearity formal  (VIF on fitted model)
#    Post-2  Outliers in residuals     (studentized residuals)
#    Post-3  Homoscedasticity          (Breusch-Pagan + plot)
#    Post-4  Normality of residuals    (Q-Q, reference only)
# ═══════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════\n")
cat("LINEAR REGRESSION — PRE-FITTING ASSUMPTION CHECKS\n")
cat("══════════════════════════════════════════════════\n\n")

# ── LM Pre-1: Independence ────────────────────────────────────
cat("LM Pre-1: Independence\n")
cat("Laps are sequentially ordered within driver stints.\n")
cat("Strict independence is not met — structural limitation\n")
cat("of lap-level race data; must be acknowledged.\n\n")

# ── LM Pre-2: Normality ───────────────────────────────────────
cat("LM Pre-2: Normality of predictors\n")
cat("Not required for linear regression (slide 17 — green).\n\n")

# ── LM Pre-3: Multicollinearity (correlation matrix) ─────────
cat("LM Pre-3: Multicollinearity — predictor correlation matrix\n")
cat("Pairs with |r| > 0.8 may indicate multicollinearity.\n")

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
cat("NOTE: Formal VIF computed after fitting.\n\n")

# ── LM Pre-4: Feature Scaling ─────────────────────────────────
cat("LM Pre-4: Feature Scaling\n")
cat("Not required for linear regression (slide 17).\n\n")

# ── LM Pre-5: Outliers in predictors ─────────────────────────
cat("LM Pre-5: Outliers (predictor space — |z| > 3)\n")
lm_num_train  <- train_data %>% select_if(is.numeric)
out_counts_lm <- sapply(lm_num_train, function(x) sum(abs(scale(x)) > 3, na.rm = TRUE))
out_counts_lm <- out_counts_lm[out_counts_lm > 0]
if (length(out_counts_lm) > 0) {
  cat("Columns with z-score outliers:\n")
  print(out_counts_lm)
} else {
  cat("No predictor columns have z-score outliers.\n")
}
cat("\n")

# ── LM Pre-6: Sample Size ─────────────────────────────────────
# Planned dummy-expanded predictors:
#   SpeedI1, SpeedI2, SpeedFL, SpeedST          (4)
#   Compound (3 levels → 2 dummies)             (2)
#   TyreLife, FreshTyre                         (2)
#   Team (9 levels → 8 dummies)                 (8)
#   Position                                    (1)
#   AirTemp, Humidity, Pressure, TrackTemp,
#   WindSpeed, yellow_flag                      (6)
#   Total                                      (23)
lm_n_pred  <- 23
lm_n_train <- nrow(train_data)
lm_ratio   <- floor(lm_n_train / lm_n_pred)

cat("LM Pre-6: Sample Size\n")
cat("Planned predictors (incl. dummies):", lm_n_pred, "\n")
cat("Training observations:", lm_n_train, "\n")
cat("Observations-per-predictor ratio:", lm_ratio, "\n")
cat("Rule of thumb: >= 10-20 obs per predictor.\n")
if (lm_ratio >= 10) {
  cat("Sample size is adequate.\n\n")
} else {
  cat("WARNING: sample size may be insufficient.\n\n")
}


# ── FIT LINEAR MODEL ──────────────────────────────────────────
linear_model <- lm(
  LapTime ~ SpeedI1 + SpeedI2 + SpeedFL + SpeedST + Compound +
    TyreLife + FreshTyre + Team + Position + AirTemp +
    Humidity + Pressure + TrackTemp + WindSpeed + yellow_flag,
  data = train_data
)

cat("Linear regression model summary\n")
print(summary(linear_model))
cat("\n")

lm_test_pred <- predict(linear_model, newdata = test_data)
lm_rmse      <- sqrt(mean((test_data$LapTime - lm_test_pred)^2))
lm_r2        <- cor(test_data$LapTime, lm_test_pred)^2

cat("Linear model test-set performance\n")
cat("RMSE:", round(lm_rmse, 4), "\n")
cat("R-squared (test):", round(lm_r2, 4), "\n\n")


cat("═══════════════════════════════════════════════════\n")
cat("LINEAR REGRESSION — POST-FITTING ASSUMPTION CHECKS\n")
cat("═══════════════════════════════════════════════════\n\n")

linear_diagnostics <- tibble(
  fitted                 = fitted(linear_model),
  residuals              = resid(linear_model),
  standardized_residuals = rstandard(linear_model)
)

# ── LM Post-1: Multicollinearity (VIF) ───────────────────────
cat("LM Post-1: Multicollinearity — VIF (formal)\n")
cat("Threshold: VIF > 10 = severe multicollinearity.\n")
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
cat("\n")

# ── LM Post-2: Outliers in residuals ─────────────────────────
cat("LM Post-2: Outliers (residual space — studentized > 3)\n")
extreme_lm <- linear_diagnostics %>% filter(abs(standardized_residuals) > 3)
cat("Number of extreme residual outliers:", nrow(extreme_lm), "\n")
if (nrow(extreme_lm) > 0) {
  cat("Likely correspond to safety car or pit-stop laps.\n")
  cat("Standardized residuals:\n")
  print(extreme_lm$standardized_residuals)
}
cat("\n")

# ── LM Post-3: Homoscedasticity ───────────────────────────────
cat("LM Post-3: Homoscedasticity\n")
cat("Required for linear regression (slide 17 — red).\n")

residuals_plot <- ggplot(linear_diagnostics, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6, color = "#2A6F97") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick") +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.9) +
  labs(title = "Residuals vs Fitted Values",
       subtitle = "Homoscedasticity check — spread should be constant",
       x = "Fitted values", y = "Residuals") +
  theme_minimal(base_size = 12)
print(residuals_plot)

bp_result   <- bptest(linear_model)
bp_pval_fmt <- ifelse(bp_result$p.value < 0.000001, "< 0.000001",
                      as.character(round(bp_result$p.value, 6)))
cat("Formal Breusch-Pagan test (lmtest::bptest)\n")
cat("H0: Residual variance is constant (homoscedasticity).\n")
cat("H1: Residual variance is not constant (heteroscedasticity).\n")
cat("BP statistic:", round(bp_result$statistic, 4), "\n")
cat("Degrees of freedom:", bp_result$parameter, "\n")
cat("p-value:", bp_pval_fmt, "\n")
if (bp_result$p.value < 0.05) {
  cat("Conclusion: reject H0 — heteroscedasticity is present.\n")
  cat("Consider robust standard errors or log-transform of LapTime.\n\n")
} else {
  cat("Conclusion: fail to reject H0 — homoscedasticity holds.\n\n")
}

# ── LM Post-4: Normality of residuals (reference only) ───────
cat("LM Post-4: Normality of residuals\n")
cat("Not required for linear regression (slide 17 — green).\n")
cat("Q-Q plot shown as reference only.\n")
qq_plot <- ggplot(linear_diagnostics, aes(sample = standardized_residuals)) +
  stat_qq(color = "#2A6F97", alpha = 0.6) +
  stat_qq_line(color = "firebrick", linewidth = 0.9) +
  labs(title = "Normal Q-Q Plot of Standardized Residuals (reference only)",
       subtitle = "Normality of residuals is not required for linear regression",
       x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal(base_size = 12)
print(qq_plot)
cat("\n")


# ═══════════════════════════════════════════════════════════════
# LOGISTIC REGRESSION (CLASSIFICATION)
#
# Correct ordering of assumption checks (slide 18):
#
#  BEFORE fitting — checked on training data:
#    Pre-1  Independence        (data structure)
#    Pre-2  Normality           (not required — slide 18 green)
#    Pre-3  Multicollinearity   (predictor correlation matrix)
#    Pre-4  Feature Scaling     (not required — slide 18 green)
#    Pre-5  Outliers            (z-scores of raw predictors)
#    Pre-6  Sample Size         (events-per-predictor)
#
#  AFTER fitting — require model output:
#    Post-1  Multicollinearity formal  (VIF on fitted model)
#    Post-2  Outliers (leverage)       (hatvalues)
#    Post-3  Homoscedasticity          (not required — slide 18 green)
# ═══════════════════════════════════════════════════════════════

cat("══════════════════════════════════════════════════════\n")
cat("LOGISTIC REGRESSION — PRE-FITTING ASSUMPTION CHECKS\n")
cat("══════════════════════════════════════════════════════\n\n")

# ── GLM Pre-1: Independence ───────────────────────────────────
cat("GLM Pre-1: Independence\n")
cat("Same structural limitation as linear model.\n")
cat("Laps are sequentially ordered within driver stints.\n\n")

# ── GLM Pre-2: Normality ──────────────────────────────────────
cat("GLM Pre-2: Normality\n")
cat("Not required for logistic regression (slide 18 — green).\n\n")

# ── GLM Pre-3: Multicollinearity (correlation matrix) ────────
cat("GLM Pre-3: Multicollinearity — predictor correlation matrix\n")
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
cat("NOTE: Formal VIF computed after fitting.\n\n")

# ── GLM Pre-4: Feature Scaling ────────────────────────────────
cat("GLM Pre-4: Feature Scaling\n")
cat("Not required for logistic regression (slide 18 — green).\n\n")

# ── GLM Pre-5: Outliers in predictors ────────────────────────
cat("GLM Pre-5: Outliers (predictor space — |z| > 3)\n")
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
cat("\n")

# ── GLM Pre-6: Sample Size (EPP) ─────────────────────────────
# Planned predictors:
#   SpeedI1, SpeedI2, SpeedFL, SpeedST          (4)
#   Compound (3 levels → 2 dummies)             (2)
#   TyreLife, FreshTyre                         (2)
#   AirTemp, Humidity, Pressure, TrackTemp,
#   WindSpeed, yellow_flag                      (6)
#   Total                                      (14)
glm_n_pred     <- 14
n_events_train <- min(table(train_data$top_10))
epp            <- floor(n_events_train / glm_n_pred)

cat("GLM Pre-6: Sample Size\n")
cat("Planned predictors:", glm_n_pred, "\n")
cat("Minority class (events) in training set:", n_events_train, "\n")
cat("Events-per-predictor (EPP):", epp, "\n")
cat("Rule of thumb: EPP >= 10.\n")
if (epp >= 10) {
  cat("Sample size is adequate.\n\n")
} else {
  cat("WARNING: EPP is low — model may be unstable.\n\n")
}


# ── FIT LOGISTIC MODEL ────────────────────────────────────────
cat("Classification model\n")
cat("Team excluded: near-perfect proxy for Position -> complete separation.\n")
cat("Position excluded: data leakage (top_10 derived from Position).\n\n")

classification_model <- glm(
  top_10 ~ SpeedI1 + SpeedI2 + SpeedFL + SpeedST + Compound +
    TyreLife + FreshTyre + AirTemp +
    Humidity + Pressure + TrackTemp + WindSpeed + yellow_flag,
  data   = train_data,
  family = binomial()
)

cat("Classification model summary\n")
print(summary(classification_model))
cat("\n")

class_test_prob <- predict(classification_model, newdata = test_data, type = "response")
class_test_pred <- factor(
  if_else(class_test_prob >= 0.5, "top_10", "not_top_10"),
  levels = c("not_top_10", "top_10")
)

cat("Classification confusion matrix (test set)\n")
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

cat("\nTest-set classification metrics\n")
cat("Accuracy: ",  round(accuracy,  4), "\n")
cat("Precision:",  round(precision, 4), "\n")
cat("Recall:   ",  round(recall,    4), "\n")
cat("F1 Score: ",  round(f1,        4), "\n\n")


cat("═════════════════════════════════════════════════════\n")
cat("LOGISTIC REGRESSION — POST-FITTING ASSUMPTION CHECKS\n")
cat("═════════════════════════════════════════════════════\n\n")

# ── GLM Post-1: Multicollinearity (VIF formal) ───────────────
cat("GLM Post-1: Multicollinearity — VIF (formal)\n")
vif_glm      <- vif(classification_model)
print(vif_glm)
high_vif_glm <- vif_glm[vif_glm > 10]
if (length(high_vif_glm) > 0) {
  cat("WARNING: VIF > 10 for:", paste(names(high_vif_glm), collapse = ", "), "\n")
} else {
  cat("No predictors exceed VIF = 10. Multicollinearity is acceptable.\n")
}
cat("\n")

# ── GLM Post-2: Outliers (leverage) ──────────────────────────
cat("GLM Post-2: Outliers (leverage)\n")
class_leverage <- hatvalues(classification_model)
n_high_lev     <- sum(class_leverage > 2 * mean(class_leverage))
cat("Observations with leverage > 2x mean:", n_high_lev, "\n\n")

# ── GLM Post-3: Homoscedasticity ─────────────────────────────
cat("GLM Post-3: Homoscedasticity\n")
cat("Not required for logistic regression (slide 18 — green).\n")
cat("The binomial GLM models variance as p*(1-p) automatically.\n\n")

cat("EDA script finished. Plots were printed in the R session.\n")