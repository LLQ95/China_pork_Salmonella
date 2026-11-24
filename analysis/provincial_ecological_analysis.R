# =============================================================================
# Reproducible provincial ecological analysis
#
# National genomic epidemiology of retail-pork Salmonella enterica across
# 25 Chinese provinces in 2021. This script reproduces the bivariate
# correlations, LASSO selection, nested leave-one-out cross-validation,
# the final two-predictor linear model, and the isolate-level group tests
# reported in the manuscript.
#
# Run this script from its own directory. Input files are read from ./data
# and generated tables are written to ./outputs.
# =============================================================================

options(stringsAsFactors = FALSE)
set.seed(2026)

prov_path  <- "data/province_level_data.csv"
isolate_path <- "data/isolate_level_data.csv"
output_dir <- "outputs"

prov <- read.csv(prov_path, check.names = FALSE)
iso  <- read.csv(isolate_path, check.names = FALSE)

candidate_vars <- c("temperature", "humidity", "rainfall", "sunshine", "pm25",
                    "pork_production", "pig_head", "pesticide", "fertilizer",
                    "ag_water", "water_use", "cod", "gdp_per_capita_k",
                    "population", "disposable_income", "pork_consumption",
                    "urbanization")

# The variables that enter the LASSO stage are the five factors showing a
# significant positive bivariate association with provincial ARG abundance.
lasso_vars <- c("temperature", "population", "rainfall",
                "gdp_per_capita_k", "disposable_income")

# =============================================================================
# 1. Spearman bivariate correlations
# =============================================================================
cat("\n[1] Spearman correlations with provincial mean ARG abundance\n")
spearman_results <- do.call(rbind, lapply(candidate_vars, function(v) {
  ct <- cor.test(prov[[v]], prov$mean_arg_count, method = "spearman",
                 exact = FALSE)
  data.frame(variable = v, rho = unname(ct$estimate), p_value = ct$p.value)
}))
spearman_results$significant <- spearman_results$p_value < 0.05
print(spearman_results, row.names = FALSE, digits = 3)
write.csv(spearman_results, file.path(output_dir, "spearman_correlations.csv"),
          row.names = FALSE)

# =============================================================================
# 2. LASSO with five-fold cross-validation
# =============================================================================
cat("\n[2] LASSO regression (five-fold cross-validation)\n")
library(glmnet)

x_full <- scale(as.matrix(prov[, lasso_vars]))
y_full <- prov$mean_arg_count
n_prov <- nrow(x_full)

# Explicit fold identifiers make the cross-validation fully deterministic.
full_fold <- rep(1:5, length.out = n_prov)
cv_full <- cv.glmnet(x_full, y_full, alpha = 1, foldid = full_fold,
                     standardize = FALSE)
coef_full <- as.matrix(coef(cv_full, s = "lambda.min"))
selected_full <- setdiff(rownames(coef_full)[coef_full[, 1] != 0], "(Intercept)")
cat("Non-zero predictors at lambda.min:", paste(selected_full, collapse = ", "), "\n")
cat("lambda.min =", signif(cv_full$lambda.min, 4), "\n")

# =============================================================================
# 3. Nested leave-one-out cross-validation selection frequency
# =============================================================================
cat("\n[3] Nested LOOCV predictor selection frequency\n")
selection_matrix <- matrix(FALSE, n_prov, length(lasso_vars),
                           dimnames = list(NULL, lasso_vars))
for (i in seq_len(n_prov)) {
  train_index <- setdiff(seq_len(n_prov), i)
  n_train <- length(train_index)
  inner_fold <- rep(1:5, length.out = n_train)
  cv_i <- cv.glmnet(x_full[train_index, , drop = FALSE], y_full[train_index],
                    alpha = 1, foldid = inner_fold, standardize = FALSE)
  coef_i <- as.matrix(coef(cv_i, s = "lambda.min"))
  nonzero_i <- rownames(coef_i)[coef_i[, 1] != 0]
  selection_matrix[i, lasso_vars %in% nonzero_i] <- TRUE
}
selection_frequency <- sort(colMeans(selection_matrix), decreasing = TRUE)
print(round(selection_frequency, 3))
write.csv(data.frame(variable = names(selection_frequency),
                     selection_frequency = as.numeric(selection_frequency)),
          file.path(output_dir, "lasso_selection_frequency.csv"),
          row.names = FALSE)

# =============================================================================
# 4. Final two-predictor linear model
# =============================================================================
cat("\n[4] Final two-predictor linear model\n")
final_model <- lm(mean_arg_count ~ temperature + gdp_per_capita_k, data = prov)
final_summary <- summary(final_model)
print(final_summary)

coefficient_table <- as.data.frame(coef(final_summary))
coefficient_table$ci_lower <- confint(final_model)[, 1]
coefficient_table$ci_upper <- confint(final_model)[, 2]
names(coefficient_table) <- c("estimate", "std_error", "t_value", "p_value",
                              "ci_lower", "ci_upper")
print(round(coefficient_table, 4))

f_stat <- final_summary$fstatistic
model_p <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
model_fit <- data.frame(
  r_squared = final_summary$r.squared,
  adjusted_r_squared = final_summary$adj.r.squared,
  aic = AIC(final_model),
  model_p_value = model_p
)
cat("\nModel fit:\n"); print(round(model_fit, 4))
write.csv(coefficient_table, file.path(output_dir, "linear_model_coefficients.csv"))
write.csv(model_fit, file.path(output_dir, "linear_model_fit.csv"),
          row.names = FALSE)

# =============================================================================
# 5. Multicollinearity (VIF)
# =============================================================================
cat("\n[5] Variance inflation factors\n")
library(car)
vif_values <- vif(final_model)
print(round(vif_values, 3))

# =============================================================================
# 6. Poisson regression and incidence rate ratios
# =============================================================================
cat("\n[6] Poisson regression (incidence rate ratios)\n")
poisson_model <- glm(round(mean_arg_count) ~ temperature + gdp_per_capita_k,
                     data = prov, family = poisson)
poisson_summary <- summary(poisson_model)
irr <- exp(coef(poisson_model))
print(data.frame(coefficient = coef(poisson_model),
                 irr = irr,
                 p_value = poisson_summary$coefficients[, 4],
                 row.names = names(coef(poisson_model))), digits = 4)

# =============================================================================
# 7. Generalized additive model for nonlinear relationships
# =============================================================================
cat("\n[7] Generalized additive model\n")
library(mgcv)
gam_model <- gam(mean_arg_count ~ s(temperature) + s(gdp_per_capita_k),
                 data = prov)
gam_summary <- summary(gam_model)
print(gam_summary)
cat("Smooth-term effective degrees of freedom:\n")
print(round(gam_summary$edf, 3))
cat("Linear GAM AIC:", round(AIC(gam(mean_arg_count ~ temperature +
    gdp_per_capita_k, data = prov)), 2),
    "  smooth GAM AIC:", round(AIC(gam_model), 2), "\n")

# =============================================================================
# 8. Isolate-level group comparisons
# =============================================================================
cat("\n[8] Isolate-level group comparisons\n")

compare_two_groups <- function(values, group, n_comparisons = 1) {
  levels_g <- levels(as.factor(group))
  a <- values[group == levels_g[1]]
  b <- values[group == levels_g[2]]
  t_test <- t.test(a, b)
  w_test <- wilcox.test(a, b)
  data.frame(level_1 = levels_g[1], level_2 = levels_g[2],
             n_1 = length(a), n_2 = length(b),
             mean_1 = mean(a), mean_2 = mean(b),
             mean_difference = mean(a) - mean(b),
             t_p = t_test$p.value,
             t_p_bonferroni = min(t_test$p.value * n_comparisons, 1),
             wilcox_p = w_test$p.value,
             wilcox_p_bonferroni = min(w_test$p.value * n_comparisons, 1))
}

print_rounded <- function(df, digits = 4) {
  numeric_columns <- sapply(df, is.numeric)
  df[numeric_columns] <- round(df[numeric_columns], digits)
  print(df)
}

# Two-sample proportion test that returns 1 when both groups have no events,
# avoiding a degenerate chi-squared result.
safe_prop_p <- function(x1, n1, x2, n2) {
  if (x1 == 0 && x2 == 0) return(1)
  prop.test(c(x1, x2), c(n1, n2))$p.value
}

# 8a. Overall ARG abundance by temperature group
cat("\n8a. Overall ARG abundance: warm versus cold\n")
overall_temperature <- compare_two_groups(iso$arg_count,
                                          as.factor(iso$temperature_group))
print_rounded(overall_temperature)

# 8b. ARG abundance by temperature group within each serovar. The five
# serovar-specific tests form one family for Bonferroni correction.
serovar_levels <- c("Rissen", "London", "4,[5],12:i:-", "Derby", "Typhimurium")
cat("\n8b. Within-serovar warm versus cold comparisons (Bonferroni m = 5)\n")
serovar_temperature <- do.call(rbind, lapply(serovar_levels, function(s) {
  sub <- iso[iso$serovar == s, ]
  result <- compare_two_groups(sub$arg_count,
                               as.factor(sub$temperature_group),
                               n_comparisons = length(serovar_levels))
  cbind(serovar = s, result)
}))
print_rounded(serovar_temperature)
write.csv(serovar_temperature,
          file.path(output_dir, "serovar_temperature_tests.csv"),
          row.names = FALSE)

# 8c. Overall ARG abundance by income group
cat("\n8c. Overall ARG abundance: high versus low income\n")
overall_income <- compare_two_groups(iso$arg_count,
                                     as.factor(iso$income_group))
print_rounded(overall_income)

# 8d. ARG abundance by income group within each serovar
cat("\n8d. Within-serovar high versus low income (Bonferroni m = 5)\n")
serovar_income <- do.call(rbind, lapply(serovar_levels, function(s) {
  sub <- iso[iso$serovar == s, ]
  result <- compare_two_groups(sub$arg_count, as.factor(sub$income_group),
                               n_comparisons = length(serovar_levels))
  cbind(serovar = s, result)
}))
print_rounded(serovar_income)
write.csv(serovar_income, file.path(output_dir, "serovar_income_tests.csv"),
          row.names = FALSE)

# 8e. ARG abundance in S. London by IncFIB(K) carriage
cat("\n8e. S. London ARG abundance by IncFIB(K) carriage\n")
london <- iso[iso$serovar == "London", ]
london$plasmid_group <- ifelse(london$incfib_k == 1, "IncFIB(K)", "other")
london_plasmid <- compare_two_groups(london$arg_count,
                                     as.factor(london$plasmid_group))
print_rounded(london_plasmid)

# 8f. Resistance rates by temperature group. Each drug is tested as a
# two-sample proportion comparison and corrected over the 13 drugs.
cat("\n8f. Resistance rates by temperature group (Bonferroni m = 13)\n")
drug_columns <- c("amp", "ipm", "fep", "ctx", "tet", "tgc", "col", "gen",
                  "cip", "sxt", "azm", "chl", "str")
resistance_temperature <- do.call(rbind, lapply(drug_columns, function(drug) {
  warm_d <- iso[iso$temperature_group == "warm", drug]
  cold_d <- iso[iso$temperature_group == "cold", drug]
  rate_warm <- mean(warm_d)
  rate_cold <- mean(cold_d)
  p_test <- safe_prop_p(sum(warm_d), length(warm_d),
                        sum(cold_d), length(cold_d))
  data.frame(drug = drug,
             rate_warm = rate_warm, rate_cold = rate_cold,
             difference = rate_warm - rate_cold,
             p_value = p_test,
             p_bonferroni = min(p_test * length(drug_columns), 1))
}))
print_rounded(resistance_temperature)
write.csv(resistance_temperature,
          file.path(output_dir, "resistance_temperature_tests.csv"),
          row.names = FALSE)

# 8g. Resistance rates by income group
cat("\n8g. Resistance rates by income group (Bonferroni m = 13)\n")
resistance_income <- do.call(rbind, lapply(drug_columns, function(drug) {
  high_d <- iso[iso$income_group == "high", drug]
  low_d  <- iso[iso$income_group == "low", drug]
  p_test <- safe_prop_p(sum(high_d), length(high_d),
                        sum(low_d), length(low_d))
  data.frame(drug = drug,
             rate_high = mean(high_d), rate_low = mean(low_d),
             difference = mean(high_d) - mean(low_d),
             p_value = p_test,
             p_bonferroni = min(p_test * length(drug_columns), 1))
}))
print_rounded(resistance_income)
write.csv(resistance_income,
          file.path(output_dir, "resistance_income_tests.csv"),
          row.names = FALSE)

cat("\nAll analyses completed. Output tables saved in", output_dir, "\n")
