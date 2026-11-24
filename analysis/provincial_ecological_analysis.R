# =============================================================================
# Reproducible provincial ecological analysis
#
# National genomic epidemiology of retail-pork Salmonella enterica across
# 25 Chinese provinces in 2021. The script reproduces the bivariate
# correlations, LASSO selection, a leak-free nested leave-one-out
# cross-validation, the final two-predictor linear model, and the
# province-clustered group comparisons reported in the manuscript.
#
# Run this script from its own directory. Input files are read from ./data
# and generated tables are written to ./outputs.
# =============================================================================

options(stringsAsFactors = FALSE)
set.seed(2026)

prov_path    <- "data/province_level_data.csv"
isolate_path <- "data/isolate_level_data.csv"
ast_path     <- "data/ast_19_drugs.csv"
output_dir   <- "outputs"

prov <- read.csv(prov_path, check.names = FALSE)
iso  <- read.csv(isolate_path, check.names = FALSE)
ast  <- read.csv(ast_path, check.names = FALSE)
iso  <- merge(iso, ast, by = "id", all.x = TRUE)

candidate_vars <- c("temperature", "humidity", "rainfall", "sunshine", "pm25",
                    "pork_production", "pig_head", "pesticide", "fertilizer",
                    "ag_water", "water_use", "cod", "gdp_per_capita_k",
                    "population", "disposable_income", "pork_consumption",
                    "urbanization")

# The variables that enter the full-data LASSO are the five factors showing a
# significant positive bivariate association with provincial ARG abundance.
lasso_vars <- c("temperature", "population", "rainfall",
                "gdp_per_capita_k", "disposable_income")

serovar_levels <- c("Rissen", "London", "4,[5],12:i:-", "Derby", "Typhimurium")
drug_order <- c("AMP", "AMS", "CZA", "FEP", "CAZ", "CTX", "IMP", "MEM", "ATM",
                "SXT", "TET", "TIG", "GEN", "AMK", "CHL", "AZM", "CTE", "CTB",
                "CIP")

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
# 2. Full-data LASSO with five-fold cross-validation
# =============================================================================
cat("\n[2] LASSO regression (five-fold cross-validation)\n")
library(glmnet)

x_full <- scale(as.matrix(prov[, lasso_vars]))
y_full <- prov$mean_arg_count
n_prov <- nrow(x_full)

full_fold <- rep(1:5, length.out = n_prov)
cv_full <- cv.glmnet(x_full, y_full, alpha = 1, foldid = full_fold,
                     standardize = FALSE)
coef_full <- as.matrix(coef(cv_full, s = "lambda.min"))
selected_full <- setdiff(rownames(coef_full)[coef_full[, 1] != 0],
                         "(Intercept)")
cat("Non-zero predictors at lambda.min:", paste(selected_full, collapse = ", "),
    "\n")
cat("lambda.min =", signif(cv_full$lambda.min, 4), "\n")

# =============================================================================
# 3. Leak-free nested LOOCV selection frequency
#
# In every outer leave-one-out fold the Spearman screening, the standardization
# and the inner cross-validation are rebuilt on the training provinces only, so
# no information from the held-out province enters the selection decision.
# =============================================================================
cat("\n[3] Leak-free nested LOOCV predictor selection frequency\n")
selection_matrix <- matrix(FALSE, n_prov, length(candidate_vars),
                           dimnames = list(NULL, candidate_vars))
for (i in seq_len(n_prov)) {
  train <- prov[-i, , drop = FALSE]
  rho <- c(); pval <- c()
  for (v in candidate_vars) {
    ct <- cor.test(train[[v]], train$mean_arg_count, method = "spearman",
                   exact = FALSE)
    rho[v] <- unname(ct$estimate); pval[v] <- ct$p.value
  }
  screened <- candidate_vars[rho > 0 & pval < 0.05]
  if (length(screened) == 0) next
  x_train <- scale(as.matrix(train[, screened]))
  inner_fold <- rep(1:5, length.out = nrow(train))
  cv_i <- cv.glmnet(x_train, train$mean_arg_count, alpha = 1,
                    foldid = inner_fold, standardize = FALSE)
  coef_i <- as.matrix(coef(cv_i, s = "lambda.min"))
  nonzero_i <- rownames(coef_i)[coef_i[, 1] != 0]
  selection_matrix[i, screened %in% nonzero_i] <- TRUE
}
selection_frequency <- sort(colMeans(selection_matrix), decreasing = TRUE)
selection_frequency <- selection_frequency[selection_frequency > 0]
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
print(data.frame(coefficient = coef(poisson_model),
                 irr = exp(coef(poisson_model)),
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
# 8. Province-clustered group comparisons
#
# Temperature and income are province-level attributes and isolates from the
# same province are not independent. The inferential unit is therefore the
# province: group tests are run on province-level summaries (n = 25), and
# isolate-level cluster-robust standard errors are reported alongside them.
# =============================================================================
cat("\n[8] Province-clustered group comparisons\n")
library(dplyr); library(sandwich); library(lmtest)

# Cluster-robust p-value from an isolate-level linear model.
cluster_p <- function(formula, data) {
  fit <- lm(formula, data = data)
  coeftest(fit, vcov = vcovCL(fit, cluster = data$province, type = "HC1"))[2, 4]
}

# Group test on province-level summaries for a continuous outcome.
continuous_group_test <- function(data, outcome, group_var) {
  data$y <- data[[outcome]]; data$g <- data[[group_var]]
  province_summary <- data %>% group_by(province) %>%
    summarise(mean_y = mean(y), g = first(g))
  p_t <- t.test(mean_y ~ g, province_summary)$p.value
  p_w <- wilcox.test(mean_y ~ g, province_summary)$p.value
  p_cluster <- cluster_p(y ~ g, data)
  data.frame(mean_1 = mean(data$y[data$g == sort(unique(data$g))[1]]),
             mean_2 = mean(data$y[data$g == sort(unique(data$g))[2]]),
             n_provinces = nrow(province_summary),
             p_province_t = p_t, p_province_wilcoxon = p_w,
             p_cluster = p_cluster)
}

# Group test on province-level summaries for a binary (0/1) outcome.
binary_group_test <- function(data, outcome, group_var) {
  data$y <- data[[outcome]]; data$g <- data[[group_var]]
  province_summary <- data %>% group_by(province) %>%
    summarise(rate = mean(y), g = first(g))
  p_t <- t.test(rate ~ g, province_summary)$p.value
  p_w <- wilcox.test(rate ~ g, province_summary)$p.value
  p_cluster <- cluster_p(y ~ g, data)
  groups <- sort(unique(data$g))
  data.frame(rate_1 = mean(data$y[data$g == groups[1]]),
             rate_2 = mean(data$y[data$g == groups[2]]),
             n_provinces = nrow(province_summary),
             p_province_t = p_t, p_province_wilcoxon = p_w,
             p_cluster = p_cluster)
}

print4 <- function(df) {
  numeric_cols <- vapply(df, is.numeric, logical(1))
  df[numeric_cols] <- round(df[numeric_cols], 4)
  print(df, row.names = FALSE)
}

# 8a. Overall ARG abundance by temperature group
cat("\n8a. Overall ARG abundance: warm versus cold\n")
overall_temperature <- continuous_group_test(iso, "arg_count",
                                             "temperature_group")
print4(overall_temperature)

# 8b. Within-serovar ARG abundance by temperature group (Holm over five serovars)
cat("\n8b. Within-serovar warm versus cold comparisons\n")
serovar_temperature <- do.call(rbind, lapply(serovar_levels, function(s) {
  cbind(serovar = s,
        continuous_group_test(iso[iso$serovar == s, ], "arg_count",
                              "temperature_group"))
}))
serovar_temperature$holm_province_t <-
  p.adjust(serovar_temperature$p_province_t, "holm")
serovar_temperature$holm_cluster <-
  p.adjust(serovar_temperature$p_cluster, "holm")
print4(serovar_temperature)
write.csv(serovar_temperature,
          file.path(output_dir, "serovar_temperature_tests.csv"),
          row.names = FALSE)

# 8c. Overall ARG abundance by income group
cat("\n8c. Overall ARG abundance: high versus low income\n")
overall_income <- continuous_group_test(iso, "arg_count", "income_group")
print4(overall_income)

# 8d. Within-serovar ARG abundance by income group
cat("\n8d. Within-serovar high versus low income comparisons\n")
serovar_income <- do.call(rbind, lapply(serovar_levels, function(s) {
  cbind(serovar = s,
        continuous_group_test(iso[iso$serovar == s, ], "arg_count",
                              "income_group"))
}))
serovar_income$holm_province_t <-
  p.adjust(serovar_income$p_province_t, "holm")
serovar_income$holm_cluster <-
  p.adjust(serovar_income$p_cluster, "holm")
print4(serovar_income)
write.csv(serovar_income, file.path(output_dir, "serovar_income_tests.csv"),
          row.names = FALSE)

# 8e. IncFIB(K) in S. London
cat("\n8e. S. London IncFIB(K) carriage by temperature and ARG by carriage\n")
london <- iso[iso$serovar == "London", ]
incfib_temperature <- binary_group_test(london, "incfib_k", "temperature_group")
print4(incfib_temperature)
london_arg_plasmid <- continuous_group_test(
  london, "arg_count", "incfib_k")
print4(london_arg_plasmid)

# 8f. Resistance rates by temperature group (Holm over 19 drugs)
cat("\n8f. Resistance rates by temperature group (19 drugs)\n")
resistance_temperature <- do.call(rbind, lapply(drug_order, function(drug) {
  cbind(drug = drug,
        binary_group_test(iso, drug, "temperature_group"))
}))
resistance_temperature$holm_province_t <-
  p.adjust(resistance_temperature$p_province_t, "holm")
resistance_temperature$holm_cluster <-
  p.adjust(resistance_temperature$p_cluster, "holm")
print4(resistance_temperature)
write.csv(resistance_temperature,
          file.path(output_dir, "resistance_temperature_tests.csv"),
          row.names = FALSE)

# 8g. Resistance rates by income group
cat("\n8g. Resistance rates by income group (19 drugs)\n")
resistance_income <- do.call(rbind, lapply(drug_order, function(drug) {
  cbind(drug = drug,
        binary_group_test(iso, drug, "income_group"))
}))
resistance_income$holm_province_t <-
  p.adjust(resistance_income$p_province_t, "holm")
resistance_income$holm_cluster <-
  p.adjust(resistance_income$p_cluster, "holm")
print4(resistance_income)
write.csv(resistance_income,
          file.path(output_dir, "resistance_income_tests.csv"),
          row.names = FALSE)

cat("\nAll analyses completed. Output tables saved in", output_dir, "\n")
