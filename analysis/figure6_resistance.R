# =============================================================================
# Figure 6. Phenotypic resistance rates by regional temperature and income
#
# Bars show the pooled isolate-level resistance rates for the 19 tested
# antibiotics; these are descriptive proportions, not the test statistic.
# The small dots show the resistance rate within each province, so the
# province-level distribution is visible. Significance markers are based on
# the province-level Welch tests with Holm correction across the 19 drugs
# (the primary clustered analysis). The cluster-robust sensitivity results
# are reported in the output tables.
# =============================================================================

library(ggplot2); library(tidyr); library(dplyr); library(patchwork)

iso_raw <- read.csv("data/isolate_level_data.csv", check.names = FALSE)
ast <- read.csv("data/ast_19_drugs.csv", check.names = FALSE)
iso <- merge(iso_raw, ast[, !grepl("_cat$", names(ast))], by = "id")

temp <- read.csv("outputs/resistance_temperature_tests.csv", check.names = FALSE)
inc  <- read.csv("outputs/resistance_income_tests.csv", check.names = FALSE)

star <- function(p) ifelse(is.na(p), "",
                    ifelse(p < 0.001, "***",
                    ifelse(p < 0.01, "**",
                    ifelse(p < 0.05, "*", ""))))

# order drugs by the warm-region rate for a readable shared axis
drug_order <- temp$drug[order(-temp$rate_2)]

# Province-level resistance rates for the overlaid dots.
province_rates <- function(group_var) {
  iso[, c("province", group_var, drug_order)] %>%
    pivot_longer(all_of(drug_order), names_to = "drug", values_to = "resistant") %>%
    group_by(drug, .data[[group_var]], province) %>%
    summarise(prov_rate = mean(resistant), .groups = "drop") %>%
    rename(group = all_of(group_var))
}
prov_temp <- province_rates("temperature_group")
prov_inc  <- province_rates("income_group")

build_long <- function(df, label1, label2) {
  out <- rbind(
    data.frame(drug = df$drug, group = label1,
               rate = df$rate_1, star = star(df$holm_province_t)),
    data.frame(drug = df$drug, group = label2,
               rate = df$rate_2, star = star(df$holm_province_t)))
  out$drug <- factor(out$drug, levels = drug_order)
  out
}
temp_long <- build_long(temp, "Cold", "Warm")
inc_long  <- build_long(inc, "Low", "High")
prov_temp$drug <- factor(prov_temp$drug, levels = drug_order)
prov_inc$drug  <- factor(prov_inc$drug, levels = drug_order)

base_theme <- theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 13))

# Show the solid fill colors in the legend even though the bars are translucent.
legend_guide <- guides(fill = guide_legend(override.aes = list(alpha = 1)))

p_temp <- ggplot(temp_long, aes(x = drug, y = 100 * rate, fill = group)) +
  geom_col(position = position_dodge(0.82), width = 0.78, alpha = 0.55) +
  geom_point(data = prov_temp, aes(x = drug, y = 100 * prov_rate, group = group),
             position = position_dodge(0.82), size = 1.1, alpha = 0.75,
             colour = "black", inherit.aes = FALSE) +
  geom_text(aes(label = star), position = position_dodge(0.82),
            vjust = -0.35, size = 4.5) +
  scale_fill_manual(values = c("Cold" = "#3F7CB2", "Warm" = "#E51A1D")) +
  labs(title = "A. Temperature groups", x = NULL,
       y = "Resistance rate (%)", fill = NULL) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  base_theme +
  legend_guide

p_inc <- ggplot(inc_long, aes(x = drug, y = 100 * rate, fill = group)) +
  geom_col(position = position_dodge(0.82), width = 0.78, alpha = 0.55) +
  geom_point(data = prov_inc, aes(x = drug, y = 100 * prov_rate, group = group),
             position = position_dodge(0.82), size = 1.1, alpha = 0.75,
             colour = "black", inherit.aes = FALSE) +
  geom_text(aes(label = star), position = position_dodge(0.82),
            vjust = -0.35, size = 4.5) +
  scale_fill_manual(values = c("Low" = "#0C5395", "High" = "#F2641A")) +
  labs(title = "B. Income groups", x = NULL,
       y = "Resistance rate (%)", fill = NULL) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  base_theme +
  legend_guide

fig <- p_temp / p_inc

ggsave("outputs/Figure6_resistance_groups.png", fig,
       width = 13, height = 11, dpi = 600)
ggsave("outputs/Figure6_resistance_groups.svg", fig,
       width = 13, height = 11)
cat("Figure 6 saved (PNG and SVG)\n")
