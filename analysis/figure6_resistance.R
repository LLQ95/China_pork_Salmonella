# =============================================================================
# Figure 6. Phenotypic resistance rates by regional temperature and income
#
# Bars show the pooled isolate-level resistance rates for the 19 tested
# antibiotics; these are descriptive proportions, not the test statistic.
# Significance markers are based on the province-level Welch tests with
# Holm correction across the 19 drugs (the primary clustered analysis).
# The cluster-robust sensitivity results are reported in the output tables.
# =============================================================================

library(ggplot2); library(patchwork)

temp <- read.csv("outputs/resistance_temperature_tests.csv", check.names = FALSE)
inc  <- read.csv("outputs/resistance_income_tests.csv", check.names = FALSE)

star <- function(p) ifelse(is.na(p), "",
                    ifelse(p < 0.001, "***",
                    ifelse(p < 0.01, "**",
                    ifelse(p < 0.05, "*", ""))))

# order drugs by the warm-region rate for a readable shared axis
drug_order <- temp$drug[order(-temp$rate_2)]

build_long <- function(df, labels, rate_cols) {
  parts <- lapply(seq_along(labels), function(i) {
    data.frame(drug = df$drug, group = labels[i],
               rate = df[[rate_cols[i]]],
               star = star(df$holm_province_t))
  })
  out <- do.call(rbind, parts)
  out$drug <- factor(out$drug, levels = drug_order)
  out$group <- factor(out$group, levels = labels)
  out
}
# rate_1 is the cold group and rate_2 is the warm group; place Warm first.
temp_long <- build_long(temp, c("Warm", "Cold"), c("rate_2", "rate_1"))
# rate_1 is the high-income group and rate_2 is the low-income group.
inc_long  <- build_long(inc, c("High", "Low"), c("rate_1", "rate_2"))

base_theme <- theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 13))

p_temp <- ggplot(temp_long, aes(x = drug, y = 100 * rate, fill = group)) +
  geom_col(position = position_dodge(0.82), width = 0.78) +
  geom_text(aes(label = star), position = position_dodge(0.82),
            vjust = -0.35, size = 4.5) +
  scale_fill_manual(values = c("Cold" = "#3F7CB2", "Warm" = "#E51A1D")) +
  labs(title = "A. Temperature groups", x = NULL,
       y = "Resistance rate (%)", fill = NULL) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  base_theme

p_inc <- ggplot(inc_long, aes(x = drug, y = 100 * rate, fill = group)) +
  geom_col(position = position_dodge(0.82), width = 0.78) +
  geom_text(aes(label = star), position = position_dodge(0.82),
            vjust = -0.35, size = 4.5) +
  scale_fill_manual(values = c("Low" = "#4C78A8", "High" = "#D62728")) +
  labs(title = "B. Income groups", x = NULL,
       y = "Resistance rate (%)", fill = NULL) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  base_theme

fig <- p_temp / p_inc

ggsave("outputs/Figure6_resistance_groups.png", fig,
       width = 13, height = 11, dpi = 600)
ggsave("outputs/Figure6_resistance_groups.svg", fig,
       width = 13, height = 11)
ggsave("outputs/Figure6_resistance_groups.pdf", fig,
       width = 13, height = 11)
cat("Figure 6 saved (PNG, SVG and PDF)\n")
