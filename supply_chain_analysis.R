# =============================================================================
# SUPPLY CHAIN ANALYSIS — Options 1, 2 & 3
# Paper: Income Generation in Rural Ghana
# Authors: Kinsley Delanyo Adjei & Yaw Bonsu Osei-Asare
# =============================================================================
# Requires all objects from the main script to be loaded in environment first.
# Add this after Section 13 (Quantile Regression) in your main script.
# =============================================================================

library(nnet)        # multinom() for multinomial logit
library(tidyverse)
library(stargazer)
library(quantreg)
library(broom)
library(flextable)
library(officer)


# =============================================================================
# SECTION 14: OPTION 1 — SUPPLY CHAIN POSITION AS WELFARE PREDICTOR
# Add supply_chain to quantile regression (Table 7 in paper)
# Research question: Does supply chain position shape welfare returns to SDI?
# =============================================================================

# Drop NA supply chain rows for this analysis
df_sc <- df %>% filter(!is.na(supply_chain))

# "Production" as reference category (most common, subsistence baseline)
df_sc$supply_chain <- relevel(factor(df_sc$supply_chain), ref = "Production")

# Updated quantile regression formula with supply_chain added
qr_formula_sc <- welfare ~ SDI + activity_count +
  supply_chain +                          # <-- NEW
  AGEY + SEX + hhsize + currently_married +
  basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  share_WAGE_HID + share_AGINC_GROSS + share_INCNF_GROSS +
  share_RENT_INC + share_REMIT_INC +
  owns_mobile_phone + electricity_access + access_to_credit +
  DISABLED + ez

quantiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Run quantile models with supply chain
qr_sc_q10 <- rq(qr_formula_sc, data = df_sc, tau = 0.10)
qr_sc_q25 <- rq(qr_formula_sc, data = df_sc, tau = 0.25)
qr_sc_q50 <- rq(qr_formula_sc, data = df_sc, tau = 0.50)
qr_sc_q75 <- rq(qr_formula_sc, data = df_sc, tau = 0.75)
qr_sc_q90 <- rq(qr_formula_sc, data = df_sc, tau = 0.90)

# OLS benchmark with supply chain
ols_sc <- lm(qr_formula_sc, data = df_sc)

# Summaries
summary(qr_sc_q50, se = "boot", R = 1000)
summary(ols_sc)

# Export Table 7
stargazer(
  qr_sc_q10, qr_sc_q25, qr_sc_q50, qr_sc_q75, qr_sc_q90, ols_sc,
  title          = "Table 7. Quantile Regression: Effect of SDI and Supply Chain Position on Welfare",
  type           = "text",
  out            = "Table7_Quantile_SupplyChain.html",
  align          = TRUE,
  dep.var.labels = "Household Welfare (GHS, deflated)",
  column.labels  = c("Q10", "Q25", "Q50", "Q75", "Q90", "OLS"),
  covariate.labels = c(
    "SDI (key variable)", "No. of Activities",
    "Supply Chain: Processing/Manufacturing",
    "Supply Chain: Transportation & Logistics",
    "Supply Chain: Wholesale & Distribution",
    "Supply Chain: Retail",
    "Supply Chain: Support Services",
    "Supply Chain: Other/Non-Supply Chain",
    "Age", "Sex (Male=1)", "Household Size", "Currently Married",
    "Basic Education", "Secondary Education", "Tertiary Education",
    "Adult Education", "Technical Education",
    "Wage Income Share", "Agricultural Income Share",
    "Non-Farm Income Share", "Rental Income Share", "Remittance Share",
    "Owns Mobile Phone", "Electricity Access", "Access to Credit", "Disabled",
    "Zone: Forest", "Zone: Savannah"
  ),
  keep.stat = c("n"),
  no.space  = TRUE
)

# --- Figure 5: Supply Chain Position Effect Across Welfare Distribution ---
# Extract supply chain coefficients across quantiles

sc_vars <- c(
  "supply_chainProcessing/Manufacturing",
  "supply_chainTransportation and Logistics",
  "supply_chainWholesale and Distribution",
  "supply_chainRetail",
  "supply_chainSupport Services"
)

sc_coef_plot <- map_dfr(
  setNames(quantiles, paste0("tau_", quantiles)),
  ~ tidy(rq(qr_formula_sc, data = df_sc, tau = .x), se.type = "boot", R = 1000),
  .id = "quantile"
) %>%
  mutate(quantile = as.numeric(str_remove(quantile, "tau_"))) %>%
  filter(term %in% sc_vars) %>%
  mutate(term = str_remove(term, "supply_chain"))

fig5 <- ggplot(sc_coef_plot, aes(x = quantile, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, fill = "#4DAC26") +
  geom_line(color = "#4DAC26", linewidth = 1.1) +
  geom_point(color = "#4DAC26", size = 2.5) +
  facet_wrap(~ term, scales = "free_y", ncol = 2) +
  scale_x_continuous(
    breaks = quantiles,
    labels = c("Q10\n(Poorest)", "Q25", "Q50\n(Median)", "Q75", "Q90\n(Wealthiest)")
  ) +
  labs(
    title    = "Figure 5. Welfare Returns by Supply Chain Position Across the Welfare Distribution",
    subtitle = "Relative to Production (reference). Green shading = 95% CI.",
    x        = "Welfare Quantile",
    y        = "Coefficient Estimate (GHS)",
    caption  = "Source: GLSS7 (2016/17). Bootstrapped standard errors (1,000 replications)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold", size = 10),
    plot.title      = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig5
ggsave("Figure5_SupplyChain_Quantile_Coefficients.png", fig5,
       width = 10, height = 8, dpi = 300)


# =============================================================================
# SECTION 15: OPTION 2 — SUPPLY CHAIN AS DIVERSIFICATION PATHWAY
# Multinomial logit: what predicts participation in higher-value chain segments?
# Research question: What household characteristics predict supply chain position?
# =============================================================================

# Reference: Production (subsistence baseline)
df_sc$supply_chain <- relevel(factor(df_sc$supply_chain), ref = "Production")

# Multinomial logit formula
mlogit_formula <- supply_chain ~ AGEY + SEX + female_prop + hhsize +
  currently_married + pstatus +
  basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  SDI + activity_count +
  owns_mobile_phone + electricity_access + access_to_credit +
  REMIT_INC + DISABLED + ez

# Fit model
mlogit_model <- multinom(mlogit_formula, data = df_sc, MaxNWts = 5000, maxit = 500)
summary(mlogit_model)

# --- Compute z-scores and p-values (multinom doesn't give them directly) ---
z_scores <- summary(mlogit_model)$coefficients /
            summary(mlogit_model)$standard.errors
p_values  <- 2 * (1 - pnorm(abs(z_scores)))

# Print p-values for inspection
print(round(p_values, 4))

# --- Marginal Effects (Average Marginal Effects via margins-style approach) ---
# For a clean table, extract coefficients and SEs

mlogit_coef <- as.data.frame(t(summary(mlogit_model)$coefficients))
mlogit_se   <- as.data.frame(t(summary(mlogit_model)$standard.errors))
mlogit_pval <- as.data.frame(t(p_values))

# Export multinomial table manually as flextable (stargazer doesn't support multinom)
mlogit_export <- map_dfr(
  colnames(t(summary(mlogit_model)$coefficients)),
  function(outcome) {
    tibble(
      Outcome    = outcome,
      Variable   = rownames(t(summary(mlogit_model)$coefficients)),
      Coef       = t(summary(mlogit_model)$coefficients)[, outcome],
      SE         = t(summary(mlogit_model)$standard.errors)[, outcome],
      z          = t(z_scores)[, outcome],
      p          = t(p_values)[, outcome]
    )
  }
) %>%
  mutate(
    Sig   = case_when(p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.1 ~ "*", TRUE ~ ""),
    Stars = paste0(round(Coef, 3), Sig, "\n(", round(SE, 3), ")")
  )

# Pivot wide for publication table
mlogit_table_wide <- mlogit_export %>%
  dplyr::select(Variable, Outcome, Stars) %>%
  pivot_wider(names_from = Outcome, values_from = Stars)

mlogit_table_wide %>%
  flextable() %>%
  set_caption("Table 8. Multinomial Logit: Determinants of Supply Chain Position (Ref: Production)") %>%
  add_footer_lines("Note: Coefficients with standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.1") %>%
  autofit() %>%
  save_as_docx(path = "Table8_Multinomial_SupplyChain.docx")

cat("Table 8 saved.\n")


# =============================================================================
# SECTION 16: OPTION 3 (EXTENSION) — SDI × SUPPLY CHAIN INTERACTION
# Research question: Do welfare returns to diversification vary by chain position?
# Framed as robustness/extension — report in appendix or supplementary table.
# =============================================================================

# Create interaction term: SDI × supply chain position
df_sc <- df_sc %>%
  mutate(supply_chain = relevel(factor(supply_chain), ref = "Production"))

# Interaction formula
qr_formula_interact <- welfare ~ SDI * supply_chain +    # <-- INTERACTION
  activity_count +
  AGEY + SEX + hhsize + currently_married +
  basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  share_WAGE_HID + share_AGINC_GROSS + share_INCNF_GROSS +
  share_RENT_INC + share_REMIT_INC +
  owns_mobile_phone + electricity_access + access_to_credit +
  DISABLED + ez

# Run at median (Q50) and OLS only for extension (keeps it lean)
qr_interact_q25 <- rq(qr_formula_interact, data = df_sc, tau = 0.25)
qr_interact_q50 <- rq(qr_formula_interact, data = df_sc, tau = 0.50)
qr_interact_q75 <- rq(qr_formula_interact, data = df_sc, tau = 0.75)
ols_interact    <- lm(qr_formula_interact, data = df_sc)

summary(qr_interact_q50, se = "boot", R = 1000)
summary(ols_interact)

# Export as appendix table
stargazer(
  qr_interact_q25, qr_interact_q50, qr_interact_q75, ols_interact,
  title          = "Appendix Table A1. Extension: SDI × Supply Chain Interaction Effects on Welfare",
  type           = "text",
  out            = "AppendixA1_SDI_SupplyChain_Interaction.html",
  align          = TRUE,
  dep.var.labels = "Household Welfare (GHS, deflated)",
  column.labels  = c("Q25", "Q50", "Q75", "OLS"),
  keep.stat      = c("n"),
  no.space       = TRUE
)

# --- Figure 6: Predicted Welfare by SDI × Supply Chain Position (OLS for clarity) ---
# Generate predicted values across SDI range for each supply chain category

sdi_range <- seq(0, 1, by = 0.05)

pred_data <- expand.grid(
  SDI            = sdi_range,
  supply_chain   = levels(df_sc$supply_chain),
  activity_count = mean(df_sc$activity_count, na.rm = TRUE),
  AGEY           = mean(df_sc$AGEY, na.rm = TRUE),
  SEX            = 1,
  hhsize         = mean(df_sc$hhsize, na.rm = TRUE),
  currently_married     = 1,
  basic_education       = 1,
  secondary_education   = 0,
  tertiary_education    = 0,
  adult_education       = 0,
  technical_education   = 0,
  share_WAGE_HID        = mean(df_sc$share_WAGE_HID, na.rm = TRUE),
  share_AGINC_GROSS     = mean(df_sc$share_AGINC_GROSS, na.rm = TRUE),
  share_INCNF_GROSS     = mean(df_sc$share_INCNF_GROSS, na.rm = TRUE),
  share_RENT_INC        = mean(df_sc$share_RENT_INC, na.rm = TRUE),
  share_REMIT_INC       = mean(df_sc$share_REMIT_INC, na.rm = TRUE),
  owns_mobile_phone     = 1,
  electricity_access    = 0,
  access_to_credit      = 0,
  DISABLED              = 0,
  ez                    = "Forest"   # modal zone
) %>%
  mutate(supply_chain = factor(supply_chain, levels = levels(df_sc$supply_chain)))

pred_data$predicted_welfare <- predict(ols_interact, newdata = pred_data)

fig6 <- ggplot(pred_data, aes(x = SDI, y = predicted_welfare,
                               color = supply_chain,
                               linetype = supply_chain)) +
  geom_line(linewidth = 1.1) +
  scale_color_brewer(palette = "Dark2", name = "Supply Chain Position") +
  scale_linetype_manual(
    values = c("solid","dashed","dotdash","longdash","twodash","dotted","solid"),
    name   = "Supply Chain Position"
  ) +
  labs(
    title    = "Figure 6. Predicted Welfare by SDI and Supply Chain Position",
    subtitle = "Predictions at mean covariate values; reference zone = Forest",
    x        = "Simpson Diversity Index (SDI)",
    y        = "Predicted Welfare (GHS, deflated)",
    caption  = "Source: GLSS7 (2016/17). OLS predictions for illustration."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 12),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig6
ggsave("Figure6_Predicted_Welfare_SDI_SupplyChain.png", fig6,
       width = 10, height = 6, dpi = 300)

cat("\nAll supply chain analyses complete.\n")
cat("Outputs saved: Table7, Table8, AppendixA1, Figure5, Figure6\n")

# =============================================================================
# END OF SUPPLY CHAIN ANALYSIS
# =============================================================================
