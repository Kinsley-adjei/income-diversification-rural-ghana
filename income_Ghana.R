# =============================================================================
# Income Generation in Rural Ghana: A Comparative Analysis of Welfare,
# Income Diversification, and Inequality Across Agroecological Zones
# =============================================================================
# Authors: Kinsley Delanyo Adjei & Yaw Bonsu Osei-Asare
# Data:    Ghana Living Standards Survey 7 (GLSS7)
# =============================================================================


# =============================================================================
# SECTION 1: SETUP — Libraries & Working Directory
# =============================================================================

# Install packages if needed (run once)
# install.packages(c("haven", "dplyr", "tidyverse", "gtsummary", "flextable",
#                    "openxlsx", "scales", "censReg", "modelsummary", "officer",
#                    "knitr", "cardx", "AER", "data.table", "broom",
#                    "ggplot2", "pscl", "readxl", "ineq", "ggthemes",
#                    "hrbrthemes", "quantreg", "stargazer", "varhandle"))

library(haven)
library(dplyr)
library(tidyverse)
library(gtsummary)
library(flextable)
library(openxlsx)
library(scales)
library(censReg)
library(modelsummary)
library(officer)
library(knitr)
library(cardx)
library(AER)
library(data.table)
library(broom)
library(ggplot2)
library(pscl)
library(readxl)
library(ineq)
library(ggthemes)
library(hrbrthemes)
library(quantreg)
library(stargazer)
library(varhandle)

setwd("D:/From drive/Academics/Thesis/Research Articles/Income Generation Article")


# =============================================================================
# SECTION 2: DATA LOADING & MERGING
# =============================================================================

# Load main GLSS7 dataset
df <- read_dta("glss7.dta")

# Load disability variable from Excel and merge
glss7_disability <- read_excel("glss7.xlsx") %>%
  dplyr::select(DISABLED, hid)

df <- df %>%
  inner_join(glss7_disability, by = "hid") %>%
  filter(rururb == 1)   # Keep rural households only


# =============================================================================
# SECTION 3: DATA CLEANING
# =============================================================================

# Impute missing categorical values
df$ETHNICITY[is.na(df$ETHNICITY)]   <- 9   # Other ethnicity
df$EDLEVEL_AR[is.na(df$EDLEVEL_AR)] <- 9   # Other education level
df$INCNF_GROSS[is.na(df$INCNF_GROSS)] <- 0 # Non-farm income: NA = zero

# Ensure key variables are numeric
df <- df %>%
  mutate(across(c(AGEY, AGINC_GROSS, INCNF_GROSS, RENT_INC, REMIT_INC,
                  INC_OTHER, TOTFOOD, RENTPAID, pindex_fd, pindex_nfd,
                  welfare, SDI),
                as.numeric))


# =============================================================================
# SECTION 4: VARIABLE CONSTRUCTION
# =============================================================================

df <- df %>%
  mutate(
    
    # --- Agroecological Zone ---
    ez = factor(ez, levels = c(1, 2, 3),
                labels = c("Coastal", "Forest", "Savannah")),
    
    # --- Age ---
    AgeGroup = cut(as.numeric(AGEY),
                   breaks = c(-Inf, 20, 40, 60, 80, Inf),
                   labels = c("0-19 years", "20-39 years", "40-59 years",
                              "60-79 years", "80+ years"),
                   right = FALSE),
    
    # --- Demographic Characteristics ---
    male_hh         = ifelse(SEX == 1, 1, 0),
    female_prop     = female / hhsize,
    currently_married = ifelse(MARSTAT %in% c(2, 3), 1, 0),
    large_hh        = ifelse(hhsize > 5, 1, 0),
    
    # --- Asset Endowments ---
    owns_house        = ifelse(OWNTYPE == 1, 1, 0),
    electricity_access = ifelse(ELEC > 0, 1, 0),
    owns_mobile_phone = ifelse(PHONE_9a > 0, 1, 0),
    
    # --- Human Capital (Education) ---
    basic_education     = ifelse(EDBASIC > 0, 1, 0),
    secondary_education = ifelse(EDLEVEL_AR == 4, 1, 0),
    tertiary_education  = ifelse(EDLEVEL_AR == 6, 1, 0),
    adult_education     = ifelse(EDLEVEL_AR == 7, 1, 0),
    technical_education = ifelse(EDLEVEL_AR == 5, 1, 0),
    
    # --- Ethnicity ---
    akan        = ifelse(ETHNICITY >= 0  & ETHNICITY < 20, 1, 0),
    ewe         = ifelse(ETHNICITY >= 30 & ETHNICITY < 40, 1, 0),
    ga_dangme   = ifelse(ETHNICITY >= 20 & ETHNICITY < 30, 1, 0),
    guan        = ifelse(ETHNICITY >= 40 & ETHNICITY < 50, 1, 0),
    gurma       = ifelse(ETHNICITY >= 50 & ETHNICITY < 60, 1, 0),
    mole_dagbani = ifelse(ETHNICITY >= 60 & ETHNICITY < 70, 1, 0),
    grusi       = ifelse(ETHNICITY >= 70 & ETHNICITY < 80, 1, 0),
    mande       = ifelse(ETHNICITY >= 80 & ETHNICITY < 90, 1, 0),
    
    # --- Religion ---
    christian     = ifelse(RELIGION %in% c(2, 3, 4, 5), 1, 0),
    muslim        = ifelse(RELIGION == 6, 1, 0),
    traditionalist = ifelse(RELIGION == 8, 1, 0),
    
    # --- Poverty ---
    worldpoor = ifelse(welfare < 3000.3, 1, 0),
    pstatus   = factor(pstatus, levels = c(0, 1, 2),
                       labels = c("Very poor", "Poor", "Non poor"))
  )


# =============================================================================
# SECTION 5: INCOME DIVERSIFICATION INDICES
# =============================================================================

df <- df %>%
  rowwise() %>%
  mutate(
    # Total household income
    total_income = sum(c_across(c(WAGE_HID, AGINC_GROSS, INCNF_GROSS,
                                  RENT_INC, REMIT_INC, INC_OTHER)),
                       na.rm = TRUE),
    
    # Income shares (zero if total income is zero)
    share_WAGE_HID    = ifelse(total_income > 0, WAGE_HID    / total_income, 0),
    share_AGINC_GROSS = ifelse(total_income > 0, AGINC_GROSS / total_income, 0),
    share_INCNF_GROSS = ifelse(total_income > 0, INCNF_GROSS / total_income, 0),
    share_RENT_INC    = ifelse(total_income > 0, RENT_INC    / total_income, 0),
    share_REMIT_INC   = ifelse(total_income > 0, REMIT_INC   / total_income, 0),
    share_INC_OTHER   = ifelse(total_income > 0, INC_OTHER   / total_income, 0),
    
    # Herfindahl Concentration Index (HCI) and Simpson Diversity Index (SDI)
    HCI = sum(c_across(starts_with("share_"))^2, na.rm = TRUE),
    SDI = 1 - HCI,
    
    # Number of income-generating activities
    activity_count = sum(c_across(c(WAGE_HID, AGINC_GROSS, INCNF_GROSS,
                                    RENT_INC, REMIT_INC, INC_OTHER)) > 0,
                         na.rm = TRUE)
  ) %>%
  ungroup()


# =============================================================================
# SECTION 6: SUPPLY CHAIN CLASSIFICATION
# =============================================================================

df <- df %>%
  mutate(
    supply_chain = case_when(
      s4aq34c %in% c(111,112,113,114,119,122,123,125,126,127,128,129,130,
                     141,144,145,146,149,150,311,312,321,322,11,151,152)
      ~ "Production",
      
      s4aq34c %in% c(1010,1020,1030,1040,1041,1050,1061,1062,1071,1073,
                     1079,1101,1102,1103,1104,1311,1312,1313,1392,1410,
                     1511,1512,1520,1610,1621,1622,1629,1709,1811,1820,
                     1920,2013,2023,2102,2220,2310,2391,2392,2393,2394,
                     2395,2410,2420,2511,2512,2591,2592,2593,2599,2652,
                     2710,2732,2824,2829,3012,3100,3211,3212,3220,3290)
      ~ "Processing/Manufacturing",
      
      s4aq34c %in% c(4921,4922,4923,5011,5012,5021,5210,5221,5223,5224,
                     5229,5310)
      ~ "Transportation and Logistics",
      
      s4aq34c %in% c(4610,4620,4630,4641,4649,4651,4652,4653,4659,4661,
                     4662,4663,4669,4690,730)
      ~ "Wholesale and Distribution",
      
      s4aq34c %in% c(4711,4719,4721,4722,4723,4730,4741,4742,4751,4752,
                     4753,4759,4761,4762,4763,4771,4772,4773,4774,4781,
                     4782,4789,4791,4799)
      ~ "Retail",
      
      s4aq34c %in% c(161,162,163,164,170,210,220,230,240,910,990,3312,
                     3314,9511,9512,9521,9522,9523,9529)
      ~ "Support Services",
      
      is.na(s4aq34c) ~ NA_character_,
      TRUE ~ "Other/Non-Supply Chain"
    ) %>%
      factor(levels = c("Production", "Processing/Manufacturing",
                        "Transportation and Logistics", "Wholesale and Distribution",
                        "Retail", "Support Services", "Other/Non-Supply Chain"))
  )


# =============================================================================
# SECTION 7: DESCRIPTIVE STATISTICS TABLE (Table 1)
# =============================================================================

table_summary <- df %>%
  tbl_summary(
    by = ez,
    include = c(AGEY, AgeGroup, male_hh, female_prop, currently_married,
                large_hh, owns_house, electricity_access, owns_mobile_phone,
                basic_education, secondary_education, tertiary_education,
                adult_education, technical_education,
                akan, ewe, ga_dangme, guan, gurma, mole_dagbani, grusi, mande,
                christian, muslim, traditionalist, DISABLED,
                worldpoor, WAGE_HID, AGINC_GROSS, INCNF_GROSS,
                RENT_INC, REMIT_INC, INC_OTHER,
                welfare, TOTFOOD, RENTPAID, pindex_fd, pindex_nfd,
                access_to_credit, pstatus, SDI, activity_count),
    label = list(
      AGEY ~ "Age",
      AgeGroup ~ "Age Group",
      male_hh ~ "Male Household Head",
      female_prop ~ "Female Household Proportion",
      currently_married ~ "Currently Married",
      large_hh ~ "Large Household (>5)",
      owns_house ~ "Owns House",
      electricity_access ~ "Electricity Access",
      owns_mobile_phone ~ "Owns Mobile Phone",
      basic_education ~ "Basic Education",
      secondary_education ~ "Secondary Education",
      tertiary_education ~ "Tertiary Education",
      adult_education ~ "Adult Education",
      technical_education ~ "Technical Education",
      akan ~ "Akan", ewe ~ "Ewe", ga_dangme ~ "Ga-Dangme",
      guan ~ "Guan", gurma ~ "Gurma", mole_dagbani ~ "Mole-Dagbani",
      grusi ~ "Grusi", mande ~ "Mande",
      christian ~ "Christian", muslim ~ "Muslim",
      traditionalist ~ "Traditionalist",
      DISABLED ~ "Disabled",
      worldpoor ~ "World Bank Poor",
      WAGE_HID ~ "Wage Income",
      AGINC_GROSS ~ "Agricultural Income",
      INCNF_GROSS ~ "Non-Farm Income",
      RENT_INC ~ "Rental Income",
      REMIT_INC ~ "Remittance Income",
      INC_OTHER ~ "Other Income",
      welfare ~ "Total Welfare",
      TOTFOOD ~ "Total Food Expenditure",
      RENTPAID ~ "Rent Paid",
      pindex_fd ~ "Food Price Index",
      pindex_nfd ~ "Non-Food Price Index",
      access_to_credit ~ "Access to Credit",
      pstatus ~ "GSS Poverty Status",
      SDI ~ "Simpson Diversity Index",
      activity_count ~ "Number of Income Activities"
    ),
    statistic = list(
      all_continuous()  ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) %>%
  add_overall() %>%
  add_n() %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      all_categorical() ~ "chisq.test"
    )
  ) %>%
  modify_caption("Table 1. Socio-Economic Characteristics of Rural Households by Agroecological Zone") %>%
  bold_labels()

table_summary

table_summary %>%
  as_flex_table() %>%
  save_as_docx(path = "Table1_Household_Characteristics_new.docx")


# =============================================================================
# SECTION 8: INCOME SHARES & PARTICIPATION TABLE (Table 2)
# =============================================================================

income_share_table <- df %>%
  mutate(
    participate_WAGE_HID    = ifelse(WAGE_HID    > 0, 1, 0),
    participate_AGINC_GROSS = ifelse(AGINC_GROSS > 0, 1, 0),
    participate_INCNF_GROSS = ifelse(INCNF_GROSS > 0, 1, 0),
    participate_RENT_INC    = ifelse(RENT_INC    > 0, 1, 0),
    participate_REMIT_INC   = ifelse(REMIT_INC   > 0, 1, 0),
    participate_INC_OTHER   = ifelse(INC_OTHER   > 0, 1, 0)
  ) %>%
  dplyr::select(ez, starts_with("share_"), starts_with("participate_")) %>%
  tbl_summary(
    by = ez,
    statistic = list(
      all_continuous()   ~ "{mean}",
      all_dichotomous()  ~ "{n} ({p}%)"
    ),
    digits  = list(all_continuous() ~ 3, all_categorical() ~ 1),
    missing = "no",
    label   = list(
      share_WAGE_HID    ~ "Share of Wage Income",
      share_AGINC_GROSS ~ "Share of Agricultural Income",
      share_INCNF_GROSS ~ "Share of Non-Farm Income",
      share_RENT_INC    ~ "Share of Rental Income",
      share_REMIT_INC   ~ "Share of Remittance Income",
      share_INC_OTHER   ~ "Share of Other Income",
      participate_WAGE_HID    ~ "Participation in Wage Income",
      participate_AGINC_GROSS ~ "Participation in Agricultural Income",
      participate_INCNF_GROSS ~ "Participation in Non-Farm Income",
      participate_RENT_INC    ~ "Participation in Rental Income",
      participate_REMIT_INC   ~ "Participation in Remittance Income",
      participate_INC_OTHER   ~ "Participation in Other Income"
    )
  ) %>%
  add_overall(col_label = "**Total (N = {N})**") %>%
  modify_caption("Table 2. Income Shares and Participation Rates by Agroecological Zone") %>%
  bold_labels()

income_share_table

income_share_table %>%
  as_flex_table() %>%
  save_as_docx(path = "Table2_Income_Shares_Participation_new.docx")


# =============================================================================
# SECTION 9: SUPPLY CHAIN TABLE (Table 3)
# =============================================================================

supply_chain_table <- df %>%
  filter(!is.na(supply_chain)) %>%
  tbl_summary(
    by      = ez,
    include = supply_chain,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    sort    = list(supply_chain ~ "frequency"),
    missing = "no"
  ) %>%
  add_overall(col_label = "**Total (N = {N})**") %>%
  modify_caption("Table 3. Supply Chain Distribution by Agroecological Zone") %>%
  bold_labels() %>%
  modify_header(label = "**Supply Chain Category**") %>%
  as_flex_table()

supply_chain_table

supply_chain_table %>%
  save_as_docx(path = "Table3_Supply_Chain_Distribution_new.docx")


# Top 10 ISIC codes by agroecological zone
top10_isic <- df %>%
  filter(!is.na(s4aq34c)) %>%
  group_by(ez, s4aq34c, supply_chain) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(ez) %>%
  arrange(desc(count), .by_group = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup()

top10_isic %>%
  flextable() %>%
  set_caption("Top 10 ISIC Codes by Agroecological Zone") %>%
  autofit() %>%
  save_as_docx(path = "Table3b_Top10_ISIC_Codes_new.docx")


# =============================================================================
# SECTION 10: FIGURES
# =============================================================================

# --- Figure 1: Income Participation & Shares by Zone ---
df_shares <- data.frame(
  IncomeSource = c("Agriculture","Non-farm Business","Wage",
                   "Rental","Remittance","Other"),
  Coastal_Share  = c(18.0, 32.5, 22.9, 16.2,  8.5, 1.0),
  Forest_Share   = c(33.9, 21.3, 17.5, 17.6,  7.8, 0.9),
  Savannah_Share = c(32.9, 16.2, 10.9, 32.4,  5.5, 1.8),
  Overall_Share  = c(31.1, 20.5, 15.2, 24.4,  6.8, 1.4),
  Coastal_Participation  = c(50.5, 98.5, 36.0, 85.4, 39.7,  4.4),
  Forest_Participation   = c(62.7, 97.2, 26.2, 88.9, 33.1,  4.0),
  Savannah_Participation = c(73.4, 96.4, 16.9, 96.9, 28.3, 10.3),
  Overall_Participation  = c(66.0, 97.2, 23.2, 92.2, 31.8,  7.0)
)

df_shares_long <- df_shares %>%
  pivot_longer(-IncomeSource,
               names_to  = c("Zone", "Metric"),
               names_sep = "_",
               values_to = "Value") %>%
  mutate(
    IncomeSource = factor(IncomeSource, levels = df_shares$IncomeSource),
    Zone = factor(Zone, levels = c("Coastal", "Forest", "Savannah", "Overall"))
  )

zone_colors <- c("Coastal" = "#2166AC", "Forest" = "#4DAC26",
                 "Savannah" = "#D01C8B", "Overall" = "gray30")

fig1 <- ggplot() +
  geom_col(
    data  = filter(df_shares_long, Metric == "Participation"),
    aes(x = IncomeSource, y = Value, fill = Zone),
    position = position_dodge(width = 0.8), width = 0.7,
    color = "black", alpha = 0.85
  ) +
  geom_line(
    data  = filter(df_shares_long, Metric == "Share"),
    aes(x = IncomeSource, y = Value * 2, group = Zone, color = Zone),
    linewidth = 1.2
  ) +
  geom_point(
    data  = filter(df_shares_long, Metric == "Share"),
    aes(x = IncomeSource, y = Value * 2, shape = Zone, color = Zone),
    size = 3.5, fill = "white", stroke = 1
  ) +
  scale_fill_manual(values = zone_colors,  name = "Participation Rate (%)") +
  scale_color_manual(values = zone_colors, name = "Income Share (%)") +
  scale_shape_manual(values = c("Coastal" = 21, "Forest" = 22,
                                "Savannah" = 24, "Overall" = 23),
                     name = "Income Share (%)") +
  scale_y_continuous(
    name     = "Participation Rate (%)",
    sec.axis = sec_axis(~ . / 2, name = "Income Share (%)"),
    limits   = c(0, 105), expand = c(0, 0),
    breaks   = seq(0, 100, by = 20)
  ) +
  labs(
    title    = "Figure 1. Household Income Participation Rates and Shares by Agroecological Zone",
    subtitle = "Bars = participation rate (left axis); Lines = income share (right axis)",
    x = NULL,
    caption  = "Source: GLSS7 (2016/17). Authors' calculations."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x   = element_text(angle = 30, hjust = 1, size = 10, face = "bold"),
    legend.position = "right",
    plot.title      = element_text(size = 12, face = "bold"),
    plot.background = element_rect(fill = "white", color = NA)
  )

fig1
ggsave("Figure1_Income_Participation_Shares_new.png", fig1, width = 10, height = 6, dpi = 300)


# --- Figure 2: Lorenz Curves & Gini by Zone ---
df$ez_label <- as.character(df$ez)   # ez is already a factor with labels

lorenz_data <- df %>%
  filter(!is.na(welfare), welfare > 0, !is.na(ez_label)) %>%
  {
    zones <- bind_rows(
      group_by(., ez_label) %>%
        group_modify(~ tibble(p = Lc(.x$welfare)$p,
                              L = Lc(.x$welfare)$L)),
      tibble(ez_label = "Overall",
             p = Lc(.$welfare)$p,
             L = Lc(.$welfare)$L)
    )
    zones
  }

gini_summary <- df %>%
  filter(!is.na(welfare), welfare > 0, !is.na(ez_label)) %>%
  {
    bind_rows(
      group_by(., ez_label) %>%
        summarise(Gini = ineq(welfare, type = "Gini")),
      tibble(ez_label = "Overall",
             Gini = ineq(.$welfare, type = "Gini"))
    )
  }

lorenz_colors <- c("Coastal" = "#2166AC", "Forest" = "#4DAC26",
                   "Savannah" = "#D01C8B", "Overall" = "gray30")

fig2 <- ggplot(lorenz_data, aes(x = p, y = L,
                                color    = ez_label,
                                linetype = ez_label)) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, color = "gray50", linetype = "dotted") +
  scale_color_manual(
    values = lorenz_colors,
    labels = function(x)
      paste0(x, " (Gini = ",
             round(gini_summary$Gini[match(x, gini_summary$ez_label)], 3), ")")
  ) +
  scale_linetype_manual(
    values = c("Coastal" = "solid", "Forest" = "solid",
               "Savannah" = "solid", "Overall" = "dashed"),
    labels = function(x)
      paste0(x, " (Gini = ",
             round(gini_summary$Gini[match(x, gini_summary$ez_label)], 3), ")")
  ) +
  labs(
    title    = "Figure 2. Lorenz Curves of Welfare by Agroecological Zone",
    x        = "Cumulative Share of Population",
    y        = "Cumulative Share of Welfare",
    color    = "Zone", linetype = "Zone",
    caption  = "Source: GLSS7 (2016/17). Authors' calculations."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.25, 0.75),
        plot.background  = element_rect(fill = "white", color = NA))

fig2
ggsave("Figure2_Lorenz_Curves_new.png", fig2, width = 8, height = 6, dpi = 300)

# Print Gini coefficients
print(gini_summary)


# --- Figure 3: Welfare Distribution Histograms by Zone (legend moved left) ---

library(ggplot2)
library(dplyr)

# Compute statistics and differences
welfare_stats <- df %>%
  filter(!is.na(welfare)) %>%
  group_by(ez) %>%
  summarise(
    mean_welfare   = mean(welfare, na.rm = TRUE),
    median_welfare = median(welfare, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    diff = mean_welfare - median_welfare,
    # Stagger arrow heights so they don't overlap
    arrow_y = case_when(
      ez == "Coastal"  ~ 800,
      ez == "Forest"   ~ 650,
      ez == "Savannah" ~ 500
    )
  )

# Prepare legend labels: zone name + difference value
legend_labels <- welfare_stats %>%
  mutate(label = sprintf("%s: Δ = %.0f GHS", ez, diff)) %>%
  pull(label)
names(legend_labels) <- c("Coastal", "Forest", "Savannah")

fig3 <- df %>%
  filter(!is.na(welfare)) %>%
  ggplot(aes(x = welfare, fill = ez)) +
  geom_histogram(color = "white", alpha = 0.65, binwidth = 500,
                 position = "identity") +
  
  # Vertical lines: mean (solid) and median (dashed) – no text labels
  geom_vline(data = welfare_stats,
             aes(xintercept = mean_welfare, color = ez),
             linetype = "solid", linewidth = 0.9) +
  geom_vline(data = welfare_stats,
             aes(xintercept = median_welfare, color = ez),
             linetype = "dashed", linewidth = 0.9) +
  
  # Double-headed arrows between median and mean
  geom_segment(
    data = welfare_stats,
    aes(x = median_welfare, xend = mean_welfare,
        y = arrow_y,        yend = arrow_y,
        color = ez),
    arrow = arrow(ends = "both", type = "closed",
                  length = unit(0.2, "cm")),
    linewidth = 0.7
  ) +
  
  # Colour scales – legend shows zone + Δ
  scale_fill_manual(
    values = c("Coastal" = "#2166AC", "Forest" = "#4DAC26", "Savannah" = "#D01C8B"),
    name   = "Zone",
    labels = legend_labels
  ) +
  scale_color_manual(
    values = c("Coastal" = "#2166AC", "Forest" = "#4DAC26", "Savannah" = "#D01C8B"),
    guide  = "none"
  ) +
  
  coord_cartesian(xlim = c(0, 15000), clip = "off") +
  labs(
    title    = "Figure 3. Distribution of Household Welfare by Agroecological Zone",
    subtitle = "Solid lines = mean; Dashed lines = median; Arrows show mean–median gap (Δ)",
    x        = "Welfare (GHS per adult equivalent, deflated)",
    y        = "Number of Households",
    caption  = "Source: GLSS7 (2016/17). Authors' calculations."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = c(0.78, 0.65),     # moved left from 0.85 → 0.78
    legend.justification = c(0, 0.5),
    legend.background = element_rect(fill = "white", color = "gray80", size = 0.3),
    plot.margin       = margin(30, 40, 10, 10),  # reduced right margin slightly
    plot.background   = element_rect(fill = "white", color = NA),
    plot.title        = element_text(face = "bold", size = 12),
    plot.subtitle     = element_text(size = 10, color = "gray40")
  )

fig3
ggsave("Figure3_Welfare_Distribution.png", fig3, width = 10, height = 6, dpi = 300)

# =============================================================================
# SECTION 11: TOBIT (CENSORED) REGRESSION — DETERMINANTS OF SDI (Table 4)
# =============================================================================

# NOTE: 'region' is included to control for district-level heterogeneity.
# pstatus is re-levelled so "Non poor" is the reference category.
df$pstatus <- relevel(factor(df$pstatus), ref = "Non poor")
df$region  <- factor(df$region)

# Shared formula object for reuse
tobit_formula <- SDI ~ AGEY + SEX + female_prop + hhsize + currently_married +
  pstatus + basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  akan + ewe + ga_dangme + guan + gurma + mole_dagbani + grusi + mande +
  christian + muslim + traditionalist +
  REMIT_INC + owns_mobile_phone + access_to_credit + DISABLED + region

# Full (pooled) model with zone dummy
tobit_full <- censReg(
  update(tobit_formula, . ~ . + ez),
  left = 0, data = df
)

# Zone-stratified models
tobit_coastal  <- censReg(tobit_formula, left = 0,
                          data = subset(df, ez == "Coastal"))
tobit_forest   <- censReg(tobit_formula, left = 0,
                          data = subset(df, ez == "Forest"))
tobit_savannah <- censReg(tobit_formula, left = 0,
                          data = subset(df, ez == "Savannah"))

summary(tobit_full)
summary(tobit_coastal)
summary(tobit_forest)
summary(tobit_savannah)

tobit_coastal <- censReg(
  SDI ~ AGEY + SEX + female_prop + hhsize + currently_married + pstatus +
    basic_education + secondary_education + tertiary_education + 
    adult_education + technical_education + akan + ewe + ga_dangme + 
    guan + mole_dagbani + christian + muslim + traditionalist + 
    REMIT_INC + owns_mobile_phone + access_to_credit + DISABLED + region,
  left = 0,
  data = subset(df, ez == "Coastal")
)
summary(tobit_coastal)

# Export side-by-side Tobit table
stargazer(
  tobit_full, tobit_coastal, tobit_forest, tobit_savannah,
  title          = "Table 4. Censored Regression Models for the Simpson Diversity Index (SDI)",
  type           = "text",
  out            = "Table4_Tobit_SDI.html",
  align          = TRUE,
  dep.var.labels = "SDI",
  column.labels  = c("Full Sample", "Coastal", "Forest", "Savannah"),
  covariate.labels = c(
    "Age", "Sex (Male=1)", "Female Proportion", "Household Size",
    "Currently Married",
    "Poverty: Very Poor", "Poverty: Poor",
    "Basic Education", "Secondary Education", "Tertiary Education",
    "Adult Education", "Technical Education",
    "Akan", "Ewe", "Ga-Dangme", "Guan", "Gurma",
    "Mole-Dagbani", "Grusi", "Mande",
    "Christian", "Muslim", "Traditionalist",
    "Remittance Income", "Owns Mobile Phone",
    "Access to Credit", "Disabled",
    "Zone: Forest", "Zone: Savannah"
  ),
  keep.stat = c("n", "ll", "aic"),
  no.space  = TRUE
)


# =============================================================================
# SECTION 12: POISSON REGRESSION — DETERMINANTS OF ACTIVITY COUNT (Table 5)
# =============================================================================

poisson_formula <- activity_count ~ AGEY + SEX + female_prop + hhsize +
  currently_married + pstatus +
  basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  akan + ewe + ga_dangme + guan + gurma + mole_dagbani + grusi + mande +
  christian + muslim + traditionalist +
  REMIT_INC + owns_mobile_phone + access_to_credit + DISABLED + region

# Full (pooled) model with zone dummy
poisson_full <- glm(
  update(poisson_formula, . ~ . + ez),
  family = poisson(link = "log"), data = df
)

# Zone-stratified models
poisson_coastal  <- glm(poisson_formula, family = poisson(link = "log"),
                        data = subset(df, ez == "Coastal"))
poisson_forest   <- glm(poisson_formula, family = poisson(link = "log"),
                        data = subset(df, ez == "Forest"))
poisson_savannah <- glm(poisson_formula, family = poisson(link = "log"),
                        data = subset(df, ez == "Savannah"))

summary(poisson_full)
summary(poisson_coastal)
summary(poisson_forest)
summary(poisson_savannah)

stargazer(
  poisson_full, poisson_coastal, poisson_forest, poisson_savannah,
  title          = "Table 5. Poisson Regression Models for Number of Income Activities",
  type           = "text",
  out            = "Table5_Poisson_ActivityCount.html",
  align          = TRUE,
  dep.var.labels = "Activity Count",
  column.labels  = c("Full Sample", "Coastal", "Forest", "Savannah"),
  covariate.labels = c(
    "Age", "Sex (Male=1)", "Female Proportion", "Household Size",
    "Currently Married",
    "Poverty: Very Poor", "Poverty: Poor",
    "Basic Education", "Secondary Education", "Tertiary Education",
    "Adult Education", "Technical Education",
    "Akan", "Ewe", "Ga-Dangme", "Guan", "Gurma",
    "Mole-Dagbani", "Grusi", "Mande",
    "Christian", "Muslim", "Traditionalist",
    "Remittance Income", "Owns Mobile Phone",
    "Access to Credit", "Disabled",
    "Zone: Forest", "Zone: Savannah"
  ),
  keep.stat = c("n", "ll", "aic"),
  no.space  = TRUE
)


# =============================================================================
# SECTION 13: QUANTILE REGRESSION — SDI EFFECT ON WELFARE (Tables 6 & 7)
# =============================================================================
# Research question: Does the effect of income diversification (SDI) on
# household welfare differ across the welfare distribution (poor vs. non-poor)?
# We use bootstrapped standard errors (se = "boot") throughout.
# =============================================================================

quantiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Core formula: welfare as outcome, SDI as key variable of interest
qr_formula <- welfare ~ SDI + activity_count +
  AGEY + SEX + hhsize + currently_married +
  basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  share_WAGE_HID + share_AGINC_GROSS + share_INCNF_GROSS +
  share_RENT_INC + share_REMIT_INC +
  owns_mobile_phone + electricity_access + access_to_credit +
  DISABLED + ez

# --- 13a. Pooled Quantile Regression (all zones, zone dummies included) ---
qr_pooled <- rq(qr_formula, data = df, tau = quantiles)
summary(qr_pooled, se = "boot", R = 1000)

# --- 13b. Zone-Stratified Quantile Regressions ---
# Remove ez from formula for zone subsets
qr_formula_zone <- welfare ~ SDI + activity_count +
  AGEY + SEX + hhsize + currently_married +
  basic_education + secondary_education + tertiary_education +
  adult_education + technical_education +
  share_WAGE_HID + share_AGINC_GROSS + share_INCNF_GROSS +
  share_RENT_INC + share_REMIT_INC +
  owns_mobile_phone + electricity_access + access_to_credit + DISABLED

qr_coastal  <- rq(qr_formula_zone, data = subset(df, ez == "Coastal"),
                  tau = quantiles)
qr_forest   <- rq(qr_formula_zone, data = subset(df, ez == "Forest"),
                  tau = quantiles)
qr_savannah <- rq(qr_formula_zone, data = subset(df, ez == "Savannah"),
                  tau = quantiles)

summary(qr_coastal,  se = "boot", R = 1000)
summary(qr_forest,   se = "boot", R = 1000)
summary(qr_savannah, se = "boot", R = 1000)


# --- 13c. OLS Benchmark (for comparison) ---
ols_full <- lm(qr_formula, data = df)
summary(ols_full)


# --- 13d. Test for Heterogeneity Across Quantiles (Wald Test) ---
# Tests whether SDI coefficient is significantly different across quantiles
qr_anova <- anova(
  rq(qr_formula, data = df, tau = quantiles),
  test = "Wald", joint = FALSE
)
print(qr_anova)


# --- 13e. Figure 4: SDI Coefficient Across the Welfare Distribution ---
# Extract tidy results for key variables
qr_coef_plot <- map_dfr(
  setNames(quantiles, paste0("tau_", quantiles)),
  ~ tidy(rq(qr_formula, data = df, tau = .x), se.type = "boot", R = 1000),
  .id = "quantile"
) %>%
  mutate(quantile = as.numeric(str_remove(quantile, "tau_")))

# OLS reference line
ols_ref <- tidy(ols_full) %>%
  mutate(model = "OLS")

# Plot SDI and activity_count across quantiles vs OLS
key_vars <- c("SDI", "activity_count",
              "share_AGINC_GROSS", "share_INCNF_GROSS", "share_WAGE_HID")

fig4 <- qr_coef_plot %>%
  filter(term %in% key_vars) %>%
  mutate(term = recode(term,
                       "SDI"               = "Simpson Diversity Index",
                       "activity_count"    = "No. of Activities",
                       "share_AGINC_GROSS" = "Agricultural Income Share",
                       "share_INCNF_GROSS" = "Non-Farm Income Share",
                       "share_WAGE_HID"    = "Wage Income Share"
  )) %>%
  ggplot(aes(x = quantile, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, fill = "#2166AC") +
  geom_line(color = "#2166AC", linewidth = 1.1) +
  geom_point(color = "#2166AC", size = 2.5) +
  # Add OLS reference
  geom_hline(
    data = ols_ref %>%
      filter(term %in% c("SDI", "activity_count",
                         "share_AGINC_GROSS", "share_INCNF_GROSS",
                         "share_WAGE_HID")) %>%
      mutate(term = recode(term,
                           "SDI"               = "Simpson Diversity Index",
                           "activity_count"    = "No. of Activities",
                           "share_AGINC_GROSS" = "Agricultural Income Share",
                           "share_INCNF_GROSS" = "Non-Farm Income Share",
                           "share_WAGE_HID"    = "Wage Income Share"
      )),
    aes(yintercept = estimate),
    linetype = "dashed", color = "#D01C8B", linewidth = 0.9
  ) +
  facet_wrap(~ term, scales = "free_y", ncol = 2) +
  scale_x_continuous(breaks = quantiles,
                     labels = c("Q10\n(Poorest)", "Q25", "Q50\n(Median)",
                                "Q75", "Q90\n(Wealthiest)")) +
  labs(
    title    = "Figure 4. Effect of Income Diversification on Welfare Across the Distribution",
    subtitle = "Blue lines = quantile regression estimates (95% CI shaded); Pink dashed = OLS benchmark",
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

fig4
ggsave("Figure4_Quantile_Coefficients.png", fig4,
       width = 10, height = 8, dpi = 300)


# --- 13f. Export Quantile Regression Table (pooled) ---
stargazer(
  # Pass individual tau models for table columns
  rq(qr_formula, data = df, tau = 0.10),
  rq(qr_formula, data = df, tau = 0.25),
  rq(qr_formula, data = df, tau = 0.50),
  rq(qr_formula, data = df, tau = 0.75),
  rq(qr_formula, data = df, tau = 0.90),
  ols_full,
  title         = "Table 6. Quantile Regression: Effect of SDI on Household Welfare",
  type          = "text",
  out           = "Table6_Quantile_Regression_Pooled.html",
  align         = TRUE,
  dep.var.labels = "Household Welfare (GHS, deflated)",
  column.labels = c("Q10", "Q25", "Q50", "Q75", "Q90", "OLS"),
  covariate.labels = c(
    "SDI (key variable)", "No. of Activities",
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

# =============================================================================
# END OF SCRIPT
# =============================================================================