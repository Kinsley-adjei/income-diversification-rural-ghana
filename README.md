# Income Generation in Rural Ghana
## Welfare, Income Diversification, and Inequality Across Agroecological Zones

**Authors:** Kinsley Delanyo Adjei & Yaw Bonsu Osei-Asare  
**Data:** Ghana Living Standards Survey 7 (GLSS7, 2016/17)  
**Target Journal:** World Development  

---

## Overview

This repository contains the full replication code for our study on income diversification and household welfare among rural households in Ghana. Using the Ghana Living Standards Survey 7 (GLSS7), we analyse how income diversification strategies and welfare outcomes vary across three agroecological zones — Coastal, Forest, and Savannah — and examine the role of supply chain participation in shaping these outcomes.

---

## Research Questions

1. How does income diversification vary across agroecological zones in rural Ghana?
2. What are the welfare returns to income diversification, and do they differ across the welfare distribution?
3. Does supply chain position mediate the relationship between diversification and welfare?
4. Is there spatial clustering in income diversification patterns across Ghanaian districts?

---

## Methods

| Method | Purpose |
|---|---|
| Simpson Diversity Index (SDI) | Measure of income diversification |
| Tobit (Censored) Regression | Determinants of SDI |
| Poisson Regression | Determinants of number of income activities |
| Quantile Regression | Heterogeneous welfare returns to diversification |
| Multinomial Logit | Determinants of supply chain participation |
| Spatial Autocorrelation (Moran's I) | Spatial clustering of diversification |
| LISA Cluster Maps | Local spatial patterns |

---

## Repository Structure

```
income-diversification-rural-ghana/
│
├── income_Ghana.R                  # Main analysis: data cleaning, variable
│                                   # construction, descriptive stats, 
│                                   # Tobit, Poisson, Quantile regression
│
├── supply_chain_analysis.R         # Supply chain classification and analysis:
│                                   # Options 1 (QR + supply chain),
│                                   # Option 2 (Multinomial logit),
│                                   # Option 3 (SDI × supply chain interaction)
│
├── spatial_analysis_final.R        # Spatial analysis: choropleth maps,
│                                   # Moran's I, LISA cluster maps,
│                                   # Moran scatter plots
│
├── data/
│   └── Map_of_Districts_216.shp   # Ghana 216-district shapefile (10 regions)
│   └── (+ supporting shapefile files)
│
└── outputs/                        # Generated figures and tables
```

---

## Data

The analysis uses the **Ghana Living Standards Survey 7 (GLSS7)**, collected by the Ghana Statistical Service (GSS) in 2016/17. The microdata is not included in this repository as it is subject to GSS data access restrictions.

The data can be accessed at: [Ghana Statistical Service](https://statsghana.gov.gh/)

The district shapefile (`Map_of_Districts_216`) reflects Ghana's 10-region, 216-district administrative structure as at 2016/17, consistent with the GLSS7 survey design.

---

## Key R Packages

```r
# Data wrangling
library(haven)       # Read .dta files
library(tidyverse)   # Data manipulation
library(data.table)  # Fast data operations

# Econometrics
library(censReg)     # Tobit regression
library(AER)         # IV and econometric tools
library(quantreg)    # Quantile regression
library(nnet)        # Multinomial logit

# Tables
library(gtsummary)   # Summary statistics
library(stargazer)   # Regression tables
library(flextable)   # Export to Word

# Spatial
library(sf)          # Spatial data handling
library(spdep)       # Spatial autocorrelation
library(ggspatial)   # Map annotations
library(viridis)     # Colour scales

# Visualisation
library(ggplot2)
library(patchwork)
library(ineq)        # Gini and Lorenz curves
```

---

## How to Replicate

1. Obtain GLSS7 microdata from the Ghana Statistical Service
2. Clone this repository
3. Set your working directory to the project folder
4. Run scripts in this order:
   - `income_Ghana.R`
   - `supply_chain_analysis.R`
   - `spatial_analysis_final.R`

---

## Contact

**Kinsley Delanyo Adjei**  
MPhil Agricultural Economics, University of Ghana  
Research Assistant, University of Zurich (Prof. Dina Pomeranz Group)  
📧 kinsley.dadjei@gmail.com  
🔗 [LinkedIn](https://linkedin.com/in/kinsley-adjei)

---

*This research is part of an ongoing study on rural livelihoods and income diversification in Sub-Saharan Africa.*
