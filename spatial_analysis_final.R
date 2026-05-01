# =============================================================================
# SPATIAL ANALYSIS — Using Ghana 216-District Shapefile (Old 10-Region Structure)
# Paper: Income Generation in Rural Ghana
# Authors: Kinsley Delanyo Adjei & Yaw Bonsu Osei-Asare
# =============================================================================
# Shapefile: Map_of_Districts_216.shp
#   - 216 districts, 10 regions — matches GLSS7 (2016/17) exactly
#   - Key columns: NAME (district), RGN_NM2012 (region), ID (district code)
# =============================================================================

library(sf)
library(spdep)
library(viridis)
library(patchwork)
library(RColorBrewer)
library(ggspatial)
library(tidyverse)
library(flextable)
library(officer)


# =============================================================================
# SECTION A: LOAD SHAPEFILE
# All 5 shapefile components must be in the same folder.
# Change path below to wherever you saved the shapefile files.
# =============================================================================

shp_path <- "Map_of_Districts_216.shp"   # <-- UPDATE PATH if needed

ghana_district <- st_read(shp_path, quiet = TRUE) %>%
  st_transform(crs = 4326)   # reproject to WGS84 for ggplot2 compatibility

cat("Shapefile loaded:", nrow(ghana_district), "districts\n")
cat("Regions:", paste(sort(unique(ghana_district$RGN_NM2012)), collapse = ", "), "\n")


# =============================================================================
# SECTION B: REGION LOOKUP — Map GLSS7 numeric codes to shapefile region names
# GLSS7 region codes (1–10) match RGN_NM2012 in the shapefile
# =============================================================================

region_lookup <- tibble(
  region_code = 1:10,
  RGN_NM2012  = c(
    "Western",       # 1
    "Central",       # 2
    "Greater Accra", # 3
    "Volta",         # 4
    "Eastern",       # 5
    "Ashanti",       # 6
    "Brong Ahafo",   # 7
    "Northern",      # 8
    "Upper East",    # 9
    "Upper West"     # 10
  ),
  ez_zone = c(
    "Forest",    # Western
    "Coastal",   # Central
    "Coastal",   # Greater Accra
    "Coastal",   # Volta
    "Forest",    # Eastern
    "Forest",    # Ashanti
    "Forest",    # Brong Ahafo
    "Savannah",  # Northern
    "Savannah",  # Upper East
    "Savannah"   # Upper West
  )
)

# Add region code and zone to district shapefile
ghana_district <- ghana_district %>%
  left_join(region_lookup, by = "RGN_NM2012")

# Dissolve districts to region level
ghana_region <- ghana_district %>%
  group_by(RGN_NM2012, region_code, ez_zone) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

cat("Region boundaries created:", nrow(ghana_region), "regions\n")


# =============================================================================
# SECTION C: AGGREGATE GLSS7 TO REGION & DISTRICT LEVEL
# =============================================================================

# --- C1: Region level ---
df_region <- df %>%
  filter(!is.na(region), !is.na(welfare), !is.na(SDI)) %>%
  group_by(region) %>%
  summarise(
    mean_welfare   = mean(welfare,        na.rm = TRUE),
    median_welfare = median(welfare,      na.rm = TRUE),
    mean_SDI       = mean(SDI,            na.rm = TRUE),
    mean_activity  = mean(activity_count, na.rm = TRUE),
    pct_poor       = mean(worldpoor,      na.rm = TRUE) * 100,
    pct_nonfarm    = mean(share_INCNF_GROSS, na.rm = TRUE) * 100,
    pct_ag         = mean(share_AGINC_GROSS, na.rm = TRUE) * 100,
    n_households   = n(),
    .groups = "drop"
  ) %>%
  rename(region_code = region)

# --- C2: District level ---
# NOTE: Check your GLSS7 district variable name — common names: district, dist, DIST
# The shapefile district ID column is 'ID' (3-digit codes e.g. 101, 102 ...)
# If your GLSS7 district codes match these IDs, use them directly.
# If your GLSS7 has district names instead of codes, join on NAME.

district_var <- "district"   # <-- CHANGE to your actual GLSS7 district variable name

df_district <- df %>%
  filter(!is.na(.data[[district_var]]), !is.na(welfare), !is.na(SDI)) %>%
  group_by(region, !!sym(district_var)) %>%
  summarise(
    mean_welfare   = mean(welfare,           na.rm = TRUE),
    mean_SDI       = mean(SDI,               na.rm = TRUE),
    mean_activity  = mean(activity_count,    na.rm = TRUE),
    pct_poor       = mean(worldpoor,         na.rm = TRUE) * 100,
    pct_nonfarm    = mean(share_INCNF_GROSS, na.rm = TRUE) * 100,
    n_households   = n(),
    .groups = "drop"
  ) %>%
  rename(region_code = region, district_id = !!sym(district_var))


# =============================================================================
# SECTION D: MERGE WITH SHAPEFILE
# =============================================================================

# --- D1: Region merge ---
ghana_region_merged <- ghana_region %>%
  left_join(df_region, by = "region_code")

# Check
unmatched_r <- ghana_region_merged %>% filter(is.na(mean_welfare)) %>% pull(RGN_NM2012)
if (length(unmatched_r) > 0) {
  cat("WARNING — Unmatched regions:", paste(unmatched_r, collapse = ", "), "\n")
} else {
  cat("All 10 regions matched.\n")
}

# --- D2: District merge ---
# Join on district ID (adjust if your codes differ from shapefile ID column)
ghana_district_merged <- ghana_district %>%
  left_join(df_district, by = c("ID" = "district_id"))
# If your district codes don't match shapefile IDs, try joining on NAME:
# ghana_district_merged <- ghana_district %>%
#   left_join(df_district_named, by = c("NAME" = "district_name"))

matched_d <- sum(!is.na(ghana_district_merged$mean_welfare))
cat("District merge:", matched_d, "of", nrow(ghana_district), "districts matched.\n")
cat("(Unmatched districts will show as gray on map — check district codes)\n")


# =============================================================================
# SECTION E: MAP 1 — AGROECOLOGICAL ZONES (Figure 7)
# =============================================================================

zone_colors <- c("Coastal" = "#2166AC", "Forest" = "#4DAC26", "Savannah" = "#D01C8B")

fig7 <- ggplot() +
  geom_sf(data = ghana_region_merged,
          aes(fill = ez_zone), color = "white", linewidth = 0.6) +
  geom_sf(data = ghana_district,
          fill = NA, color = "white", linewidth = 0.15, alpha = 0.5) +
  geom_sf_label(
    data = ghana_region_merged,
    aes(label = RGN_NM2012),
    size = 2.5, fontface = "bold", label.size = NA, fill = NA
  ) +
  scale_fill_manual(values = zone_colors, name = "Agroecological Zone",
                    na.value = "gray90") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(
    title    = "Figure 7. Agroecological Zones of Ghana",
    subtitle = "216 district boundaries shown; shading by agroecological zone",
    caption  = "Source: Ghana Statistical Service (2016/17)."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "gray40"),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

fig7
ggsave("Figure7_Agroecological_Zones.png", fig7, width = 8, height = 9, dpi = 300)
cat("Figure 7 saved.\n")


# =============================================================================
# SECTION F: MAP 2 — WELFARE & POVERTY BY REGION + DISTRICT (Figure 8)
# =============================================================================

# Regional welfare
map_welfare_region <- ggplot() +
  geom_sf(data = ghana_region_merged,
          aes(fill = mean_welfare), color = "white", linewidth = 0.5) +
  geom_sf(data = ghana_district, fill = NA, color = "white",
          linewidth = 0.1, alpha = 0.4) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Mean Welfare\n(GHS, deflated)",
                       labels = scales::comma, na.value = "gray90") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  labs(title = "A. Mean Welfare — Regional") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

# District welfare (finer detail)
map_welfare_district <- ggplot() +
  geom_sf(data = ghana_district_merged,
          aes(fill = mean_welfare), color = "white", linewidth = 0.1) +
  geom_sf(data = ghana_region, fill = NA, color = "gray30", linewidth = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "Mean Welfare\n(GHS, deflated)",
                       labels = scales::comma, na.value = "gray90") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  labs(title = "B. Mean Welfare — District") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

fig8 <- map_welfare_region + map_welfare_district +
  plot_annotation(
    title   = "Figure 8. Spatial Distribution of Household Welfare Across Ghana",
    caption = "Source: GLSS7 (2016/17). Authors' calculations.",
    theme   = theme(
      plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

fig8
ggsave("Figure8_Welfare_Maps.png", fig8, width = 14, height = 7, dpi = 300)
cat("Figure 8 saved.\n")


# =============================================================================
# SECTION G: MAP 3 — SDI BY REGION + DISTRICT (Figure 9)
# =============================================================================

map_sdi_region <- ggplot() +
  geom_sf(data = ghana_region_merged,
          aes(fill = mean_SDI), color = "white", linewidth = 0.5) +
  geom_sf(data = ghana_district, fill = NA, color = "white",
          linewidth = 0.1, alpha = 0.4) +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name = "Mean SDI", limits = c(0, 1),
                       na.value = "gray90") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal,
                         height = unit(0.8,"cm"), width = unit(0.8,"cm")) +
  labs(title = "A. Income Diversification (SDI) — Regional") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

map_sdi_district <- ggplot() +
  geom_sf(data = ghana_district_merged,
          aes(fill = mean_SDI), color = "white", linewidth = 0.1) +
  geom_sf(data = ghana_region, fill = NA, color = "gray30", linewidth = 0.5) +
  scale_fill_viridis_c(option = "plasma", direction = -1,
                       name = "Mean SDI", limits = c(0, 1),
                       na.value = "gray90") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  labs(title = "B. Income Diversification (SDI) — District") +
  theme_void(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

fig9 <- map_sdi_region + map_sdi_district +
  plot_annotation(
    title   = "Figure 9. Spatial Distribution of Income Diversification Across Ghana",
    caption = "Source: GLSS7 (2016/17). Authors' calculations.",
    theme   = theme(
      plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  )

fig9
ggsave("Figure9_SDI_Maps.png", fig9, width = 14, height = 7, dpi = 300)
cat("Figure 9 saved.\n")


# =============================================================================
# SECTION H: MAP 4 — NON-FARM INCOME SHARE BY DISTRICT (Figure 10)
# =============================================================================

fig10 <- ggplot() +
  geom_sf(data = ghana_district_merged,
          aes(fill = pct_nonfarm), color = "white", linewidth = 0.1) +
  geom_sf(data = ghana_region, fill = NA, color = "gray20", linewidth = 0.6) +
  geom_sf_label(
    data = ghana_region_merged,
    aes(label = RGN_NM2012),
    size = 2.3, fontface = "bold", label.size = NA, fill = NA
  ) +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       name = "Non-Farm\nIncome Share (%)",
                       na.value = "gray90") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal) +
  labs(
    title    = "Figure 10. Non-Farm Income Share by District",
    subtitle = "Regional boundaries overlaid",
    caption  = "Source: GLSS7 (2016/17). Authors' calculations."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

fig10
ggsave("Figure10_NonFarm_District_Map.png", fig10, width = 8, height = 9, dpi = 300)
cat("Figure 10 saved.\n")


# =============================================================================
# SECTION I: SPATIAL AUTOCORRELATION — MORAN'S I (Table 9)
# Run at DISTRICT level (216 obs) for more statistical power than 10 regions
# =============================================================================

# Use district-level merged data — drop rows with no welfare data
ghana_dist_complete <- ghana_district_merged %>%
  filter(!is.na(mean_SDI), !is.na(mean_welfare))

cat("\nDistricts with data for Moran's I:", nrow(ghana_dist_complete), "\n")

# Spatial weights
nb_dist    <- poly2nb(as(ghana_dist_complete, "Spatial"), queen = TRUE)
listw_dist <- nb2listw(nb_dist, style = "W", zero.policy = TRUE)

# Global Moran's I
moran_sdi     <- moran.test(ghana_dist_complete$mean_SDI,     listw_dist, zero.policy = TRUE)
moran_welfare <- moran.test(ghana_dist_complete$mean_welfare, listw_dist, zero.policy = TRUE)
moran_poor    <- moran.test(ghana_dist_complete$pct_poor,     listw_dist, zero.policy = TRUE)

cat("\n--- Moran's I: SDI ---");     print(moran_sdi)
cat("\n--- Moran's I: Welfare ---"); print(moran_welfare)
cat("\n--- Moran's I: Poverty ---"); print(moran_poor)

# Export Table 9
tibble(
  Variable  = c("SDI", "Mean Welfare", "Poverty Rate"),
  `Moran's I` = round(c(moran_sdi$estimate[1],
                         moran_welfare$estimate[1],
                         moran_poor$estimate[1]), 4),
  Expected    = round(c(moran_sdi$estimate[2],
                         moran_welfare$estimate[2],
                         moran_poor$estimate[2]), 4),
  `p-value`   = round(c(moran_sdi$p.value,
                         moran_welfare$p.value,
                         moran_poor$p.value), 4),
  Interpretation = case_when(
    c(moran_sdi$p.value, moran_welfare$p.value, moran_poor$p.value) < 0.01 ~ "Strong clustering (p<0.01)",
    c(moran_sdi$p.value, moran_welfare$p.value, moran_poor$p.value) < 0.05 ~ "Clustering (p<0.05)",
    TRUE ~ "No significant clustering"
  )
) %>%
  flextable() %>%
  set_caption("Table 9. Global Moran's I — Spatial Autocorrelation (District Level, n=216)") %>%
  add_footer_lines("Note: Queen contiguity spatial weights. H0: No spatial autocorrelation.") %>%
  autofit() %>%
  save_as_docx(path = "Table9_Morans_I.docx")

cat("Table 9 saved.\n")


# =============================================================================
# SECTION J: LISA CLUSTER MAP — SDI (Figure 11)
# =============================================================================

local_moran_sdi <- localmoran(ghana_dist_complete$mean_SDI, listw_dist, zero.policy = TRUE)

ghana_dist_complete <- ghana_dist_complete %>%
  mutate(
    local_moran_p = local_moran_sdi[, 5],
    sdi_std       = scale(mean_SDI)[, 1],
    lag_sdi_std   = lag.listw(listw_dist, scale(mean_SDI)[, 1], zero.policy = TRUE),
    lisa_cluster  = case_when(
      sdi_std > 0 & lag_sdi_std > 0 & local_moran_p < 0.05 ~ "High-High",
      sdi_std < 0 & lag_sdi_std < 0 & local_moran_p < 0.05 ~ "Low-Low",
      sdi_std > 0 & lag_sdi_std < 0 & local_moran_p < 0.05 ~ "High-Low",
      sdi_std < 0 & lag_sdi_std > 0 & local_moran_p < 0.05 ~ "Low-High",
      TRUE ~ "Not significant"
    )
  )

lisa_colors <- c("High-High" = "#D01C8B", "Low-Low" = "#2166AC",
                 "High-Low"  = "#F4A582", "Low-High" = "#92C5DE",
                 "Not significant" = "gray90")

fig11 <- ggplot() +
  geom_sf(data = ghana_dist_complete,
          aes(fill = lisa_cluster), color = "white", linewidth = 0.1) +
  geom_sf(data = ghana_region, fill = NA, color = "gray20", linewidth = 0.6) +
  geom_sf_label(
    data = ghana_region_merged,
    aes(label = RGN_NM2012),
    size = 2.3, label.size = NA, fill = NA, fontface = "bold"
  ) +
  scale_fill_manual(values = lisa_colors, name = "LISA Cluster") +
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "tr", style = north_arrow_minimal) +
  labs(
    title    = "Figure 11. LISA Cluster Map — Income Diversification (SDI)",
    subtitle = "District-level Local Indicators of Spatial Association (p < 0.05)",
    caption  = "Source: GLSS7 (2016/17). Queen contiguity weights."
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "gray40"),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

fig11
ggsave("Figure11_LISA_SDI_District.png", fig11, width = 8, height = 9, dpi = 300)
cat("Figure 11 saved.\n")


# =============================================================================
# SECTION K: MORAN SCATTER PLOT (Figure 12)
# =============================================================================

fig12 <- ghana_dist_complete %>%
  st_drop_geometry() %>%
  ggplot(aes(x = sdi_std, y = lag_sdi_std)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(aes(color = lisa_cluster), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9) +
  scale_color_manual(values = lisa_colors, name = "LISA Cluster") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Moran's I = ", round(moran_sdi$estimate[1], 3),
                          "\np = ", round(moran_sdi$p.value, 3)),
           hjust = 1.1, vjust = 1.5, size = 3.5, fontface = "italic") +
  labs(
    title   = "Figure 12. Moran Scatter Plot — Income Diversification (SDI)",
    x       = "Standardised SDI (district)",
    y       = "Spatially Lagged SDI",
    caption = "Source: GLSS7 (2016/17). n = 216 districts."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    legend.position = "right",
    plot.background = element_rect(fill = "white", color = NA)
  )

fig12
ggsave("Figure12_Moran_Scatter_District.png", fig12, width = 8, height = 7, dpi = 300)
cat("Figure 12 saved.\n")


# =============================================================================
# DONE
# =============================================================================
cat("\n========================================\n")
cat("All spatial outputs complete:\n")
cat("  Figures 7–12 saved as .png\n")
cat("  Table 9 saved as .docx\n")
cat("========================================\n")
