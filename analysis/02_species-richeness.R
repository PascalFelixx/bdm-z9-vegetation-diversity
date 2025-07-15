# ------------------------------------------------------------------------------
# Script Name: 02_statistical-analysis.R
# Purpose:     Perform statistical analysis on BDM Z9 vegetation diversity trends.
#              Analyses include temporal changes in plant species 
#              occurrence, trend significance testing, and group-wise comparisons.
# Author:      Pascal Felix
# Date:        2025-04-01
# ------------------------------------------------------------------------------

# ==============================================================================
# Linear Mixed Models (LMM) Plants
# ==============================================================================

# Libraries
library(tidyverse)     # Data manipulation, visualization
library(glmmTMB)       # Fitting generalized linear (mixed) models
library(DHARMa)        # Diagnostic tools for residual checks
library(ggeffects)     # Creating predicted values from models
library(emmeans)       # Estimated marginal means and trends
library(car)           # Companion to Applied Regression – used for Anova()
library(performance)   # Model quality metrics

# Custom plot theme (defined in separate script)
source("analysis/00_mytheme.R")  # Loads 'mytheme' for consistent ggplot styling

# ==============================================================================
# Define File Path
# ==============================================================================

# Set working directory // Adjust to your machine
setwd("C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity")

# ==============================================================================
# Load Processed Data
# ==============================================================================

# Load harmonized and grouped species data for mosses and plants,
# along with plot metadata containing survey year and environmental variables.
moss <- read.csv("data/processed/moss_species_grouped.csv")
plants <- read.csv("data/processed/plant_species_grouped.csv")
kopfdaten <- read.csv("data/processed/kopfdaten_clean.csv")

# Quick preview of data structure
glimpse(moss)
glimpse(plants)
glimpse(kopfdaten)

# ==============================================================================
# Species Richness Models – Plants
# ==============================================================================

# 1. Calculate species richness per survey (aID_KD)
#    Each survey (plot × year) is summarized by counting unique species.
plant_richness <- plants %>%
  group_by(aID_KD) %>%
  summarise(species_richness = n_distinct(grouped_name))

# 2. Merge with plot metadata and prepare variables for modeling
#    year_index represents time since 2001 (year 2001 = 1).
#    Only surveys with valid year and STAO ID are kept.
plant_data <- plant_richness %>%
  left_join(kopfdaten, by = "aID_KD") %>%
  mutate(
    year_index = yearPl - 2000  # Year 2001 = 1
  ) %>%
  filter(!is.na(year_index), !is.na(aID_STAO))

# Ensure categorical variables are treated as factors
plant_data <- plant_data %>%
  mutate(
    HN = as.factor(HN),
    BGR_6 = as.factor(BGR_6),
    elevation_group = as.factor(if_else(Hoehe > 1200, "high", "low"))
  )

# ==============================================================================
# Linear Mixed Models (LMM) using glmmTMB – Species Richness of Plants
# ==============================================================================

# ------------------------------------------------------------------------------
# Model 0: Simple model with year only (baseline)
# ------------------------------------------------------------------------------

# This model estimates the effect of time on species richness,
# with a random intercept for each survey location (aID_STAO).
model_year_only <- glmmTMB(
  species_richness ~ year_index + (1 | aID_STAO),
  data = plant_data,
  family = gaussian()
)

Anova(model_year_only)
summary(model_year_only)

r2(model_year_only)
r2_year_only <- r2(model_year_only)[["R2m"]]

# Model diagnostics
simulation_year_only <- simulateResiduals(model_year_only)
plot(simulation_year_only)


# Using ggpredict() from the ggeffects package to extract predicted values 
# for the fixed effect 'year_index' from the fitted model.
# This provides predicted species richness values across the full range of years.
pred_data <- ggpredict(model_year_only, terms = "year_index")

# Plot: Observations and predicted trend
ggplot() +
  geom_point(data = plant_data, aes(x = year_index + 2000, y = species_richness), 
             color = "grey40", alpha = 0.5) +
  geom_line(data = pred_data, aes(x = x + 2000, y = predicted), 
            color = "red", size = 1.2) +
  labs(x = "Year", y = "Species richness per 10 m²") +
  scale_x_continuous(
    breaks = seq(2001, 2023, by = 2),
    expand = c(0, 0)
  ) +
  mytheme + 
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1))

ggsave("outputs/figures/02_richness_trend_year_only.png", width = 8, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# Model 1: Year × Biogeographic Region interaction
# ------------------------------------------------------------------------------

# This model investigates whether species richness trends over time
# differ across biogeographic regions (BGR_6).
model_bgr_interaction <- glmmTMB(
  species_richness ~ year_index * BGR_6 + (1 | aID_STAO),
  data = plant_data,
  family = gaussian()
)

Anova(model_bgr_interaction)
summary(model_bgr_interaction)

r2(model_bgr_interaction)

r2_bgr <- r2(model_bgr_interaction)[["R2m"]]

# Model diagnostics
simulation_bgr <- simulateResiduals(model_bgr_interaction)
plot(simulation_bgr)

# Using ggeffect() instead of ggpredict() to generate marginal predictions
# for the interaction term. This avoids dependency on the reference category 
# (Alpennordflanke) and provides absolute predicted values for each biogeographic 
# region, making the interaction plot easier to interpret and compare across groups.

# Use ggeffect instead of ggpredict to obtain marginal predicted values
pred_data_bgr <- ggeffect(model_bgr_interaction, terms = c("year_index", "BGR_6"))

# Compute estimated trends (slopes) per region using emmeans
slopes_bgr <- emtrends(model_bgr_interaction, ~ BGR_6, var = "year_index")
summary(slopes_bgr)

# Manually create significance labels based on model output
labels_bgr <- c(
  "Northern Alps",
  "Southern Alps",
  "Jura",
  "Central Plateau",
  "Eastern Central Alps",
  "Western Central Alps"
)

# Assign labels to the prediction data for plotting
pred_data_bgr$label <- labels_bgr[match(pred_data_bgr$group, unique(pred_data_bgr$group))]

# Visualize Year × Biogeographic Region Interaction
ggplot(pred_data_bgr, aes(x = x + 2000, y = predicted, color = label)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Species richness per 10 m²",
    color = "Biogeographic region"
  ) +
  scale_x_continuous(
    breaks = seq(2001, 2023, by = 2),
    expand = c(0, 0)
  ) +
  ylim(0, max(pred_data_bgr$predicted)) + 
  mytheme +
  theme(
    axis.text.x    = element_text(size = 9, angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box      = "vertical",
    legend.text     = element_text(size = 9),
    legend.title    = element_text(size = 10),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  guides(color = guide_legend(ncol = 2, title.position = "top"))

ggsave("outputs/figures/02_richness_trend_by_bioregion.png", width = 8, height = 5.5, dpi = 300)

# ------------------------------------------------------------------------------
# Model 2: Year × Land Use interaction (HN)
# ------------------------------------------------------------------------------

# This model assesses if temporal trends in species richness vary
# depending on the dominant land use type (HN).
model_hn_interaction <- glmmTMB(
  species_richness ~ year_index * HN + (1 | aID_STAO),
  data = plant_data,
  family = gaussian()
)

Anova(model_hn_interaction)
summary(model_hn_interaction)

r2(model_hn_interaction)

r2_hn    <- r2(model_hn_interaction)[["R2m"]]

# Model diagnostics
simulation_hn <- simulateResiduals(model_hn_interaction)
plot(simulation_hn)

# Use ggeffect to obtain marginal predicted values for interaction
pred_data_hn <- ggeffect(model_hn_interaction, terms = c("year_index", "HN"))

# Compute estimated trends (slopes) per land use category
slopes_hn <- emtrends(model_hn_interaction, ~ HN, var = "year_index")
summary(slopes_hn)

# Based on output from emtrends, manually create significance labels
labels_hn <- c(
  "Arable land",              # Aecker
  "Alpine pastures",          # Alpweiden
  "Non productive areas",     # Nicht genutzte Flaechen
  "Settlements",              # Siedlung
  "Forests",                  # Wald
  "Grasslands"                # Wiesen, Weiden
)

# Assign labels to the prediction data for plotting
pred_data_hn$label <- labels_hn[match(pred_data_hn$group, unique(pred_data_hn$group))]

# Visualize Year × Land Use Interaction with significance in legend
ggplot(pred_data_hn, aes(x = x + 2000, y = predicted, color = label)) +
  geom_line(size = 1.2) +
  labs(x = "Year", 
       y = "Species richness per 10 m²",
       color = NULL) +
  scale_x_continuous(
    breaks = seq(2001, 2023, by = 2),
    expand = c(0, 0)
  ) +
  ylim(0, max(pred_data_hn$predicted)) +  
  mytheme +
  theme(
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  guides(color = guide_legend(ncol = 2, title.position = "top"))

ggsave("outputs/figures/02_richness_trend_by_landuse.png", width = 8, height = 5.5, dpi = 300)

# ------------------------------------------------------------------------------
# Model 4: Year × Elevation Band Interaction
# ------------------------------------------------------------------------------

# Define elevation bands (400m steps)
plant_data <- plant_data %>%
  mutate(elevation_band = cut(
    Hoehe,
    breaks = c(200, 600, 1000, 1400, 1800, 2200, 2600, 3100),
    labels = c("200-600", "600-1000", "1000-1400", "1400-1800", "1800-2200", "2200-2600", "2600-3100"),
    right = FALSE
  ))

# ------------------------------------------------------------------------------
# Overview: Sample size per elevation band
# ------------------------------------------------------------------------------

# Count observations per elevation band
elevation_counts <- plant_data %>%
  count(elevation_band) %>%
  arrange(elevation_band)

# Print table to console
print(elevation_counts)

# Formatted n-labels for the legend later:
labels_band_n <- paste0(elevation_counts$elevation_band, " m a.s.l. (n = ", elevation_counts$n, ")")

# ------------------------------------------------------------------------------
# Barplot for sample sizes
# ------------------------------------------------------------------------------

# Quick barplot of number of observations per elevation band
ggplot(elevation_counts, aes(x = elevation_band, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    x = "Elevation band (m a.s.l.)",
    y = "Number of observations"
  ) +
  mytheme +
  theme(
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
  )

ggsave("outputs/figures/02_sample_sizes_by_elevation_band.png", width = 7, height = 5, dpi = 300)

# Build the interaction model with elevation bands
model_elevation_band <- glmmTMB(
  species_richness ~ year_index * elevation_band + (1 | aID_STAO),
  data = plant_data,
  family = gaussian()
)

# Model evaluation
Anova(model_elevation_band)
summary(model_elevation_band)

r2(model_elevation_band)

r2_band  <- r2(model_elevation_band)[["R2m"]]

# Model diagnostics
simulation_band <- simulateResiduals(model_elevation_band)
plot(simulation_band)

# Generate predicted values using ggeffect()
pred_data_band <- ggeffect(model_elevation_band, terms = c("year_index", "elevation_band"))

# Compute estimated slopes per band using emtrends()
slopes_band <- emtrends(model_elevation_band, ~ elevation_band, var = "year_index")
summary(slopes_band)

# Manually enter the significance labels:
labels_band <- c(
  "200 – 600 m a.s.l.",
  "600 – 1000 m a.s.l.",
  "1000 – 1400 m a.s.l.",
  "1400 – 1800 m a.s.l.",
  "1800 – 2200 m a.s.l.",
  "2200 – 2600 m a.s.l.",
  "2600 – 3100 m a.s.l."
)

# Add labels to prediction data
pred_data_band$label <- labels_band[match(pred_data_band$group, unique(pred_data_band$group))]

# Plot the interaction with significance labels
# Plot the interaction with significance labels
ggplot(pred_data_band, aes(x = x + 2000, y = predicted, color = label)) +
  geom_line(size = 1.2) +
  labs(
    x = "Year", 
    y = "Species richness per 10 m²",
    color = "Elevation band"
  ) +
  scale_x_continuous(
    breaks = seq(2001, 2023, by = 2),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = c(
      "200 – 600 m a.s.l."   = "#1b9e77", 
      "600 – 1000 m a.s.l."  = "#d95f02", 
      "1000 – 1400 m a.s.l." = "#7570b3",
      "1400 – 1800 m a.s.l." = "#e7298a",
      "1800 – 2200 m a.s.l." = "#66a61e",
      "2200 – 2600 m a.s.l." = "#e6ab02",
      "2600 – 3100 m a.s.l." = "#a6761d"
    ),
    breaks = c(
      "200 – 600 m a.s.l.", "600 – 1000 m a.s.l.", "1000 – 1400 m a.s.l.",
      "1400 – 1800 m a.s.l.", "1800 – 2200 m a.s.l.",
      "2200 – 2600 m a.s.l.", "2600 – 3100 m a.s.l."
    )
  ) +
  coord_cartesian(ylim = c(0, 40)) +
  mytheme +
  theme(
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.spacing.y = unit(0.2, "cm")
  ) +
  guides(color = guide_legend(ncol = 2, title.position = "top"))


ggsave("outputs/figures/02_richness_trend_by_elevation_band.png", width = 8, height = 5.5, dpi = 300)

# ==============================================================================
# Model Comparison – Summary of AIC, Year Effects, and Significance
# ==============================================================================

# Create a summary table to compare model fit and temporal trends across models

model_info <- tibble(
  model = c("Year only", "BGR", "HN", "Elevation (banded)"),
  
  AIC = c(
    AIC(model_year_only),
    AIC(model_bgr_interaction),
    AIC(model_hn_interaction),
    AIC(model_elevation_band)
  ),
  
  year_effect = c(
    fixef(model_year_only)$cond["year_index"],
    fixef(model_bgr_interaction)$cond["year_index"],
    fixef(model_hn_interaction)$cond["year_index"],
    fixef(model_elevation_band)$cond["year_index"]
  ),
  r2_marginal = c(
    r2_year_only,
    r2_bgr,
    r2_hn,
    r2_band
  )
)

# Print table to console
print(model_info)

# Add a logical column to indicate if the year effect is statistically significant (p < 0.05)
model_info <- model_info %>%
  mutate(
    year_significant = c(
      summary(model_year_only)$coefficients$cond["year_index", "Pr(>|z|)"] < 0.05,
      summary(model_bgr_interaction)$coefficients$cond["year_index", "Pr(>|z|)"] < 0.05,
      summary(model_hn_interaction)$coefficients$cond["year_index", "Pr(>|z|)"] < 0.05,
      summary(model_elevation_band)$coefficients$cond["year_index", "Pr(>|z|)"] < 0.05
    )
  )

# Export the model summary table as CSV for documentation or reporting
write_csv(model_info, "outputs/tables/model_summary_species_richness.csv")

