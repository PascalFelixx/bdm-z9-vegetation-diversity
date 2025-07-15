# ------------------------------------------------------------------------------
# Script Name: 05_cm-indicator-values.R
# Purpose:     Calculate Community Means (CWM) for ecological indicator 
#              values (EIV) based on processed BDM species data and matched 
#              Flora indicativa traits.
# Author:      Pascal Felix
# Date:        2025-05-08
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Load required libraries and data
# ==============================================================================

library(tidyverse)
library(FD)
library(ggplot2)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(emmeans)
library(ggeffects)

# Custom plot theme (defined in separate script)
source("analysis/00_mytheme.R")  # Loads 'mytheme' for consistent ggplot styling

# Set working directory // Adjust to your machine
setwd("C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity")

# ==============================================================================
# 2. Import processed species × plot table (plant_species_grouped.csv)
# ==============================================================================

plant_species_grouped <- read_csv("data/processed/plant_species_grouped.csv")

# Check what the data looks like
glimpse(plant_species_grouped)

# ==============================================================================
# 3. Import cleaned plot metadata (kopfdaten_clean.csv)
# ==============================================================================

kopfdaten_clean <- read_csv("data/processed/kopfdaten_clean.csv")

# Check what the data looks like
glimpse(kopfdaten_clean)

# ==============================================================================
# 4. Import ecological indicator values (EIVs) (plant_eiv_matched.csv)
# ==============================================================================

eiv <- read_csv("data/processed/plant_eiv_matched.csv")

# Check what the data looks like
glimpse(eiv)

# ============================================================
# 5. CM Calculation for Ecological Indicator Values (EIVs)
# ============================================================

# Each species per plot is assigned a value of 1 (presence)
presence_matrix <- eiv %>%
  mutate(present = 1) %>%
  select(aID_KD, grouped_name, present) %>%
  distinct() %>%
  pivot_wider(names_from = grouped_name, values_from = present, values_fill = 0)

dim(presence_matrix) # [1] 6283 1382 (1 Spalte = aID_KD + 1381 Arten)
head(presence_matrix[, 1:5])
length(unique(eiv$aID_KD))  # 6283

# Store aID_KD as separate vector for later re-attachment
plot_ids <- presence_matrix$aID_KD
presence_matrix <- presence_matrix %>% select(-aID_KD)

# -----------------------------
# 2. Prepare trait matrix: species × EIVs
# -----------------------------

names(eiv)

# Select relevant ecological indicator values and average them per species
eiv_traits <- eiv %>%
  select(grouped_name, T, L, F, R, N, EM, MV, C_score, S_score, R_score) %>%
  group_by(grouped_name) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

# Convert to data frame for functcomp()
eiv_traits <- as.data.frame(eiv_traits)
rownames(eiv_traits) <- eiv_traits$grouped_name
eiv_traits <- eiv_traits %>% select(-grouped_name)

dim(eiv_traits)       
glimpse(eiv_traits)   
colSums(is.na(eiv_traits))

summary_stats <- eiv_traits %>% 
  summarise(across(everything(), ~sum(!is.na(.)), .names = "non_na_{.col}"))

print(summary_stats)

# Ensure identical and sorted species between traits and presence matrix
common_species <- intersect(colnames(presence_matrix), rownames(eiv_traits))
common_species_sorted <- sort(common_species)

presence_matrix_sorted <- presence_matrix[, common_species_sorted]
eiv_traits_sorted <- eiv_traits[common_species_sorted, ]

# -----------------------------
# 3. Calculate Community Means (CMs)
# -----------------------------

# Calculate CMs using unweighted presence-absence data
cwm_eiv <- functcomp(
  as.matrix(eiv_traits_sorted), 
  as.matrix(presence_matrix_sorted), 
  CWM.type = "all"
)

# Check dimensions and preview result
dim(cwm_eiv)
glimpse(cwm_eiv)

# Convert aID_KD in both datasets to character to ensure consistent matching
# This prevents mismatches due to floating point precision issues in numeric IDs

# Add stored aID_KD vector back into CWM result
cwm_eiv <- cwm_eiv %>%
  mutate(aID_KD = as.character(plot_ids))

kopfdaten_clean <- kopfdaten_clean %>%
  mutate(aID_KD = as.character(aID_KD))       # Convert aID_KD to character type in metadata

# Join CM values with plot metadata based on aID_KD
cwm_full <- left_join(kopfdaten_clean, cwm_eiv, by = "aID_KD")

# Quick check of merged dataset
glimpse(cwm_full)

sum(!is.na(cwm_full$T))  # 6258 -> good.


# -------------------------------------------
# Prepare data
# -------------------------------------------

# Create a time index variable (years since 2001)
cwm_full <- cwm_full %>%
  mutate(year_index = yearPl - 2001)

# Define list of all ecological indicator values (EIVs) to analyze
eiv_list <- c("T", "L", "F", "R", "N", "EM", "MV", "C_score", "S_score", "R_score")

# Initialize empty lists to store models and trend results
models <- list()
slopes <- list()


# -------------------------------------------
# Loop over all EIV variables
# -------------------------------------------

for (trait in eiv_list) {
  
  # Subset data: remove missing values for current trait
  data_trait <- cwm_full %>%
    filter(!is.na(.data[[trait]]))
  
  # Fit linear mixed model with random intercept per site
  model <- glmmTMB(
    formula = as.formula(paste0(trait, " ~ year_index + (1 | aID_STAO)")),
    data = data_trait,
    family = gaussian()
  )
  
  # Model diagnostics using DHARMa
  simres <- simulateResiduals(model)
  plot(simres)
  mtext(paste("DHARMa residual diagnostics – CWM", trait), side = 3, line = 3, cex = 1.1, font = 2)
  
  # Extract year slope estimate for current trait
  trend <- summary(model)$coefficients$cond["year_index",]
  
  # Store model and slope estimate
  models[[trait]] <- model
  slopes[[trait]] <- trend
}

# -------------------------------------------
# Combine results into a single table
# -------------------------------------------

results <- do.call(rbind, slopes) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Trait")

# Print summary table of all slope estimates
print(results)

pred_data <- ggeffect(model, terms = "year_index")
pred_data <- pred_data %>%
  mutate(year = x + 2001)



# Generate and show the plots for each CWM trait
for (trait in eiv_list) {
  
  model <- models[[trait]]
  
  # Use ggemmeans() instead of ggeffect() for more stable predictions
  pred_data <- tryCatch({
    ggemmeans(model, terms = "year_index") %>%
      as.data.frame() %>%
      mutate(year = x + 2001)
  }, error = function(e) {
    message(paste("Skipping trait:", trait, "due to prediction error"))
    return(NULL)
  })
  
  # Skip if prediction failed
  if (is.null(pred_data)) next
  
  data_trait <- cwm_full %>% 
    select(yearPl, !!sym(trait)) %>%
    filter(!is.na(.data[[trait]]))
  
  p <- ggplot() +
    geom_point(data = data_trait, aes(x = yearPl, y = .data[[trait]]), 
               alpha = 0.1, color = "grey50", size = 1) +
    geom_line(data = pred_data, aes(x = year, y = predicted), 
              size = 1.2, color = "steelblue") +
    labs(
      x = "Year",
      y = paste("CWM", trait)
    ) +
    scale_x_continuous(
      breaks = seq(2001, 2023, by = 2),
      expand = c(0, 0)
    ) +
    mytheme +
    theme(
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.title = element_text(size = 11),
      legend.position = "none"
    )
  
  print(p)
}

# Combine slope results into a dataframe
results <- do.call(rbind, slopes) %>%
  as.data.frame() %>%
  rownames_to_column(var = "Trait")

# Add formatted p-values and yearly change
results <- results %>%
  mutate(
    `Estimate_per_year` = Estimate,
    `Std_Error` = `Std. Error`,
    `z_value` = `z value`,
    `p_value` = `Pr(>|z|)`,
    `Significance` = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.1   ~ ".",
      TRUE ~ "n.s."
    )
  ) %>%
  select(Trait, Estimate_per_year, Std_Error, z_value, p_value, Significance)

# Print table
print(results)

# Round for nicer output
results_rounded <- results %>%
  mutate(across(where(is.numeric), round, digits = 5))

print(results_rounded)




