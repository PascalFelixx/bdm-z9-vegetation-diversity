# ------------------------------------------------------------------------------
# Script Name: 05_species-level-analysis.R
# Purpose:     Perform species-level statistical analysis on BDM Z9 plant data.
#              Logistic GLMs for presence/absence trends and visualization of winners/losers.
# Author:      Pascal Felix
# Date:        2025-06-06
# ------------------------------------------------------------------------------

# ==============================================================================
# Load required libraries
# ==============================================================================

library(tidyverse)
library(broom)
library(forcats)
library(tidyverse)
library(glmmTMB)
library(broom.mixed)
library(purrr)
library(ggplot2)
library(scales)

# Custom plot theme (defined in separate script)
source("analysis/00_mytheme.R")  # Loads 'mytheme' for consistent ggplot styling


# Set working directory (adjust if needed)
setwd("C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity")

# ==============================================================================
# Load processed data & check data structure
# ==============================================================================

plants <- read.csv("data/processed/plant_species_grouped.csv")
kopfdaten <- read.csv("data/processed/kopfdaten_clean.csv")

# Glimpse species data
glimpse(plants)

# Glimpse metadata
glimpse(kopfdaten)

# Show a few unique species names
plants %>% 
  distinct(grouped_name) %>% 
  head(10)

# Quick year range check
kopfdaten %>% 
  summarise(min_year = min(yearPl, na.rm = TRUE), 
            max_year = max(yearPl, na.rm = TRUE))

# Prepare survey base (unique plot-years with year index)
survey_base <- kopfdaten %>%
  select(aID_KD, yearPl) %>%
  mutate(year_index = yearPl - 2000)

# List of unique species
arten_liste <- unique(plants$grouped_name)

glimpse(arten_liste)

# ==============================================================================
# GLMM species-level temporal trends
# ==============================================================================

# Prepare basic metadata table with plot-year and plot ID

plot_ids <- kopfdaten %>%
  select(aID_KD, aID_STAO, yearPl) %>%
  mutate(year_index = yearPl - 2000)  # year index for modeling

# Get unique species
species_to_model <- unique(plants$grouped_name)
glimpse(species_to_model)

# Run per-species GLMM using lazy expansion (species × plot-year)

glmm_results <- map_df(species_to_model, function(sp) {
  message("Processing species: ", sp)
  
  # Get all plots where this species was observed
  observed_plots <- plants %>%
    filter(grouped_name == sp) %>%
    distinct(aID_KD) %>%
    mutate(presence = 1)
  
  # Create all plot-year combinations for this species (expand lazily)
  sp_grid <- plot_ids %>%
    mutate(grouped_name = sp) %>%
    left_join(observed_plots, by = "aID_KD") %>%
    mutate(presence = replace_na(presence, 0))  # absent if not found
  
  # Filter out species with no variation in response (in order to avoid unstable modells)
  if (sum(sp_grid$presence == 1) < 10 | sum(sp_grid$presence == 0) < 10) {
    message("  Skipped ", sp, ": not enough variation in presence/absence")
    return(NULL)
  }
  
  # Fit GLMM model (binomial) with year as fixed effect and plot ID as random intercept
  tryCatch({
    mod <- glmmTMB(
      presence ~ year_index + (1 | aID_STAO),
      data = sp_grid,
      family = binomial
    )
    
    # Extract estimate and stats
    est <- fixef(mod)$cond["year_index"]
    se  <- sqrt(vcov(mod)$cond["year_index", "year_index"])
    z   <- est / se
    p   <- 2 * pnorm(abs(z), lower.tail = FALSE)
    
    tibble(
      species = sp,
      estimate = est,
      std_error = se,
      z_value = z,
      p_value = p
    )
    
  }, error = function(e) {
    message("  ⚠️ Model failed for ", sp, ": ", conditionMessage(e))
    NULL
  })
})

# Annotate trend direction and show results

glmm_results <- glmm_results %>%
  mutate(
    trend = case_when(
      p_value < 0.05 & estimate > 0 ~ "increasing",
      p_value < 0.05 & estimate < 0 ~ "decreasing",
      TRUE ~ "n.s."
    )
  ) %>%
  arrange(p_value)

# Preview top results
print(head(glmm_results, 10))

glmm_results %>%
  ggplot(aes(x = estimate, y = -log10(p_value), color = trend)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Year effect (slope)", y = "Significance (-log10 p)", title = "Species trends over time")


# Evaluation of species coverage, full run

# 1. Total species in the plant dataset
total_species <- plants %>% 
  distinct(grouped_name) %>% 
  nrow()

# 2. All species that were attempted
attempted_species <- unique(plants$grouped_name)
n_attempted_species <- length(attempted_species)

# 3. Species for which models were successfully fitted
modeled_species <- unique(glmm_results$species)
n_modeled_species <- length(modeled_species)

# 4. Species skipped (due to low variation or model failure)
skipped_species <- setdiff(attempted_species, modeled_species)
n_skipped_species <- length(skipped_species)

# Summary printout
cat("---------- Species Modeling Summary ----------\n")
cat("Total species in dataset:                ", total_species, "\n")
cat("Species attempted for modeling:          ", n_attempted_species, "\n")
cat("Species successfully modeled (GLMM):     ", n_modeled_species, "\n")
cat("Species skipped due to low variation:    ", n_skipped_species, "\n")
cat("Coverage of all species in dataset:      ", round(100 * n_modeled_species / total_species, 1), "%\n")

# Select top 10 positive and negative trends (significant only)
top_10 <- glmm_results %>%
  filter(p_value < 0.05) %>%
  slice_max(order_by = estimate, n = 10) %>%
  mutate(trend = "increasing") %>%
  bind_rows(
    glmm_results %>%
      filter(p_value < 0.05) %>%
      slice_min(order_by = estimate, n = 10) %>%
      mutate(trend = "decreasing")
  ) %>%
  mutate(species = fct_reorder(species, estimate))

# Plot
ggplot(top_10, aes(x = species, y = estimate, fill = trend)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Species",
    y = "Trend over time (GLMM β estimate)"
  ) +
  scale_fill_manual(values = c("increasing" = "forestgreen", "decreasing" = "firebrick")) +
  theme_minimal(base_size = 13)


# Define cutoff for visual y-axis
plotting_limit <- 130

# Prepare GLMM plot data and join number of observations
glmm_plot_data <- glmm_results %>%
  filter(p_value < 0.05) %>%
  mutate(
    yearly_change_percent = (exp(estimate) - 1) * 100,
    significance = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.01  ~ "p < 0.01",
      p_value < 0.05  ~ "p < 0.05"
    )
  ) %>%
  # Add number of observations per species
  left_join(
    plants %>%
      group_by(grouped_name) %>%
      summarise(n_obs = n(), .groups = "drop"),
    by = c("species" = "grouped_name")
  )

# Select top 10 increasing and decreasing species
top10_both <- glmm_plot_data %>%
  slice_max(order_by = yearly_change_percent, n = 10) %>%
  bind_rows(
    glmm_plot_data %>%
      slice_min(order_by = yearly_change_percent, n = 10)
  ) %>%
  # Build labels and truncate bars for display, but keep actual values for text
  mutate(
    species_label = paste0(species, " (n = ", n_obs, ")"),
    species_label = fct_reorder(species_label, yearly_change_percent),
    
    # Show actual value in label with * if it's clipped
    label = case_when(
      abs(yearly_change_percent) > plotting_limit ~
        paste0(ifelse(yearly_change_percent > 0, "+", ""), sprintf("%.1f", yearly_change_percent), "%*"),
      TRUE ~
        paste0(ifelse(yearly_change_percent > 0, "+", ""), sprintf("%.1f", yearly_change_percent), "%")
    ),
    
    # Truncate bar to axis limit (but keep full value for label)
    display_value = pmax(pmin(yearly_change_percent, plotting_limit), -plotting_limit)
  )

# Create barplot with clipping at ±130%
ggplot(top10_both, aes(x = species_label, y = display_value, fill = significance)) +
  geom_col() +
  geom_text(
    aes(label = label),
    hjust = ifelse(top10_both$display_value > 0, -0.1, 1.1),
    size = 2.6
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c(
      "p < 0.001" = "#d73027",
      "p < 0.01"  = "#fc8d59",
      "p < 0.05"  = "#fee08b"
    )
  ) +
  scale_y_continuous(
    limits = c(-plotting_limit, plotting_limit),
    breaks = seq(-plotting_limit, plotting_limit, by = 20),
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  labs(
    x = "Species (with number of observations)",
    y = "Estimated yearly change (%)",
    fill = "Significance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 7),        
    axis.text.x = element_text(size = 9),        
    axis.title.x = element_text(size = 10),      
    axis.title.y = element_text(size = 10),
    legend.text = element_text(size = 8),       # Text in der Legende
    legend.title = element_text(size = 9)       # Titel der Legende
  )

ggsave("outputs/figures/05_species_trend_top_10_increase_and_decrease.png", width = 8, height = 5.5, dpi = 300)


# Plausibilty Check (replace species)
glmm_results %>%
  filter(species == "Capsella rubella")
(exp(glmm_results %>% filter(species == "Capsella rubella") %>% pull(estimate)) - 1) * 100


# Step 1: Identify top 20 most frequent species
top20_species <- plants %>%
  count(grouped_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  pull(grouped_name)

# Step 2: Create plot-year table
plot_ids <- kopfdaten %>%
  select(aID_KD, aID_STAO, yearPl) %>%
  mutate(year_index = yearPl - 2000)


# Count of species per trend category
glmm_results %>%
  filter(p_value < 0.05) %>%
  count(trend) %>%
  rename(Significant_Trend = trend, Number_of_Species = n) %>%
  print()

# Export full table of significantly changing species
write_csv(
  glmm_plot_data %>% arrange(desc(abs(yearly_change_percent))),
  "outputs/tables/species_trends_significant.csv"
)

# Plotting
top20_results %>%
  mutate(
    yearly_change_percent = (exp(estimate) - 1) * 100,
    significance = case_when(
      p_value < 0.001 ~ "p < 0.001",
      p_value < 0.01 ~ "p < 0.01",
      p_value < 0.05 ~ "p < 0.05",
      TRUE ~ "n.s."
    ),
    species = fct_reorder(species, yearly_change_percent)
  ) %>%
  ggplot(aes(x = species, y = yearly_change_percent, fill = significance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Trend of the 20 Most Frequent Plant Species",
    y = "Estimated yearly change (%)",
    x = "Species"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ==============================================================================
# Export species trends (for thesis appendix)
# ==============================================================================

# Filter signifikante Ergebnisse mit gewünschten Spalten
significant_species_export <- glmm_plot_data %>%
  select(species, yearly_change_percent, p_value, trend, n_obs)

# Aufteilen in Gewinner und Verlierer
species_increase <- significant_species_export %>%
  filter(trend == "increasing") %>%
  arrange(desc(yearly_change_percent))

species_decrease <- significant_species_export %>%
  filter(trend == "decreasing") %>%
  arrange(yearly_change_percent)

# Exportieren als CSV für Anhang
write_csv(species_increase, "outputs/tables/appendix_species_winners.csv")
write_csv(species_decrease, "outputs/tables/appendix_species_losers.csv")


