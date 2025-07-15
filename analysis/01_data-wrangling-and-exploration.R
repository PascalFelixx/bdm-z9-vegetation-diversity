# ------------------------------------------------------------------------------
# Script Name: 01_data-wrangling-and-exploration.R
# Purpose:     Prepare and clean BDM Z9 moss and plant species data for analysis.
#              This includes data import, taxonomic harmonization, species grouping,
#              temporal aggregation, and the export of ready-to-use trend tables
#              for statistical analysis.
# Author:      Pascal Felix
# Date:        2025-03-09
# ------------------------------------------------------------------------------

# ==============================================================================
# Install & Load Required Packages
# ==============================================================================

# Load libraries
library(readxl)       # To read Excel files
library(tidyverse)    # Data manipulation, visualization
library(stringr)      # For working with and manipulating strings
library(sf)           # For Creating Maps

# ==============================================================================
# Define File Path
# ==============================================================================

# Set working directory // Adjust to your machine
setwd("C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity")

# Create output folders if they don't exist
if (!dir.exists("outputs/figures")) dir.create("outputs/figures", recursive = TRUE)
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

# ==============================================================================
# Load Main Data
# ==============================================================================

# Load metadata (Kopfdaten)
kopfdaten <- read_excel("data/raw/2000_Z9_Kopfdaten.xlsx")

# Load plant species data
# Increase 'guess_max' to avoid incorrect column type guessing (e.g., Braun-Blanquet codes like "2a", "+", "r")
pflanzen <- read_excel("data/raw/2000_Z9_Pflanzen.xlsx", guess_max = 100000)

# Load moss species data
moose <- read_excel("data/raw/2000_Z9_Moose.xlsx")

# Load Swiss Border for displaying BDM distribution
schweiz_outline <- st_read("data/raw/swissBOUNDARIES3D_1_5_TLM_LANDESGEBIET.shp")

# ==============================================================================
# Load Data Additional Data for Taxation
# ==============================================================================

# Load cleaned moss checklist (starting from real header)
checklist <- read_excel("data/raw/Checklist_2017_simple_version_20230503.xlsx",
                        sheet = "Checklist 2017",
                        skip = 4)
# Inspect structure
glimpse(checklist)

# Load bryophyte species list
bryo_list <- read_excel("data/raw/Artenliste Swissbryophytes Konzept7_2018_JD.xlsx",
                        sheet = "Bearbeitet JD")

# Inspect structure
glimpse(bryo_list)

# ==============================================================================
# Preview Main Data Structures
# ==============================================================================

glimpse(kopfdaten)
glimpse(pflanzen)
glimpse(moose)

# See the top rows
head(kopfdaten)
head(pflanzen)
head(moose)

# Check for missing values across all datasets.
# This helps identify incomplete survey records (e.g., missing yearPl or yearMoos),
# which will be excluded in the validated version of the metadata (kopfdaten_valid).

cat("\nMissing Values:\n")
colSums(is.na(kopfdaten))
colSums(is.na(moose))
colSums(is.na(pflanzen))

kopfdaten_valid <- kopfdaten %>%
  filter(!is.na(yearPl) | !is.na(yearMoos))

glimpse(kopfdaten_valid)
nrow(kopfdaten_valid)

nrow(kopfdaten) - nrow(kopfdaten_valid)

# 107 columns have been dropped.

# ==============================================================================
# Data Exploration & Visualization
# ==============================================================================

# 1. Number of plant observations per year
kopfdaten_valid %>%
  filter(!is.na(yearPl)) %>%
  count(yearPl) %>%
  ggplot(aes(x = yearPl, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Number of Plant Surveys per Year", x = "Year", y = "Number of Surveys") +
  theme_minimal()

# Save
ggsave("outputs/figures/01_plant_surveys_raw.png", width = 8, height = 5, dpi = 300)

# Unexpected data points appear before 2001, although plant surveys officially start in 2001.
# These early records are likely due to test runs.
# All plant records with yearPl < 2001 will be excluded in further analyses to maintain consistency.

kopfdaten_clean <- kopfdaten_valid %>%
  filter(is.na(yearPl) | yearPl >= 2001)

# 1.1 Number of plant observations per year / < 2001 removed?
kopfdaten_clean %>%
  filter(!is.na(yearPl)) %>%
  count(yearPl) %>%
  ggplot(aes(x = yearPl, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Number of Plant Surveys per Year", x = "Year", y = "Number of Surveys") +
  theme_minimal()

ggsave("outputs/figures/01_plant_surveys_cleaned.png", width = 8, height = 5, dpi = 300)

# Removal succesfull.

# 2. Number of moss observations per year
kopfdaten_clean %>%
  filter(!is.na(yearMoos)) %>%
  count(yearMoos) %>%
  ggplot(aes(x = yearMoos, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Moss Surveys per Year", x = "Year", y = "Number of Surveys") +
  theme_minimal()

ggsave("outputs/figures/01_moss_surveys_per_year.png", width = 8, height = 5, dpi = 300)

# 3. Map of survey locations // every 5 year the pattern should repeat
ggplot(kopfdaten_clean, aes(x = XKoord_LV95, y = YKoord_LV95)) +
  geom_point(alpha = 0.5, color = "darkred") +
  labs(title = "Survey Site Locations (All Years)", x = "X (LV95)", y = "Y (LV95)") +
  theme_minimal()

ggsave("outputs/figures/01_survey_locations_map.png", width = 8, height = 6, dpi = 300)

# 3.1 Create an appropriate Map for BA Thesis

# Convert kopfdaten_clean to sf-object with LV95 (CH1903+)
kopfdaten_sf <- st_as_sf(kopfdaten_clean,
                         coords = c("XKoord_LV95", "YKoord_LV95"),
                         crs = 2056)

# Define color palette
landuse_colors <- c(
  "Aecker" = "#1b9e77",
  "Alpweiden" = "#d95f02",
  "Nicht genutzte Flaechen" = "#7570b3",
  "Siedlung" = "#e7298a",
  "Wald" = "#66a61e",
  "Wiesen, Weiden" = "#e6ab02"
)

# Define English labels
landuse_labels <- c(
  "Aecker" = "Arable land",
  "Alpweiden" = "Alpine pastures",
  "Nicht genutzte Flaechen" = "Non productive areas",
  "Siedlung" = "Settlements",
  "Wald" = "Forests",
  "Wiesen, Weiden" = "Grasslands"
)

# Plot
ggplot() +
  geom_sf(data = schweiz_outline, fill = "grey95", color = "grey60", size = 0.4) +
  geom_sf(data = kopfdaten_sf %>% filter(!is.na(HN)),
          aes(color = HN),
          size = 1, alpha = 0.9) +
  scale_color_manual(values = landuse_colors, labels = landuse_labels, name = "Land Use Type") +
  theme_void(base_size = 12) +  # removes axes, ticks, grid, and background
  theme(
    legend.position = "right",
    plot.caption = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

ggsave("outputs/figures/01_survey_locations_land_use_types_map.png", width = 8, height = 6, dpi = 300)

# 4. Elevation distribution of sites
ggplot(kopfdaten_clean, aes(x = Hoehe)) +
  geom_histogram(bins = 30, fill = "gray30") +
  labs(title = "Elevation Distribution of Survey Sites", x = "Elevation (m)", y = "Frequency") +
  theme_minimal()

ggsave("outputs/figures/01_elevation_distribution.png", width = 8, height = 5, dpi = 300)

# 5. Top 20 most common plant species
pflanzen %>%
  count(Gattung, Art, sort = TRUE) %>%
  unite("Species", Gattung, Art, sep = " ") %>%
  slice_max(order_by = n, n = 20) %>%
  ggplot(aes(x = reorder(Species, n), y = n)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Plant Species", x = "Species", y = "Number of Records") +
  theme_minimal()

ggsave("outputs/figures/01_top20_plant_species.png", width = 8, height = 6, dpi = 300)

# 6. Top 20 most common moss species
moose %>%
  count(Gattung, Art, sort = TRUE) %>%
  unite("Species", Gattung, Art, sep = " ") %>%
  slice_max(order_by = n, n = 20) %>%
  ggplot(aes(x = reorder(Species, n), y = n)) +
  geom_col(fill = "dodgerblue4") +
  coord_flip() +
  labs(title = "Top 20 Most Frequent Moss Species", x = "Species", y = "Number of Records") +
  theme_minimal()

ggsave("outputs/figures/01_top20_moss_species.png", width = 8, height = 6, dpi = 300)

# 7. Number of sites per canton
kopfdaten_clean %>%
  count(Kanton) %>%
  ggplot(aes(x = reorder(Kanton, n), y = n)) +
  geom_col(fill = "orange3") +
  coord_flip() +
  labs(title = "Number of Survey Sites per Canton", x = "Canton", y = "Number of Sites") +
  theme_minimal()

ggsave("outputs/figures/01_sites_per_canton.png", width = 8, height = 6, dpi = 300)

# Summary of Exploratory Data Checks:
# After excluding records dated before 2001, the temporal distribution of valid plant and moss surveys was inspected.
# Survey effort appears relatively stable across the years, with approximately 250–320 surveys conducted annually.
# The spatial distribution of plots covers most regions of Switzerland, with a concentration in lowland to submontane elevations.
# The most frequently recorded species include widespread and common taxa, suggesting a consistent and plausible dataset.

# ==============================================================================
# Moss Species Aggregation and Trend Analysis
# ==============================================================================

# This section performs taxonomic harmonization of moss records by linking species
# names in the BDM dataset to a reference taxonomy (Konzept7) and a national checklist.
# Species are grouped under aggregates where appropriate to ensure consistency
# in temporal trend analyses and to avoid artificial splitting of taxonomic units.


# 1. Clean species name in BDM moss dataset
moose_clean <- moose %>%
  filter(aID_KD %in% kopfdaten$aID_KD[!is.na(kopfdaten$yearMoos)]) %>%
  mutate(species_name = str_trim(paste(Gattung, Art)))

# 2. Prepare bryophyte list from Konzept7 for matching
bryo_list_clean <- bryo_list %>%
  mutate(species_name = str_trim(`Full name without authorities`)) %>%
  select(bryo_taxa_id, taxonnamen_id, species_name, Gattung, Art, name_D, name_E)

# 3. Join bryophyte metadata into moss observations
moose_matched <- moose_clean %>%
  left_join(bryo_list_clean, by = "species_name")

# Only one species name occurs more than once ('Brachythecium albicans aggr.').

# Which species names appear more than once in the bryophyte list?
bryo_list_clean %>%
  count(species_name, sort = TRUE) %>%
  filter(n > 1)

# Only one species name ('Brachythecium albicans aggr.') appears more than once in the reference list.
# The duplicates likely represent the same aggregate taxon with different taxonnamen_ids.
# To ensure a safe join and avoid duplication, only the first occurrence per species_name is retained.

# Remove duplicated species name entries in the bryophyte list
bryo_list_dedup <- bryo_list_clean %>%
  distinct(species_name, .keep_all = TRUE)

# Join safely into your moss data
moose_matched <- moose_clean %>%
  left_join(bryo_list_dedup, by = "species_name")

# Can be united

# 4. Prepare checklist for grouping: identify if a species is part of an aggregate
checklist_clean <- checklist %>%
  rename_with(~make.names(.)) %>%
  filter(!is.na(Ist.Teil.von..Taxon.ID)) %>%
  select(Taxon.ID, Taxonname, Ist.Teil.von..Taxonname, Ist.Teil.von..Taxon.ID) %>%
  rename(
    taxon_id = Taxon.ID,
    taxon_name = Taxonname,
    parent_name = Ist.Teil.von..Taxonname,
    parent_id = Ist.Teil.von..Taxon.ID
  )

# 5. Add grouping information: fallback to original name if no aggregate exists
moose_grouped <- moose_matched %>%
  left_join(checklist_clean, by = c("species_name" = "taxon_name")) %>%
  mutate(grouped_name = coalesce(parent_name, species_name))

# 6. Visualize grouped moss trends over time
moose_grouped %>%
  left_join(kopfdaten, by = "aID_KD") %>%
  filter(!is.na(grouped_name), !is.na(yearMoos)) %>%
  group_by(yearMoos, grouped_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = yearMoos, y = n, color = grouped_name)) +
  geom_line(show.legend = FALSE) +
  labs(
    title = "Grouped Moss Species Trends Over Time",
    x = "Year",
    y = "Number of Observations"
  ) +
  theme_minimal()

ggsave("outputs/figures/01_moss_species_trends.png", width = 8, height = 5, dpi = 300)

# 7. Calculate change in observation count per species
trend_table <- moose_grouped %>%
  left_join(kopfdaten, by = "aID_KD") %>%
  filter(!is.na(grouped_name), !is.na(yearMoos)) %>%
  group_by(yearMoos, grouped_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(grouped_name) %>%
  summarise(
    first_year = min(yearMoos),
    last_year = max(yearMoos),
    obs_first = n[which.min(yearMoos)],
    obs_last = n[which.max(yearMoos)],
    trend = obs_last - obs_first,
    perc_change = round((obs_last - obs_first) / obs_first * 100, 1)
  ) %>%
  arrange(desc(trend))  

# View(trend_table)

# 8. Identify top 5 increasing and decreasing moss species
top5_up_moss <- trend_table %>% arrange(desc(trend)) %>% slice_head(n = 5)
top5_down_moss <- trend_table %>% arrange(trend) %>% slice_head(n = 5)
top_species_moss <- bind_rows(top5_up_moss, top5_down_moss)

# 9. Plot species richness over time
moose_grouped %>%
  left_join(kopfdaten_clean, by = "aID_KD") %>%
  filter(grouped_name %in% top_species_moss$grouped_name, !is.na(yearMoos)) %>%
  group_by(yearMoos, grouped_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = yearMoos, y = n, color = grouped_name)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Top 5 Increasing & Decreasing Moss Species",
    x = "Year", y = "Number of Observations", color = "Species"
  ) +
  theme_minimal()

ggsave("outputs/figures/01_top5_moss_trends.png", width = 8, height = 5, dpi = 300)

moose_grouped %>%
  left_join(kopfdaten_clean, by = "aID_KD") %>%
  filter(!is.na(grouped_name), !is.na(yearMoos)) %>%
  group_by(yearMoos) %>%
  summarise(richness = n_distinct(grouped_name)) %>%
  ggplot(aes(x = yearMoos, y = richness)) +
  geom_line(color = "steelblue", linewidth = 1.3) +
  labs(title = "Moss Species Richness Over Time", x = "Year", y = "Unique Species") +
  theme_minimal()

ggsave("outputs/figures/01_moss_richness_over_time.png", width = 8, height = 5, dpi = 300)

# ==============================================================================
# Plants Species Aggregation and Trend Analysis
# ==============================================================================

# This section applies taxonomic grouping to vascular plant observations.
# Subspecies and variants are grouped under parent aggregates using the national
# checklist to enable consistent trend analyses and to reduce taxonomic noise
# caused by naming inconsistencies across years.

# 1. Prepare the checklist for grouping
checklist_plants_clean <- checklist %>%
  rename_with(~make.names(.)) %>%
  filter(!is.na(Ist.Teil.von..Taxon.ID)) %>%
  select(Taxon.ID, Taxonname, Ist.Teil.von..Taxonname, Ist.Teil.von..Taxon.ID) %>%
  rename(
    taxon_id = Taxon.ID,
    taxon_name = Taxonname,
    parent_name = Ist.Teil.von..Taxonname,
    parent_id = Ist.Teil.von..Taxon.ID
  )

# 2. Filter valid plant observations and clean species names
pflanzen_clean <- pflanzen %>%
  filter(aID_KD %in% kopfdaten_clean$aID_KD[!is.na(kopfdaten_clean$yearPl)]) %>%
  mutate(
    species_name = str_trim(paste(Gattung, Art))
  )

# 3. Join checklist info to group subspecies or variants into aggregates
pflanzen_grouped <- pflanzen_clean %>%
  left_join(checklist_plants_clean, by = c("species_name" = "taxon_name")) %>%
  mutate(grouped_name = coalesce(parent_name, species_name))

# 4. Join with metadata to get observation year
pflanzen_joined <- pflanzen_grouped %>%
  left_join(kopfdaten, by = "aID_KD")

# 5. Count number of observations per year and species group
pflanzen_trends <- pflanzen_joined %>%
  filter(!is.na(yearPl)) %>%
  group_by(yearPl, grouped_name) %>%
  summarise(n = n(), .groups = "drop")

# 6. Create trend summary table
trend_table_plants <- pflanzen_trends %>%
  group_by(grouped_name) %>%
  summarise(
    first_year = min(yearPl),
    last_year = max(yearPl),
    obs_first = n[which.min(yearPl)],
    obs_last = n[which.max(yearPl)],
    trend = obs_last - obs_first,
    perc_change = ifelse(obs_first == 0, NA, round((obs_last - obs_first) / obs_first * 100, 1))
  ) %>%
  arrange(desc(trend))

# View(trend_table_plants)

# 7. Plot top 5 increasing & decreasing species
top5_up <- trend_table_plants %>% arrange(desc(trend)) %>% slice_head(n = 5)
top5_down <- trend_table_plants %>% arrange(trend) %>% slice_head(n = 5)
top_species <- bind_rows(top5_up, top5_down)

pflanzen_trends %>%
  filter(grouped_name %in% top_species$grouped_name) %>%
  ggplot(aes(x = yearPl, y = n, color = grouped_name)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Top 5 Increasing & Decreasing Plant Species",
    x = "Year", y = "Number of Observations", color = "Species"
  ) +
  theme_minimal()

ggsave("outputs/figures/01_top5_plant_trends.png", width = 8, height = 5, dpi = 300)

# 8. Plot species richness (unique species per year)
pflanzen_joined %>%
  filter(!is.na(grouped_name), !is.na(yearPl)) %>%
  group_by(yearPl) %>%
  summarise(richness = n_distinct(grouped_name)) %>%
  ggplot(aes(x = yearPl, y = richness)) +
  geom_line(color = "darkgreen", linewidth = 1.3) +
  labs(title = "Plant Species Richness Over Time", x = "Year", y = "Unique Species") +
  theme_minimal()

ggsave("outputs/figures/01_plant_richness_over_time.png", width = 8, height = 5, dpi = 300)

# ==============================================================================
# Final Data Export
# ==============================================================================

# Export cleaned and grouped datasets for statistical analysis.
# These files contain taxonomically harmonized species names and can be directly used for modeling.
# No further cleaning steps will be required in the analysis script.

write.csv(moose_grouped, file = "data/processed/moss_species_grouped.csv", row.names = FALSE)
write.csv(pflanzen_grouped, file = "data/processed/plant_species_grouped.csv", row.names = FALSE)
write.csv(kopfdaten_clean, file = "data/processed/kopfdaten_clean.csv", row.names = FALSE)


