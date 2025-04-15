# ------------------------------------------------------------------------------
# Script Name: 01_*****.R
# Purpose:     *****
# Author:      Pascal Felix
# Date:        2025-03-09
# ------------------------------------------------------------------------------

# -----------------------------
# Install & Load Required Packages
# -----------------------------

# Load libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)

# -----------------------------
# Define File Path
# -----------------------------

# Set working directory // Adjust to your machine
setwd("C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity")

# -----------------------------
# Load Main Data
# -----------------------------

# Load metadata (Kopfdaten)
kopfdaten <- read_excel("data/raw/2000_Z9_Kopfdaten.xlsx") %>%
  filter(yearPl >= 2001 | yearMoos >= 2001)

# Load plant species data
pflanzen <- read_excel("data/raw/2000_Z9_Pflanzen.xlsx")

# Load moss species data
moose <- read_excel("data/raw/2000_Z9_Moose.xlsx")

# -----------------------------
# Load Data Additional Data for Taxation
# -----------------------------

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


# -----------------------------
# Preview Main Data Structures
# -----------------------------

glimpse(kopfdaten)
glimpse(pflanzen)
glimpse(moose)

# See the top rows
head(kopfdaten)
head(pflanzen)
head(moose)

cat("\nMissing Values:\n")
colSums(is.na(kopfdaten))
colSums(is.na(moose))
colSums(is.na(pflanzen))

# -----------------------------
# Data Exploration & Visualization
# -----------------------------

# 1. Number of plant observations per year - why is there data for 1996-2000?
kopfdaten %>%
  filter(!is.na(yearPl)) %>%
  count(yearPl) %>%
  ggplot(aes(x = yearPl, y = n)) +
  geom_col(fill = "darkgreen") +
  labs(title = "Number of Plant Surveys per Year", x = "Year", y = "Number of Surveys") +
  theme_minimal()

# 2. Number of moss observations per year
kopfdaten %>%
  filter(!is.na(yearMoos)) %>%
  count(yearMoos) %>%
  ggplot(aes(x = yearMoos, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Number of Moss Surveys per Year", x = "Year", y = "Number of Surveys") +
  theme_minimal()

# 3. Map of survey locations // every 5 year the pattern should repeat
ggplot(kopfdaten, aes(x = XKoord_LV95, y = YKoord_LV95)) +
  geom_point(alpha = 0.5, color = "darkred") +
  labs(title = "Survey Site Locations (All Years)", x = "X (LV95)", y = "Y (LV95)") +
  theme_minimal()

# 4. Elevation distribution of sites
ggplot(kopfdaten, aes(x = Hoehe)) +
  geom_histogram(bins = 30, fill = "gray30") +
  labs(title = "Elevation Distribution of Survey Sites", x = "Elevation (m)", y = "Frequency") +
  theme_minimal()

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

# 7. Number of sites per canton
kopfdaten %>%
  count(Kanton) %>%
  ggplot(aes(x = reorder(Kanton, n), y = n)) +
  geom_col(fill = "orange3") +
  coord_flip() +
  labs(title = "Number of Survey Sites per Canton", x = "Canton", y = "Number of Sites") +
  theme_minimal()


# -----------------------------
# Species Aggregation Based on Moss Checklist
# -----------------------------

# We create a clean species name for each moss observation by combining genus and species.
# This makes it easier to match the BDM moss data with external checklists for grouping and validation.
# We also remove moss records that don’t have a valid survey year.

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

# Several Species found!

# Which species names appear more than once in the bryophyte list?
bryo_list_clean %>%
  count(species_name, sort = TRUE) %>%
  filter(n > 1)

# Alright, the issue is minor: It's just one species
# Brachythecium albicans aggr.     2
# this duplicate is probably a minor issue 
# (maybe different taxonnamen_ids for the same aggregate).

# Remove duplicated species name entries in the bryophyte list
bryo_list_dedup <- bryo_list_clean %>%
  distinct(species_name, .keep_all = TRUE)

# Sind Aggregate wie sie in bdm definiert sind so wie in den Checklisten?

# Join safely into your moss data
moose_matched <- moose_clean %>%
  left_join(bryo_list_dedup, by = "species_name")
#kann zusammengeführt werden

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

# Calculate change in observation count per species
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

View(trend_table)

# -----------------------------
# Species Aggregation and Trend Analysis – Plants
# -----------------------------

# This prepares the plant checklist so we can group subspecies or varieties under a shared parent name.
# If a species is part of a larger group (e.g. an aggregate), we want to use that group for analysis.
# This helps avoid splitting trends across nearly identical species.

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
  filter(aID_KD %in% kopfdaten$aID_KD[!is.na(kopfdaten$yearPl)]) %>%
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

View(trend_table_plants)

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

# 8. Plot species richness (unique species per year)
pflanzen_joined %>%
  filter(!is.na(grouped_name), !is.na(yearPl)) %>%
  group_by(yearPl) %>%
  summarise(richness = n_distinct(grouped_name)) %>%
  ggplot(aes(x = yearPl, y = richness)) +
  geom_line(color = "darkgreen", linewidth = 1.3) +
  labs(title = "Plant Species Richness Over Time", x = "Year", y = "Unique Species") +
  theme_minimal()

# Plot und Veränderungsprozent falsch wegen Daten 1996 - 2000
# Zacken im Plot aufgrund absoluter Zahlen, besser in Prozent/Jahr, Aufnahme
# Folgende Anpassung: 1. und letzter 5 Jahresabschnitte (oder letzer angefangener)
# Ausdrücken in Prozent. Prozent im Sinne von in wie vielen plots der (300) kommt eine Art vor
# 2001 - 2005  vs 2016 - 2020 / Absolut und in Prozent, Differenz 


# --- Alternative trend table based on fixed time periods (2001–2005 vs. 2016–2020) ---

# Create new version with yearMoos from kopfdaten included
moose_grouped_with_year <- moose_grouped %>%
  left_join(kopfdaten %>% select(aID_KD, yearMoos), by = "aID_KD")

# 1. Assign each observation to a 5-year period
moose_periods <- moose_grouped_with_year %>%
    filter(!is.na(grouped_name)) %>%
  mutate(period = case_when(
    yearMoos >= 2001 & yearMoos <= 2005 ~ "2001-2005",
    yearMoos >= 2016 & yearMoos <= 2020 ~ "2016-2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

# 2. Count total unique plots per period (needed for % calculation)
total_plots_per_period <- kopfdaten %>%
  select(aID_KD, yearMoos) %>%
  filter(!is.na(yearMoos)) %>%
  mutate(period = case_when(
    yearMoos >= 2001 & yearMoos <= 2005 ~ "2001-2005",
    yearMoos >= 2016 & yearMoos <= 2020 ~ "2016-2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  distinct(period, aID_KD) %>%
  count(period) %>%
  pivot_wider(names_from = period, values_from = n)

# 3. Count how many plots each species appears in per period
trend_table_periods <- moose_periods %>%
  distinct(aID_KD, grouped_name, period) %>%
  count(grouped_name, period) %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  mutate(
    abs_diff = `2016-2020` - `2001-2005`,
    perc_2001_2005 = round(`2001-2005` / total_plots_per_period$`2001-2005` * 100, 1),
    perc_2016_2020 = round(`2016-2020` / total_plots_per_period$`2016-2020` * 100, 1),
    perc_diff = perc_2016_2020 - perc_2001_2005
  ) %>%
  arrange(desc(abs_diff))

# Optional: View the result
View(trend_table_periods)


############################

# Add yearPl to the plant data (in case it's not there)
pflanzen_grouped_with_year <- pflanzen_grouped %>%
  left_join(kopfdaten %>% select(aID_KD, yearPl), by = "aID_KD")

# Assign 5-year periods
pflanzen_periods <- pflanzen_grouped_with_year %>%
  filter(!is.na(grouped_name), !is.na(yearPl)) %>%
  mutate(period = case_when(
    yearPl >= 2001 & yearPl <= 2005 ~ "2001-2005",
    yearPl >= 2016 & yearPl <= 2020 ~ "2016-2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

total_plots_per_period_plants <- kopfdaten %>%
  select(aID_KD, yearPl) %>%
  filter(!is.na(yearPl)) %>%
  mutate(period = case_when(
    yearPl >= 2001 & yearPl <= 2005 ~ "2001-2005",
    yearPl >= 2016 & yearPl <= 2020 ~ "2016-2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  distinct(period, aID_KD) %>%
  count(period) %>%
  pivot_wider(names_from = period, values_from = n)

trend_table_periods_plants <- pflanzen_periods %>%
  distinct(aID_KD, grouped_name, period) %>%
  count(grouped_name, period) %>%
  pivot_wider(names_from = period, values_from = n, values_fill = 0) %>%
  mutate(
    abs_diff = `2016-2020` - `2001-2005`,
    perc_2001_2005 = round(`2001-2005` / total_plots_per_period_plants$`2001-2005` * 100, 1),
    perc_2016_2020 = round(`2016-2020` / total_plots_per_period_plants$`2016-2020` * 100, 1),
    perc_diff = perc_2016_2020 - perc_2001_2005
  ) %>%
  arrange(desc(abs_diff))

# View it if needed
View(trend_table_periods_plants)

# Define output path
output_path <- "C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity/outputs/tables"

# Make sure folder exists (optional safety)
dir.create(output_path, showWarnings = FALSE)

# Save moss trend table
write.csv(
  trend_table_periods,
  file = file.path(output_path, "trend_table_moss_2001-2005_vs_2016-2020.csv"),
  row.names = FALSE
)

# Save plant trend table
write.csv(
  trend_table_periods_plants,
  file = file.path(output_path, "trend_table_plants_2001-2005_vs_2016-2020.csv"),
  row.names = FALSE
)

