# ------------------------------------------------------------------------------
# Script Name: 03_indicator-values-matching-and-linkage.R
# Purpose:     Link BDM Z9 plant species data with Flora indicativa ecological 
#              indicator values. Clean, match taxa (manual and fuzzy matching), 
#              and prepare full data for community-unweighted mean calculation.
# Author:      Pascal Felix
# Date:        2025-05-08
# ------------------------------------------------------------------------------

# ==============================================================================
# 1. Load required libraries
# ==============================================================================

# Libraries
library(tidyverse)    # For data manipulation
library(readxl)       # To read Excel files
library(fuzzyjoin)    # For joining tables using approximate (fuzzy) string matching
library(stringdist)   # Provides string distance metrics used in fuzzy matching

# ==============================================================================
# 2. Import plant data and raw ecological indicator values
# ==============================================================================
plants <- read_csv("data/processed/plant_species_grouped.csv")
eiv_raw <- read_excel("data/raw/2023-03-29_FI-export_Stefan_Widmer.xlsx", sheet = 1, skip = 1)

# Quick overview
glimpse(plants)
glimpse(eiv_raw)

# ==============================================================================
# 3. Clean and prepare ecological indicator values (EIV)
# ==============================================================================
eiv <- eiv_raw %>%
  select(Taxon, T, L, F, R, N, MV, EM, KS) %>%
  mutate(
    across(c(T, L, F, R, N, MV, EM), ~ str_replace_all(., c("^-+$" = NA_character_, "^x$" = NA_character_))),
    F = str_replace_all(F, fixed("^"), ""),
    EM = str_replace(EM, "^(\\d+(\\.\\d+)?)-(\\d+(\\.\\d+)?)$", 
                     function(x) {
                       rng <- as.numeric(unlist(str_split(x, "-")))
                       as.character(mean(rng))
                     }),
    across(c(T, L, F, R, N, MV, EM), ~ as.numeric(.))
  )

# CSR strategy scores
csr_scores <- tribble(
  ~KS, ~C_score, ~S_score, ~R_score,
  "ccc", 3, 0, 0,
  "sss", 0, 3, 0,
  "rrr", 0, 0, 3,
  "ccs", 2, 1, 0,
  "css", 1, 2, 0,
  "ccr", 2, 0, 1,
  "crr", 1, 0, 2,
  "ssr", 0, 2, 1,
  "srr", 0, 1, 2,
  "crs", 1, 1, 1
)

eiv <- eiv %>%
  left_join(csr_scores, by = "KS")

# ==============================================================================
# 4. Prepare full species list and matching table
# ==============================================================================
species_list <- plants %>%
  distinct(grouped_name) %>%
  arrange(grouped_name)

# Load existing template or create one if it doesn't exist
template_path <- "data/processed/species_name_matching_template.csv"
if (!file.exists(template_path)) {
  species_list %>%
    mutate(matched_name = NA_character_, comment = NA_character_) %>%
    write_csv(template_path)
}

# Merge full species list with existing matching info
matching_table_existing <- read_csv(template_path)
matching_table <- species_list %>%
  left_join(matching_table_existing, by = "grouped_name")

# ==============================================================================
# 5. Load Stefan Widmer's expert corrections
# ==============================================================================
stefan_matches <- read_excel("data/processed/Fuzzy_Match_Suggestions-SW.xlsx", sheet = 1) %>%
  select(grouped_name = 1, corrected_name = 5) %>%
  filter(!is.na(corrected_name), corrected_name != "")

glimpse(stefan_matches)

# ==============================================================================
# 6. Fuzzy matching support
# ==============================================================================
fuzzy_matches <- stringdist_left_join(
  matching_table %>% select(grouped_name),
  eiv %>% select(Taxon) %>% distinct(),
  by = c("grouped_name" = "Taxon"),
  max_dist = 5,
  method = "lv"
) %>%
  mutate(distance = stringdist(grouped_name, Taxon, method = "lv")) %>%
  group_by(grouped_name) %>%
  arrange(distance) %>%
  slice(1) %>%
  ungroup()

auto_matches <- fuzzy_matches %>%
  filter(distance == 0 |
           str_remove_all(grouped_name, "bdm-agg\\.|bdm\\.|bdm| s\\. l\\.|\\s+") ==
           str_remove_all(Taxon, " s\\. l\\.|aggr\\.|\\s+")) %>%
  rename(auto_match = Taxon)

# ==============================================================================
# 7. Finalize matching table with logic: Stefan > manual > auto > fallback
# ==============================================================================
matching_table_final <- matching_table %>%
  left_join(auto_matches, by = "grouped_name") %>%
  left_join(stefan_matches, by = "grouped_name") %>%
  mutate(
    manual_validated = comment == "green",
    final_name = case_when(
      manual_validated ~ matched_name,
      !is.na(corrected_name) ~ corrected_name,
      !is.na(matched_name) ~ matched_name,
      !is.na(auto_match) ~ auto_match,
      TRUE ~ grouped_name
    ),
    match_status = case_when(
      manual_validated ~ "manual_validated",
      !is.na(corrected_name) ~ "manual_review",
      !is.na(matched_name) ~ "manual",
      !is.na(auto_match) ~ "auto",
      TRUE ~ "original"
    )
  ) %>%
  select(grouped_name, final_name, match_status)

# Export final matching table
write_csv(matching_table_final, "data/processed/species_name_matching_final.csv")

# ==============================================================================
# 8. Join final names with plant data and link to EIV
# ==============================================================================
plants_with_final_names <- plants %>%
  left_join(matching_table_final, by = "grouped_name")

plant_eiv_matched <- plants_with_final_names %>%
  left_join(eiv, by = c("final_name" = "Taxon"))

# Print diagnostics
cat("\nNumber of matched rows:", plant_eiv_matched %>% filter(!is.na(T)) %>% nrow(), "\n")
cat("Number of unmatched species:", plant_eiv_matched %>% filter(is.na(T)) %>% distinct(final_name) %>% nrow(), "\n")

# ==============================================================================
# 9. Export final dataset and unmatched species
# ==============================================================================
write_csv(plant_eiv_matched, "data/processed/plant_eiv_matched.csv")

unmatched_species_remaining <- plant_eiv_matched %>%
  filter(is.na(T)) %>%
  distinct(grouped_name)

write_csv(unmatched_species_remaining, "data/processed/unmatched_species_remaining.csv")


# ==============================================================================
# 10. Final validation checks 
# ==============================================================================

# Check that all final names exist in EIV dataset
missing_final_names <- matching_table_final %>%
  filter(!(final_name %in% eiv$Taxon))
cat("\nSpecies in final table but not in EIV:", nrow(missing_final_names), "\n")

# Check for missing ecological indicator values
na_summary <- plant_eiv_matched %>%
  summarise(across(c(T, L, F, R, N, EM), ~ sum(is.na(.))))
print(na_summary)

# Check that no final_name is NA
cat("\nNumber of NA in final_name:", sum(is.na(plant_eiv_matched$final_name)), "\n")

View(missing_final_names)

# ==============================================================================
# 11. Final test: Show species that could not be matched to EIV (final_name = NA)
# ==============================================================================

# Define species that should have no match (manual_review but unmatched)
unmatched_test_species <- c(
  "Amaranthus hybridus aggr.",
  "Eragrostis lugens",
  "Hieracium pilosum",
  "Primula latifolia"
)

# Check their final_name, match_status, and presence of EIVs
unmatched_check <- plant_eiv_matched %>%
  filter(grouped_name %in% unmatched_test_species) %>%
  group_by(grouped_name) %>%
  summarise(
    final_name = first(final_name),
    match_status = first(match_status),
    has_eiv = !all(is.na(c_across(c(T, L, F, R, N, EM))))
  )

print(unmatched_check)

# Test to verify that a species was correctly matched to EIV data
matching_check <- plant_eiv_matched %>%
  filter(grouped_name == "Viola reichenbachiana bdm-agg.") %>% # Adjust 
  summarise(
    final_name = first(final_name),
    match_status = first(match_status),
    has_eiv = !all(is.na(c_across(c(T, L, F, R, N, EM))))
  )

print(matching_check)





