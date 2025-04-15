# -------------------------------------------------------------------
# Script Name: 02_*****.R
# Purpose:     Analyze changes in species richness and frequency 
#              over time using BDM Z9 vegetation data (plants & moss)
# Author:      Pascal Felix
# Date:        2025-04-01
# -------------------------------------------------------------------

# Notes from last meeting:

# Quasi Gamme species richness über alle Arten und Jahre.
# Was genau wird noch untersucht?
# Species Richeness / nach Lebensraum, Höhe (in Klassen aufteilen, oder andere Heransgehensweise, bspw. kontinuierlich), 
# biographischem Raum (BGR_6) - wie verteilen sich diese über die Höhenstufen? / Vergleich per Periode.
# Subsamples von jedem Jahr nehmen oder 5 Epochen. Subsamples bringt mehr Möglichkeiten, mehr Datenpunkte.
# Wenn viele unterschiedliche Parameter berücksichtigt werden, evt. Epochen anstelle von Subsamples nehmen.
# Mittlere Zeigerwerte, (Landolt nehmen), Nährstoffzahl beispielsweise.

# Höhenstufe: Einfache Lösung mit Höhenstufen, die bereits verwendet wurden
# Besser wäre kontinuierlich.
# Poisson GLMM bei species richness (Zeit-ID)
# Katrins Paper bereits als public nehmen also nehmen und zitieren.



# -----------------------------
# Load Required Packages
# -----------------------------
# Install if necessary:
# install.packages(c("vegan", "mgcv", "car", "emmeans", "performance", "readxl", "dplyr", "tidyverse"))

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(mgcv)
library(car)
library(emmeans)
library(performance)
library(vegan)

# -----------------------------
# Load Data
# -----------------------------

setwd("C:/Users/pasca/Desktop/bdm-z9-vegetation-diversity/bdm-z9-vegetation-diversity")

kopfdaten <- read_excel("data/raw/2000_Z9_Kopfdaten.xlsx")
pflanzen <- read_excel("data/raw/2000_Z9_Pflanzen.xlsx")
moose <- read_excel("data/raw/2000_Z9_Moose.xlsx")
checklist <- read_excel("data/raw/Checklist_2017_simple_version_20230503.xlsx", sheet = "Checklist 2017", skip = 4)
bryo_list <- read_excel("data/raw/Artenliste Swissbryophytes Konzept7_2018_JD.xlsx", sheet = "Bearbeitet JD")

# -----------------------------
# Prepare and Join Species Data
# -----------------------------

moose_clean <- moose %>%
  filter(aID_KD %in% kopfdaten$aID_KD[!is.na(kopfdaten$yearMoos)]) %>%
  mutate(species_name = str_trim(paste(Gattung, Art)))

bryo_list_clean <- bryo_list %>%
  mutate(species_name = str_trim(`Full name without authorities`)) %>%
  select(bryo_taxa_id, taxonnamen_id, species_name, Gattung, Art, name_D, name_E) %>%
  distinct(species_name, .keep_all = TRUE)

moose_matched <- moose_clean %>%
  left_join(bryo_list_clean, by = "species_name")

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

moose_grouped <- moose_matched %>%
  left_join(checklist_clean, by = c("species_name" = "taxon_name")) %>%
  mutate(grouped_name = coalesce(parent_name, species_name))

pflanzen_clean <- pflanzen %>%
  filter(aID_KD %in% kopfdaten$aID_KD[!is.na(kopfdaten$yearPl)]) %>%
  mutate(species_name = str_trim(paste(Gattung, Art)))

pflanzen_grouped <- pflanzen_clean %>%
  left_join(checklist_clean, by = c("species_name" = "taxon_name")) %>%
  mutate(grouped_name = coalesce(parent_name, species_name))

pflanzen_joined <- pflanzen_grouped %>%
  left_join(kopfdaten, by = "aID_KD")

# -----------------------------
# Plant Richness Analysis
# -----------------------------

pflanzen_richness <- pflanzen_joined %>%
  filter(!is.na(grouped_name), !is.na(yearPl)) %>%
  group_by(yearPl, aID_KD) %>%
  summarise(richness = n_distinct(grouped_name), .groups = "drop")

# Diagnose für mod_gam_rich
gam.check(mod_gam_rich)
plot(mod_gam_rich, pages = 1, residuals = TRUE, rug = TRUE, shade = TRUE)

res <- residuals(mod_gam_rich)
fit <- fitted(mod_gam_rich)

ggplot(data.frame(fit = fit, res = res), aes(x = fit, y = res)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted (Plant GAM)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Period comparison
pflanzen_richness <- pflanzen_richness %>%
  mutate(period = case_when(
    yearPl %in% 2001:2005 ~ "2001-2005",
    yearPl %in% 2016:2020 ~ "2016-2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

ggplot(pflanzen_richness, aes(x = period, y = richness, fill = period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Plant Richness Comparison by Period", y = "Richness") +
  theme_minimal()

shapiro.test(pflanzen_richness$richness[pflanzen_richness$period == "2001-2005"])
t.test(richness ~ period, data = pflanzen_richness)

# Environmental model (Plants)
gam_env <- gam(richness ~ s(Hoehe) + BGR_6,
               data = pflanzen_richness %>%
                 left_join(kopfdaten, by = "aID_KD"),
               method = "REML")
summary(gam_env)
check_model(gam_env)

# -----------------------------
# Moss Richness Analysis
# -----------------------------

moose_joined <- moose_grouped %>%
  left_join(kopfdaten, by = "aID_KD")

moose_richness <- moose_joined %>%
  filter(!is.na(grouped_name), !is.na(yearMoos)) %>%
  group_by(yearMoos, aID_KD) %>%
  summarise(richness = n_distinct(grouped_name), .groups = "drop")

mod_gam_rich_moos <- gam(richness ~ s(yearMoos), data = moose_richness, method = "REML")
summary(mod_gam_rich_moos)
check_model(mod_gam_rich_moos)

ggplot(moose_richness, aes(x = yearMoos, y = richness)) +
  geom_point(alpha = 0.3) +
  stat_smooth(method = "gam", formula = y ~ s(x), method.args = list(method = "REML")) +
  labs(title = "Moss Species Richness Trend (GAM)", x = "Year", y = "Species Richness") +
  theme_minimal()

# Period comparison
moose_richness <- moose_richness %>%
  mutate(period = case_when(
    yearMoos %in% 2001:2005 ~ "2001-2005",
    yearMoos %in% 2016:2020 ~ "2016-2020",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period))

ggplot(moose_richness, aes(x = period, y = richness, fill = period)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Moss Richness Comparison by Period", y = "Richness") +
  theme_minimal()

shapiro.test(moose_richness$richness[moose_richness$period == "2001-2005"])
t.test(richness ~ period, data = moose_richness)

# -----------------------------
# Interaction Model: Elevation x Year (Moss)
# -----------------------------

moose_richness_cont <- moose_richness %>%
  left_join(kopfdaten, by = "aID_KD") %>%
  filter(!is.na(Hoehe), !is.na(richness), !is.na(yearMoos))

# Tensor product (flexible interaction)
mod_te <- gam(richness ~ te(yearMoos, Hoehe), data = moose_richness_cont, method = "REML")
summary(mod_te)
check_model(mod_te)

# Predict on grid for visualization
grid <- expand.grid(
  yearMoos = seq(min(moose_richness_cont$yearMoos), max(moose_richness_cont$yearMoos), length.out = 50),
  Hoehe = seq(min(moose_richness_cont$Hoehe), max(moose_richness_cont$Hoehe), length.out = 50)
)

grid$richness_pred <- predict(mod_te, newdata = grid)

ggplot(grid, aes(x = yearMoos, y = Hoehe, fill = richness_pred)) +
  geom_tile() +
  scale_fill_viridis_c(option = "D") +
  labs(title = "Predicted Moss Richness by Year and Elevation",
       x = "Year", y = "Elevation (m)", fill = "Predicted\nRichness") +
  theme_minimal()

# Environmental model (Moss)
gam_env_moos <- gam(richness ~ s(Hoehe) + BGR_6,
                    data = moose_richness %>%
                      left_join(kopfdaten, by = "aID_KD"),
                    method = "REML")
summary(gam_env_moos)
check_model(gam_env_moos)