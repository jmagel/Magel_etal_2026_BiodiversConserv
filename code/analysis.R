

## IMPACTS OF SEA LEVEL RISE ON THE WORLD'S SHOREBIRDS

# Jennifer M.T. Magel [1], Scott Wilson [1,2], and Tara G. Martin [1]

# [1] Conservation Decisions Lab, Department of Forest & Conservation Sciences, University of British Columbia, 
#     Vancouver, BC, Canada V6T 1Z4
# [2] Wildlife Research Division, Environment and Climate Change Canada, University of Alberta, 
#     Edmonton, AB, Canada T6G 2E9

# Corresponding author: Jennifer M.T. Magel (jenn.magel@gmail.com)

# Description: Code used to calculate basic statistics for the above paper


################################################################################################################

## SETUP

## Set working directory
# NOTE: Please ensure that the working directory is set to match the location of the folder on your computer
setwd("C:/...")

## Load the data
study_full <- read.csv("data/SLR_study_full.csv", stringsAsFactors = TRUE)
study_short <- read.csv("data/SLR_study_short.csv", stringsAsFactors = TRUE)
study_species <- read.csv("data/SLR_study_species.csv", stringsAsFactors = TRUE)
study_direction <- read.csv("data/SLR_study_direction.csv", stringsAsFactors = TRUE)

## Load necessary packages
library(dplyr)


################################################################################################################

## STUDY TIMELINE

# Calculate the number of studies for each endpoint year
n_endpoint <- study_full %>% group_by(end_year) %>% summarise(count = n_distinct(study_id))
n_endpoint

# Create a data frame containing only the studies ending in 2100
study_2100 <- subset(study_full, end_year == "2100")

# Calculate the mean and SD of study length for studies ending in 2100
length_2100 <- study_2100 %>% group_by(study_id, end_year, study_length) %>% summarise(endpoint = mean(end_year))
mean(length_2100$study_length, na.rm = TRUE)
sd(length_2100$study_length, na.rm = TRUE)

# Calculate the mean and SD for study length overall
length_all <- study_full %>% group_by(study_id, end_year, study_length) %>% summarise(endpoint = mean(end_year))
mean(length_all$study_length, na.rm = TRUE)
sd(length_all$study_length, na.rm = TRUE)


################################################################################################################

## ANNUAL CYCLE

# Calculate the number of studies from each stage of the annual cycle
n_anncycle <- study_full %>% group_by(annual_cycle) %>% summarise(count = n_distinct(study_id))
n_anncycle

# Calculate the number of studies from each stage of the annual cycle, by habitat type
n_anncycle_hab <- study_direction %>% group_by(impact_coarse, habitat_type, annual_cycle) %>% summarise(count = n_distinct(study_id))
n_anncycle_hab

# Calculate the number of studies from each stage of the annual cycle, by type and direction of impact
n_anncycle_change <- study_direction %>% group_by(annual_cycle, impact_coarse, change_mean) %>% summarise(count = n_distinct(study_id))
n_anncycle_change


################################################################################################################

# IMPACT (habitat vs. populations/individuals)

# Calculate the number of studies for each type of broad SLR impact
n_impact <- study_full %>% group_by(impact_coarse) %>% summarise(count = n_distinct(study_id))
n_impact

# Calculate the number of studies for each type of broad SLR impact during each stage of the annual cycle
n_impact_ann <- study_full %>% group_by(impact_coarse, annual_cycle) %>% summarise(count = n_distinct(study_id))
n_impact_ann

# Calculate the number of studies for each type of fine SLR impact
n_impact_fine <- study_full %>% group_by(impact_coarse, impact_fine) %>% summarise(count = n_distinct(study_id))
n_impact_fine


################################################################################################################

## SPECIES

# Calculate the number of studies for each species
n_species <- study_species %>% group_by(species_name) %>% summarise(count = n_distinct(study_id))
n_species

# Calculate the number of species in each family
n_family <- study_species %>% group_by(family) %>% summarise(count = n_distinct(species_name))
n_family


################################################################################################################

## FLYWAY/COUNTRY

# Calculate the number of studies from each flyway
n_flyway <- study_full %>% group_by(flyway) %>% summarise(count = n_distinct(study_id))
n_flyway

# Calculate the number of studies from each country
n_country <- study_full %>% group_by(country) %>% summarise(count = n_distinct(study_id))
n_country


################################################################################################################


