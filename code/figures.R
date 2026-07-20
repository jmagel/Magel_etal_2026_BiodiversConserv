

## IMPACTS OF SEA LEVEL RISE ON THE WORLD'S SHOREBIRDS

# Jennifer M.T. Magel [1], Scott Wilson [1,2], and Tara G. Martin [1]

# [1] Conservation Decisions Lab, Department of Forest & Conservation Sciences, University of British Columbia, 
#     Vancouver, BC, Canada V6T 1Z4
# [2] Wildlife Research Division, Environment and Climate Change Canada, University of Alberta, 
#     Edmonton, AB, Canada T6G 2E9

# Corresponding author: Jennifer M.T. Magel (jenn.magel@gmail.com)

# Description: Code used to produce figures (Fig. 2, Fig. 6, Fig. S1) for the above paper


################################################################################################################

## SETUP

## Set working directory
# NOTE: Please ensure that the working directory is set to match the location of the folder on your computer
setwd("C:/...")

## Load the data
study_data <- read.csv("data/SLR_study_full.csv", stringsAsFactors = TRUE)
study_data_plot <- read.csv("data/SLR_study_short.csv", stringsAsFactors = TRUE)
species <- read.csv("data/SLR_study_species.csv", stringsAsFactors = TRUE)
direction <- read.csv("data/SLR_study_direction.csv", stringsAsFactors = TRUE)

## Load necessary packages
library(dplyr)
library(ggplot2)
library(cowplot)
library(beyonce)
library(egg)


                                            #####################
############################################### DATA CLEANING ##############################################
                                            #####################

## Fix incorrect characters in flyway names
study_data_plot$flyway <- as.character(study_data_plot$flyway)
study_data_plot$flyway[study_data_plot$flyway == "East Asian-Australasian"] <- "East Asian–Australasian"
study_data_plot$flyway[study_data_plot$flyway == "Black Sea-Mediterranean"] <- "Black Sea–Mediterranean"
study_data_plot$flyway[study_data_plot$flyway == "West Asian-East African"] <- "West Asian–East African"
study_data_plot$flyway <- as.factor(study_data_plot$flyway)
levels(study_data_plot$flyway)

study_data$country <- as.character(study_data$country)
study_data$country[study_data$country == "East Asia-Australasia"] <- "East Asia–Australasia"
study_data$country <- as.factor((study_data$country))

study_data_plot$country <- as.character(study_data_plot$country)
study_data_plot$country[study_data_plot$country == "East Asia-Australasia"] <- "East Asia–Australasia"
study_data_plot$country <- as.factor((study_data_plot$country))

## Re-order flyway code variable
study_data$flyway_code <- factor(study_data$flyway_code, 
                                      levels(factor(study_data$flyway_code))[c(6,5,1,3,2,7,4)])
levels(study_data$flyway_code)

study_data_plot$flyway_code <- factor(study_data_plot$flyway_code, 
                                  levels(factor(study_data_plot$flyway_code))[c(6,5,1,3,2,7,4)])
levels(study_data_plot$flyway_code)

## Re-order annual cycle variable
study_data_plot$annual_cycle <- factor(study_data_plot$annual_cycle,
                                       levels(factor(study_data_plot$annual_cycle))[c(1,2,3,5,6,4,7,8)])
levels(study_data_plot$annual_cycle)

## Re-name 'bird' factor level
study_data_plot$impact_coarse <- as.character(study_data_plot$impact_coarse)
study_data_plot$impact_coarse[study_data_plot$impact_coarse == "Bird"] <- "Pop./indiv."
study_data_plot$impact_coarse <- as.factor(study_data_plot$impact_coarse)


                                              ################
################################################# FIGURE 2 #################################################
                                              ################

## CREATE/PREPARE DATA FRAMES FOR PLOTTING

## Consolidate data frame to determine the number of studies for different combinations of variables
# Number of studies for each year/annual cycle combination
study_data_year <- study_data_plot %>% group_by(pub_year, annual_cycle) %>% 
  summarise(count = n_distinct(study_id))
# Number of studies for each flyway/country combination
study_data_flyway <- study_data_plot %>% group_by(flyway_code, country) %>% 
  summarize(count = n_distinct(study_id))
# Number of studies for each impact/annual cycle combination
study_data_impact <- study_data_plot %>% group_by(impact_coarse, annual_cycle) %>% 
  summarize(count = n_distinct(study_id))

## Perform additional processing on 'species' data frame to limit plot to the top 9 most studied species
# Determine the most-studied species
species_count <- species %>% group_by(species_name, species_code) %>% 
  summarize(count = n_distinct(study_id))
# Create vector with only the top 9 most-studies species
species_top_list <- c("PIPL", "REKN", "EUOY", "DUNL", "EUCU", "CREH", "BTGO", "BLTG","SNPL")
# Create data frame with only the top 9 most-studies species
species_top <- species[species$species_code %in% species_top_list, ]
species_top$species_code <- droplevels(species_top$species_code)
# Order species codes from most to least number of studies
species_top$species_code <- factor(species_top$species_code, 
                                          levels(factor(species_top$species_code))[c(7,8,6,4,5,3,2,1,9)])
levels(species_top$species_code)
# Determine the number of studies for each species/annual cycle combination
study_data_species <- species_top %>% group_by(species_code, annual_cycle) %>% 
  summarise(count = n_distinct(study_id))
# Re-order annual cycle variable
droplevels(study_data_species$annual_cycle)
study_data_species$annual_cycle <- factor(study_data_species$annual_cycle,
                                       levels(factor(study_data_species$annual_cycle))[c(1,3,4,2,5)])
levels(study_data_species$annual_cycle)


###################################################################################

## SET PLOT SPECIFICATIONS

## Set theme
theme_set(theme_cowplot(font_size = 12)) 

## Set colour palettes
year_pal <- c("#182B64", "#E1C24B", "#B47FAE", "#F2F4F5", "#B9DBF1", "#7A5AA6", "#7891BA", "#CBB394")

species_pal <- c("#182B64", "#4990C2", "#B9DBF1", "#7A5AA6", "#CBB394")


country_pal <- c("#339ea1", "#252b5b", "#0C66ac", "#6fa7b9", "#80ccdf", "#edf8fb", 
                 "#fcea1b", "#f9c043", "#f97e96", "#eac89b", "#e69ea6")



## Define tag_facet function (to add labels to individual plots)
tag_facet <- function(p, open = "", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}


###################################################################################

## PLOT THE DATA

# Year (Fig. 2a)
p2a <- ggplot(study_data_year, aes(pub_year, count)) +
  geom_bar(aes(fill = annual_cycle), colour = "black", stat = "identity", position = "stack") +
  scale_x_continuous(breaks = seq(2001, 2025, 2)) +
  xlab("Year") + ylab("Number of studies") +
  guides(fill = guide_legend(title = "Annual cycle")) +
  scale_fill_manual(values = year_pal)

# Impact (Fig. 2b)
p2b <- ggplot(study_data_impact, aes(impact_coarse, count)) +
  geom_bar(aes(fill = annual_cycle), colour = "black", stat = "identity", position = "stack") +
  ylim(c(0,30)) +
  xlab("Type of impact") + ylab("Number of studies") +
  guides(fill = guide_legend(title = "Annual cycle")) +
  scale_fill_manual(values = year_pal)

# Species (Fig. 2c)
p2c <- ggplot(study_data_species, aes(species_code, count)) +
  geom_bar(aes(fill = annual_cycle), colour = "black", stat = "identity", position = "stack") +
  scale_y_continuous(limits = c(0,10), breaks = seq(0, 10, 2)) +
  xlab("Species") + ylab("Number of studies") +
  guides(fill = guide_legend(title = "Annual cycle")) +
  scale_fill_manual(values = species_pal)

# Flyway/country (Fig. 2d)
p2d <- ggplot(study_data_flyway, aes(flyway_code, count)) +
  geom_bar(aes(fill = country), colour = "black", stat = "identity", position = "stack") +
  xlab("Flyway") + ylab("Number of studies") +
  guides(fill = guide_legend(title = "Country")) +
  scale_fill_manual(values = country_pal)


## Make dummy plot with all levels of annual_cycle legend
# Make new data frame
dummy_data <- study_data_plot
# Change 'annual cycle' designation of one study to 'Migration'
dummy_data$annual_cycle <- as.character(dummy_data$annual_cycle)
dummy_data$annual_cycle[dummy_data$study_id == "Iwamura et al. 2013"] <- "Migration"
# Re-order annual cycle variable
dummy_data$annual_cycle <- as.factor(dummy_data$annual_cycle)
levels(dummy_data$annual_cycle)

dummy_data$annual_cycle <- factor(dummy_data$annual_cycle,
                                       levels(factor(dummy_data$annual_cycle))[c(1,2,3,5,6,7,4,8,9)])
levels(dummy_data$annual_cycle)
# 
dummy_data_year <- dummy_data %>% group_by(pub_year, annual_cycle) %>% 
  summarise(count = n_distinct(study_id))
# Set colour palette
dummy_pal <- c("#182B64", "#E1C24B", "#B47FAE", "#4990C2", "#F2F4F5", "#B9DBF1", "#7A5AA6", "#7891BA", "#CBB394")


# Make dummy plot
p2x <- ggplot(dummy_data_year, aes(pub_year, count)) +
  geom_bar(aes(fill = annual_cycle), colour = "black", stat = "identity", position = "stack") +
  scale_x_continuous(breaks = seq(2002, 2024, 2)) +
  xlab("Year") + ylab("Number of studies") +
  guides(fill = guide_legend(title = "Annual cycle")) +
  scale_fill_manual(values = dummy_pal)


## Combine all panels in a single figure and export plot
legend1 <- get_legend(p2x)
legend2 <- get_legend(p2d)

fig2_top <- plot_grid(p2a + theme(legend.position = "none"), 
                      p2b + theme(legend.position = "none"),
                      legend1,
                      rel_widths = c(2.25, 1.25, 0.65),
                      labels = c("a)", "b)", ""),
                      hjust = -0.25,
                      nrow = 1)

fig2_bottom <- plot_grid(p2c + theme(legend.position = "none"),
                         p2d + theme(legend.position = "none"),
                         legend2,
                         rel_widths = c(1.75, 1.75, 0.65),
                         labels = c("c)", "d)", ""),
                         hjust = -0.25,
                         nrow = 1)

fig2 <- plot_grid(fig2_top, fig2_bottom, nrow = 2)

jpeg(filename = "fig_2.jpg",
     width = 13, height = 8, units = "in", res = 400)

fig2

dev.off()


                                              ################
################################################# FIGURE 6 #################################################
                                              ################


## CREATE DATAFRAMES FOR PLOTTING

## Create new data frame with only those values needed for plotting
direction_summary <- direction %>% group_by(annual_cycle, impact_coarse, dynamic, habitat_type, change_mean) %>% 
  summarize(count = n_distinct(study_id))
# Remove one study focusing on 'resident' species
direction_summary <- direction_summary[!(direction_summary$annual_cycle == "Resident"), ]

## Create separate data frames for static habitat, dynamic habitat, and bird datasets
direction_static <- direction_summary[direction_summary$impact_coarse == "Habitat" & 
                                        direction_summary$dynamic == "no", ]
direction_dynamic <- direction_summary[direction_summary$impact_coarse == "Habitat" & 
                                         direction_summary$dynamic == "yes", ]
direction_bird <- direction_summary[direction_summary$impact_coarse == "Bird", ] %>% 
  group_by(annual_cycle, habitat_type, change_mean) %>% summarize(count = sum(count))

# Drop unnecessary factor levels for each data frame
droplevels(direction_static$change_mean)
droplevels(direction_bird$change_mean)

# Re-order 'change_mean' metric for plotting
direction_dynamic$change_mean <- factor(direction_dynamic$change_mean, 
                                        levels(factor(direction_dynamic$change_mean))[c(3,1,2)])
levels(direction_dynamic$change_mean)


###################################################################################

## SET PLOT SPECIFICATIONS

## Set theme
theme_set(theme_bw(base_size = 14))

## Set colour palettes
static_pal <- c("#eac89b", "#0C66ac", "#252b5b", "#80ccdf") # Palette for 'static' habitat studies
dynamic_pal <- c("#eac89b", "#0C66ac", "#252b5b", "#80ccdf") # Palette for 'dynamic' habitat studies
bird_pal <- c("#eac89b", "#0C66ac", "#e69ea6", "#252b5b", "#80ccdf") # Palette for 'bird' studies


###################################################################################

## PLOT THE DATA

## Create individual plots for static habitat, dynamic habitat, and bird datasets
# Static
p6a <- ggplot(direction_static, aes(change_mean, count)) +
  geom_bar(aes(fill = habitat_type), colour = "black", stat = "identity", position = "stack") +
  xlab("Direction of impact") + ylab("Number of studies") +
  facet_wrap(~annual_cycle, nrow = 3) +
  scale_y_continuous(limits = c(0, 13)) +
  guides(fill = guide_legend(title = "Habitat type")) +
  scale_fill_manual(values = static_pal) +
  theme_cowplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.x = element_text(colour = "white"))

# Dynamic
p6b <- ggplot(direction_dynamic, aes(change_mean, count)) +
  geom_bar(aes(fill = habitat_type), colour = "black", stat = "identity", position = "stack") +
  xlab("Direction of impact") + ylab("Number of studies") +
  facet_wrap(~annual_cycle, nrow = 3) +
  scale_y_continuous(limits = c(0, 13)) +
  guides(fill = guide_legend(title = "Habitat type")) +
  scale_fill_manual(values = dynamic_pal) +
  theme_cowplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Populations/individuals
p6c <- ggplot(direction_bird, aes(change_mean, count)) +
  geom_bar(aes(fill = habitat_type), colour = "black", stat = "identity", position = "stack") +
  xlab("Direction of impact") + ylab("Number of studies") +
  facet_wrap(~annual_cycle, nrow = 3) +
  scale_y_continuous(limits = c(0, 13)) +
  guides(fill = guide_legend(title = "Habitat type")) +
  scale_fill_manual(values = bird_pal) +
  theme_cowplot() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(colour = "white"))


## Combine all panels in a single figure and export plot
fig6 <- plot_grid(p6a + theme(legend.position = "none"),
                  p6b + theme(legend.position = "none"),
                  p6c + theme(legend.position = c(0.125, 0.53)),
                  rel_widths = c(1.4,3,2),
                  nrow = 1)

jpeg(filename = "fig_6.jpg",
     width = 11, height = 8, units = "in", res = 400)

fig6

dev.off()

                                             ###################
################################################# FIGURE S1 #################################################
                                             ###################

## PREPARE DATAFRAMES FOR PLOTTING

## Create new data frame with only those values needed for plotting
anncycle_impact <- study_data %>% group_by(annual_cycle, impact_coarse, flyway_code, country) %>% 
  summarize(count = n_distinct(study_id))
# Remove 'resident' and 'full annual cycle' studies
anncycle_impact <- anncycle_impact[!(anncycle_impact$annual_cycle == "Full annual cycle"), ]
anncycle_impact <- anncycle_impact[!(anncycle_impact$annual_cycle == "Resident"), ]
## Re-name 'Bird' factor level
anncycle_impact$impact_coarse <- as.character(anncycle_impact$impact_coarse)
anncycle_impact$impact_coarse[anncycle_impact$impact_coarse == "Bird"] <- "Populations/individuals"
anncycle_impact$impact_coarse <- as.factor(anncycle_impact$impact_coarse)


###################################################################################

## PLOT THE DATA

pS1 <- ggplot(anncycle_impact, aes(flyway_code, count)) +
  geom_bar(aes(fill = country), colour = "black", stat = "identity", position = "stack") +
  xlab("Flyway") + ylab("Number of studies") +
  facet_grid(annual_cycle ~ impact_coarse) +
  guides(fill = guide_legend(title = "Country")) +
  scale_fill_manual(values = country_pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_rect(colour = "black"))

pS1 <- tag_facet(pS1)

## Export plot
jpeg(filename = "fig_S1.jpg",
     width = 13, height = 8, units = "in", res = 400)

pS1

dev.off()

