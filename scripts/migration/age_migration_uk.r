# Title: Age_migration_UK
# Date: 2024-02-01
# Purpose: Graph for age migration patterns
# /* cSpell:disable */

# Libraries --------------
library(tidyverse)
library(geojsonsf)
library(sf)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# data downloaded from ONS.gov.uk
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset # nolint

dat1 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_1.csv") # nolint
dat2 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_2.csv") # nolint

# Age distribution migrtion
age_mig <- bind_rows(dat1, dat2) %>%
    group_by(age) %>%
    summarise(moves = sum(moves, na.rm = TRUE)) %>%
    ungroup()

plot_age_mig <-
    age_mig %>%
    ggplot(aes(x = age, y = moves)) +
    geom_line(lwd = 1.25, color = "#000080") +
    scale_x_continuous(
        limits = c(0, 110),
        breaks = seq(0, 110, 10)
    ) +
    labs(
        x = "Age", y = "Number of migrations",
        caption = "Source: Office for National Statistics (2023)",
        title = "Age distribution of migration in England & Wales in 2021"
    )
plot_age_mig

ggsave(
    filename = "viszs/age_mig.png", plot = plot_age_mig,
    width = 32, height = 18,
    units = "cm"
)
