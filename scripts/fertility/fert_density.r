# Title: Fertilty - Pop Density
# Date: 2024-01-25
# Purpose: Script Purpose
# /* cSpell:disable */


# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(eurostat)
library(wbstats)

source("scripts/0_config.R")
source("scripts/0_settings.R")


### Not sure if it will be included in the lecture

# World --------------

world_tfr <- wb_data("SP.DYN.TFRT.IN", start_date = 2000, end_date = 2000) %>%
    rename(tfr = SP.DYN.TFRT.IN) %>%
    select(c(tfr, date, iso3c))

world_popdens <- wb_data("EN.POP.DNST", start_date = 1995, end_date = 1995) %>%
    rename(pop_dens = "EN.POP.DNST") %>%
    select(pop_dens, date, iso3c)


world_comb <- world_tfr %>%
    left_join(world_popdens, by = c("iso3c")) %>%
    mutate(region = countrycode(
        sourcevar = iso3c,
        origin = "iso3c",
        destination = "region"
    )) %>%
    drop_na(region)



world_comb %>%
    drop_na() %>%
    ggplot(aes(x = log(pop_dens), y = tfr)) +
    geom_point(aes(color = region), size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(y = "TFR 2000", x = "Population density 1995")



min(world_comb$pop_dens, na.rm = T)



# Europe --------------
euro_tfr <-
    get_eurostat("demo_r_find3", time_format = "num") %>%
    filter(
        nchar(geo) == 5,
        indic_de == "TOTFERRT",
        TIME_PERIOD == 2018
    ) %>%
    filter(!grepl("^TR|^TR$", geo)) %>% # Turkey
    select(c(geo, values)) %>%
    rename(tfr = values)


euro_popdens <-
    get_eurostat("demo_r_d3dens", time_format = "num") %>%
    filter(
        nchar(geo) == 5,
        TIME_PERIOD == 2018
    ) %>%
    select(c(geo, values)) %>%
    rename(pop_dens = values)


dat_comb <- euro_tfr %>%
    left_join(euro_popdens)


dat_comb %>%
    filter(tfr < 3) %>%
    ggplot(aes(x = pop_dens, y = tfr)) +
    geom_point() +
    scale_x_log10() +
    geom_smooth(method = "lm")
