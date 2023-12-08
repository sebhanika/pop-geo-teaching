# Title: Population pyramids
# Date: 2023-10-10
# Purpose: Script Purpose

# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(gganimate)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# death rate
swe_deaths <- readHMDweb(
    CNTRY = "SWE",
    "Deaths_1x1",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(-c(Female, Male)) %>%
    rename(deaths = Total) %>%
    group_by(Year) %>%
    summarise(across(
        .cols = c(deaths),
        .fns = ~ sum(.x, na.rm = TRUE)
    )) %>%
    ungroup()



swe_pop <- readHMDweb(
    CNTRY = "SWE",
    "Population",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(c(Year, Age, Total2)) %>%
    rename(pop = Total2) %>%
    group_by(Year) %>%
    summarise(across(
        .cols = c(pop),
        .fns = ~ sum(.x, na.rm = TRUE)
    )) %>%
    ungroup()


# crude birth rate
swe_births <- readHMDweb(
    CNTRY = "SWE",
    "Births",
    username = hfd_username,
    password = hfd_password,
    fixup = TRUE
) %>%
    select(-c(Female, Male)) %>%
    rename(births = Total)


swe_dat <- swe_deaths %>%
    left_join(swe_births, by = c("Year")) %>%
    left_join(swe_pop, by = c("Year")) %>%
    mutate(
        cbr = births / (pop / 1000),
        cdr = deaths / (pop / 1000)
    )


plot(swe_dat$Year, swe_dat$cdr, type = "l")
lines(swe_dat$Year, swe_dat$cbr, lty = 2)
