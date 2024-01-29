# Title: DTT_Sweden
# Date: 2024-01-29
# Purpose: Graph showing demographic transition Theory
# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------


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
    ) %>%
    pivot_longer(
        cols = c(cbr, cdr),
        names_to = "categ", values_to = "rates"
    )


dtt_sweden <-
    swe_dat %>%
    ggplot(aes(
        x = Year, y = rates,
        color = categ, linetype = categ
    )) +
    geom_line() +
    scale_color_manual(
        values = (c("#9C6114", "#000080")),
        label = c("Crude Birth Rate", "Crude Death Rate")
    ) +
    scale_linetype_manual(
        values = (c(1, 3)),
        label = c("Crude Birth Rate", "Crude Death Rate")
    ) +
    theme(
        legend.position = c(0.86, 0.915),
        legend.title = element_blank(),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.key.width = unit(1.5, "cm")
    ) +
    labs(
        x = "Year",
        y = "Birth and death rates per 1000 persons",
        title = "Demographic Transition in Sweden",
        caption = "Source: Human Mortality Database (2023)"
    )


ggsave(
    filename = "viszs/dtt_sweden.png",
    plot = dtt_sweden, width = 32, height = 18, units = "cm"
)
