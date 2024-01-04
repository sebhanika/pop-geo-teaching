# Title: Deaths
# Date: 2023-09-22
# Purpose: deaths in sweden
# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)
library(countrycode)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------

deaths <-
    readHMDweb(
        CNTRY = "SWE",
        item = "Deaths_1x1",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    ) %>%
    select(-c(Total)) %>%
    pivot_longer(
        cols = c(Female, Male),
        names_to = "Sex"
    ) %>%
    mutate(value = ifelse(value == 0, NA, value))


deaths %>%
    filter(Year == 2019) %>%
    ggplot(aes(x = Age, y = value, col = Sex)) +
    geom_line() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = c(0.92, 0.05),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        )
    ) +
    labs(
        x = "Age",
        y = "Death rates", title = "Death rates Sweden in 2019"
    )
