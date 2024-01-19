# Title: Mortality Sex
# Date: 2024-01-19
# Purpose: Sex differences in mortality
# /* cSpell:disable */

# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Load data --------------

# Specify countries of interest
mort_countries <- c("SWE", "USA", "JPN", "AUS", "POL", "ESP")
mort <- list()

# download data
for (i in seq_along(mort_countries)) {
    mort[[i]] <- readHMDweb(
        CNTRY = mort_countries[i],
        "Mx_1x1",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )
    mort[[i]]$CNTRY <- mort_countries[i]
}

# combine data
mort_comb <- do.call(dplyr::bind_rows, mort) %>%
    select(-c(Total)) %>%
    pivot_longer(
        cols = c(Female, Male),
        names_to = "Sex"
    ) %>%
    mutate(value = ifelse(value == 0, NA, value))


# mid age mortality hump across different countries

mort_comb %>%
    filter(Year == 2019) %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, col = Sex)) +
    facet_wrap(~CNTRY) +
    scale_y_log10() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom"
    )
