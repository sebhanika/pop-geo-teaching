# Title: Death rate
# Date: 2023-09-22
# Purpose: death rates in countries
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

# Specify countries of interest
mort_countries <- c("SWE", "USA", "JPN", "ESP")


# create labels
cntry_labels <- setNames(
    countrycode(mort_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    mort_countries
)

mort <- list()
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


# Plots --------------

# Plot force of mortality SWE

mort_comb %>%
    filter(Year == 2019, CNTRY == "SWE") %>%
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

swe <- mort_comb %>%
    filter(Year == 2019, CNTRY == "SWE")






# Plot force of mortality USA
mort_comb %>%
    filter(Year == 2019, CNTRY == "USA") %>%
    ggplot(aes(x = Age, y = value, col = Sex)) +
    geom_line() +
    scale_y_log10() +
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
        y = "Death rates (log)", title = "Death rates USA in 2019"
    )



# mid age mortality hump across different countries

mort_comb %>%
    filter(Year == 2019) %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, col = Sex)) +
    facet_wrap(~CNTRY, labeller = as_labeller(cntry_labels)) +
    scale_y_log10() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom"
    )
