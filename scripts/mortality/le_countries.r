# Title: Life Expectancy
# Date: 2024-01-04
# Purpose: Life expectancy for selected countries
# /* cSpell:disable */

# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(countrycode)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# life expectancy graphs --------------

# Specify countries of interest
le_countries <- c("SWE", "JPN", "DNK", "ESP", "NLD")

# create labels
cntry_labels <- setNames(
    countrycode(le_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    le_countries
)

# download data
le <- list()

for (i in seq_along(le_countries)) {
    le[[i]] <- readHMDweb(
        CNTRY = le_countries[i],
        "E0per",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )
    le[[i]]$CNTRY <- le_countries[i]
}

# combine data
le_comb <- do.call(dplyr::bind_rows, le) %>%
    janitor::clean_names()

# create plot object
le_plot <- le_comb %>%
    filter(year %in% 1900:2021) %>%
    ggplot() +
    geom_line(aes(
        x = year, y = female,
        color = cntry, linetype = cntry
    ), lwd = 1.25) +
    scale_x_continuous(
        limits = c(1900, 2021),
        breaks = seq(1900, 2020, 20)
    ) +
    scale_color_manual(
        values = park_palette("Saguaro"),
        labels = cntry_labels
    ) +
    scale_linetype_manual(
        values = c(1, 2, 3, 4, 6),
        labels = cntry_labels
    ) +
    labs(
        x = "", y = "Female Period Life Expectancy in Years",
        title = "Female Life Expectancy in selected countries",
        caption = "Source: Human Mortality Database (2023)"
    ) +
    theme(
        legend.position = c(0.89, 0.15),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm")
    )
le_plot

# save plot for presentation
ggsave(
    filename = "viszs/le_countries.png",
    plot = le_plot,
    width = 32, height = 18, units = "cm"
)
