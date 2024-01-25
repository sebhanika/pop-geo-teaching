# Title: Mean_age_birth
# Date: 2024-01-24
# Purpose: Graph showing mean age at birth
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
birth_countries <- c("SWE", "PRT", "JPN", "USA")

# create labels
cntry_labels <- setNames(
    countrycode(birth_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    birth_countries
)

brth <- list()
for (i in seq_along(birth_countries)) {
    brth[[i]] <- readHFDweb(
        CNTRY = birth_countries[i],
        "mabRR",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )
    brth[[i]]$CNTRY <- birth_countries[i]
}

# combine data
brth_comb <- do.call(dplyr::bind_rows, brth) %>%
    janitor::clean_names()

plot_mab <- brth_comb %>%
    ggplot(aes(
        x = year, y = mab, color = cntry, linetype = cntry
    )) +
    geom_line(linewidth = 1.2, alpha = 0.85) +
    scale_color_manual(
        values = park_palette("Saguaro", length(birth_countries)),
        labels = cntry_labels
    ) +
    scale_linetype_manual(
        values = c(1, 2, 3, 4, 6),
        labels = cntry_labels
    ) +
    labs(
        x = "Age of mother",
        y = "Mean Age of Women at Birth",
        title = "Mean Age at Birth in selected Countries",
        caption = "Source: Human Fertility Database (2023)"
    ) +
    theme(
        legend.position = c(0.12, 0.15),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm")
    )
plot_mab


ggsave(
    filename = "viszs/ma_births.png",
    plot = plot_mab, width = 32, height = 18, units = "cm"
)
