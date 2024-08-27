# Title: pop_projections_EU
# Date: 2024-08-23
# Purpose: Population Projections EU, based on Eurostat

# /* cSpell:disable */
# Load libraries --------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(eurostat)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Eurostat projections --------------

# get data
age_proj_raw <- get_eurostat("proj_23np", time_format = "num")

# define countries
proj_countries <- c("SE", "PL", "ES", "NL", "BG", "EU27_2020")

# create labels
country_labels <- c(
    SE = "Sweden", PL = "Poland", ES = "Spain",
    NL = "Netherlands", BG = "Bulgaria", EU27_2020 = "EU27"
)

proj_labels <- c(
    BSL = "Baseline", LFRT = "Lower Fertility",
    LMRT = "Lower Mortality", HMIGR = "Higher Migration",
    LMIGR = "Lower Migration", NMIGR = "No Migration"
)


# create filtered data for plots
dat_country_proj <-
    age_proj_raw %>%
    filter(
        geo %in% proj_countries,
        age %in% c("TOTAL", "Y_GE65"),
        sex == "T"
    ) %>%
    group_by(projection, geo, TIME_PERIOD) %>%
    reframe(values_new = values / lag(values, 1)) %>%
    drop_na(values_new) |>
    rename(time = TIME_PERIOD) |>
    mutate(projection = factor(projection,
        levels = c("BSL", "NMIGR", "LMIGR", "HMIGR", "LMRT", "LFRT")
    ))

# plot object
plot_country_proj <- dat_country_proj %>%
    ggplot(aes(
        x = time, y = values_new,
        color = projection, linetype = projection
    )) +
    geom_line(lwd = 1.2) +
    scale_color_manual(
        values = c(
            "#252525", "#83111e", "#eb6a37", "#c98b68",
            "#4393c3", "#35978f"
        ),
        labels = proj_labels
    ) +
    scale_linetype_manual(
        values = c(1, 3, 3, 3, 2, 5),
        labels = proj_labels
    ) +
    labs(
        title = "Population Projections EU - Age dependency ratio",
        x = "Year",
        y = "Share of people aged 65+",
        caption = "Source: Eurostat (2024)"
    ) +
    facet_wrap(~geo, nrow = 2, labeller = as_labeller(country_labels)) +
    theme(
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank(),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

plot_country_proj

ggsave(
    filename = "viszs/pop_proj_ageing.png",
    plot = plot_country_proj,
    width = 32, height = 18, units = "cm"
)
