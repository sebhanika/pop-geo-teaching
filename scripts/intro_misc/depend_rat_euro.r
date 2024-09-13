# Title: depend_rat_euro
# Date: 2024-09-06
# Purpose: Dependecy Ratio in Europe

# /* cSpell:disable */
# Load libraries --------------

library(dplyr)
library(ggplot2)
library(countrycode)
library(eurostat)
library(tidyr)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Load data --------------
dat_raw <- get_eurostat("demo_pjanbroad", time_format = "num") |>
    janitor::clean_names()


str(dat_raw)

dat_plot <- dat_raw |>
    mutate(cntry = countrycode(
        sourcevar = geo,
        origin = "eurostat",
        destination = "country.name"
    )) |>
    filter(
        age %in% c("TOTAL", "Y_GE65"),
        sex == "T",
        cntry %in% c("Sweden", "Italy", "Czechia")
    ) |>
    pivot_wider(id_cols = c(cntry, time_period), values_from = values, names_from = age) |>
    mutate(rat65 = Y_GE65 / TOTAL)

plot_rat65 <-
    dat_plot |>
    ggplot((aes(
        x = time_period, y = rat65,
        color = cntry,
        linetype = cntry
    ))) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = c("lightblue", "#9C6114", "#000080")) +
    scale_linetype_manual(values = c(1, 2, 3)) +
    labs(
        title = "Trends in old-age dependency ratios in Europe",
        x = "Year",
        y = "Old-age depedency ratio (65+)",
        caption = "Source: Eurostat (2024)"
    ) +
    theme(
        legend.position = "inside",
        legend.title = element_blank(),
        legend.position.inside = c(0.094, 0.90),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.key.width = unit(2, "cm")
    )
plot_rat65

ggsave(
    filename = "viszs/plot_rat65_europe.png",
    plot = plot_rat65,
    width = 32,
    height = 18,
    unit = "cm",
    dpi = 400
)
