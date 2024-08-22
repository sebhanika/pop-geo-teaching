# Title: pop_projections_UN
# Date: 2024-08-21
# Purpose: Population Projections UN, based on WPP2022

# /* cSpell:disable */
# Load libraries --------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(wpp2022)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Data prepping --------------
data(pop1dt)
data(popproj1dt)

un_regions <- c(
    "World", "Sub-Saharan Africa",
    "Northern Africa and Western Asia",
    "Central and Southern Asia",
    "Eastern and South-Eastern Asia",
    "Latin America and the Caribbean",
    "Oceania (excluding Australia and New Zealand)",
    "Australia/New Zealand",
    "Europe and Northern America"
)

# Historical data
pop_hist <- pop1dt |>
    as_tibble() |>
    select(-c(popM, popF)) |>
    filter(name %in% un_regions) |>
    pivot_longer(cols = starts_with("pop"), names_to = "categ") |>
    mutate(categ = ifelse(categ == "pop", "Observed", NA))

# Projection data
pop_proj <- popproj1dt |>
    as_tibble() |>
    select(-c(popM, popF)) |>
    filter(name %in% un_regions) |>
    pivot_longer(cols = starts_with("pop"), names_to = "categ")


# Combine dat
comb_dat <- bind_rows(pop_hist, pop_proj) |>
    mutate(categ = case_when(
        categ == "pop_low" ~ "Low Fertility",
        categ == "pop_high" ~ "High Fertility",
        categ == "pop" ~ "Median",
        categ == "pop_80l" ~ "Lower 80%",
        categ == "pop_80u" ~ "Upper 80%",
        categ == "pop_95l" ~ "Lower 95%",
        categ == "pop_95u" ~ "Upper 95%",
        TRUE ~ categ
    )) |>
    mutate(
        year = as.numeric(year),
        categ = factor(categ,
            levels = c(
                "High Fertility",
                "Upper 95%",
                "Upper 80%",
                "Observed",
                "Median",
                "Lower 80%",
                "Lower 95%",
                "Low Fertility"
            )
        )
    ) |>
    mutate(value = value / 1000000)


# World Data --------------

wpp_2022_p <-
    comb_dat |>
    filter(name == "World") |>
    ggplot(aes(
        x = year,
        y = value,
        color = categ
    )) +
    # scale_linetype_manual(values = c(
    #     "solid", "solid",
    #     "dotted", "dashed",
    #     "dotted", "dashed",
    #     "dotted", "dashed"
    # )) +
    scale_color_manual(values = c(
        "#b2182b", "#ef8a62",
        "#fddbc7",
        "black", "black",
        "#d1e5f0",
        "#67a9cf", "#2166ac"
    )) +
    geom_line(linewidth = 1.15) +
    geom_vline(
        xintercept = 2022,
        alpha = 0.6,
        color = "darkred",
        linetype = 2
    ) +
    labs(
        x = "Year", y = "Population in billion",
        title = "Population Projection Scenarios",
        subtitle = "based on World Population Prospects 2022"
    ) +
    theme(legend.title = element_blank())

wpp_2022_p
