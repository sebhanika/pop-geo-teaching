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
    )


# World Data --------------

wpp_2022_p <-
    comb_dat |>
    filter(name == "World") |>
    mutate(value = value / 1000000) |> # turn data into billion
    ggplot(aes(
        x = year,
        y = value,
        color = categ,
        linetype = categ
    )) +
    scale_linetype_manual(values = c(
        2, 3, 4, 1, 1, 4, 3, 2
    )) +
    scale_color_manual(values = c(
        "#b2182b", "#ef8a62",
        "#fddbc7",
        "#6a6a6a", "black",
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
        title = "Population Projection Scenarios - World",
        caption = "Source: World Population Prospects (2022)"
    ) +
    theme(
        legend.title = element_blank(),
        legend.key.size = unit(2, "line")
    )

wpp_2022_p

ggsave("viszs/pop_proj2022.png", plot = wpp_2022_p, width = 32, height = 18, units = "cm")

# Regional

regio_wpp_2022_p <-
    comb_dat |>
    mutate(
        name = ifelse(grepl("Oceania", name), "Oceania", name),
        value = value / 1000
    ) |>
    filter(name != "World") |>
    ggplot(aes(
        x = year,
        y = value,
        color = categ,
        linetype = categ
    )) +
    scale_linetype_manual(values = c(
        2, 3, 4, 1, 1, 4, 3, 2
    )) +
    scale_color_manual(values = c(
        "#b2182b", "#ef8a62",
        "#fddbc7",
        "#6a6a6a", "black",
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
    facet_wrap(~name,
        scales = "free",
        nrow = 2,
        labeller = label_wrap_gen(30)
    ) +
    labs(
        x = "Year", y = "Population in million",
        title = "Population Projection Scenarios - UN Regions",
        caption = "Source: World Population Prospects (2022)"
    ) +
    theme(
        legend.title = element_blank(),
        legend.position = "right",
        legend.key.size = unit(2, "line"),
        axis.text = element_text(size = 10)
    )

regio_wpp_2022_p

ggsave("viszs/regio_pop_proj2022.png",
    plot = regio_wpp_2022_p,
    width = 40, height = 20, units = "cm"
)
