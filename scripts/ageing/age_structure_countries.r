# Title: age_structure_countries
# Date: 2024-08-23
# Purpose: Age pyramids countries

# /* cSpell:disable */

# Load libraries --------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(wpp2022)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Data prepping --------------
data(popAge5dt)

countries <- c(
    "Japan", "Sweden", "Italy", "China, Hong Kong SAR",
    "Mexico", "United Arab Emirates", "Sint Maarten (Dutch part)",
    "Qatar", "Benin", "United States of America", "China",
    "Russian Federation", "Romania", "Monaco"
)

age_order <- unique(popAge5dt$age)


pop_dat <- popAge5dt |>
    as_tibble() |>
    filter(year == 2020, name %in% countries) |>
    group_by(name) |>
    mutate(total_pop = sum(pop)) |>
    ungroup() |>
    mutate(male = popM / total_pop, female = popF / total_pop) |>
    select(-c(country_code, popM, popF, pop, total_pop)) |>
    pivot_longer(
        cols = c(male, female),
        names_to = "sex",
        values_to = "pop"
    ) |>
    mutate(age = factor(age, levels = age_order))

unique(popAge5dt$name)

###

# normal pyramids

p_normal_pyrs <- pop_dat %>%
    filter(name %in% c("Benin", "Mexico", "Sweden", "Japan")) |>
    mutate(name = factor(name, levels = c("Benin", "Mexico", "Sweden", "Japan"))) |>
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        pop,
        fill = sex
    )) +
    geom_bar(stat = "identity", alpha = 0.8) +
    scale_y_continuous(
        limits = c(-0.085, 0.085),
        breaks = c(-0.05, 0, 0.05),
        labels = c(0.05, 0, 0.05)
    ) +
    scale_fill_manual(
        values = c("#8392b6", "#c3a088"),
        guide = guide_legend(reverse = TRUE),
        labels = c("Female", "Male")
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand",
        caption = "Source: World Population Prospects (2022)",
        title = ""
    ) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    facet_wrap(~name, nrow = 1)

p_normal_pyrs

ggsave("viszs/p_normal_pyrs.png",
    plot = p_normal_pyrs, width = 32, height = 18, units = "cm"
)


# do crazy pyramids




p_crazy_pyrs <- pop_dat %>%
    filter(name %in% c("Monaco", "United Arab Emirates", "Sint Maarten (Dutch part)")) |>
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        pop,
        fill = sex
    )) +
    geom_bar(stat = "identity", alpha = 0.8) +
    scale_y_continuous(
        limits = c(-0.13, 0.13),
        breaks = c(-0.1, 0, 0.1),
        labels = c(0.1, 0, 0.1)
    ) +
    scale_fill_manual(
        values = c("#8392b6", "#c3a088"),
        guide = guide_legend(reverse = TRUE),
        labels = c("Female", "Male")
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand",
        caption = "Source: World Population Prospects (2022)",
        title = ""
    ) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    facet_wrap(~name, nrow = 1)

p_crazy_pyrs


ggsave("viszs/p_crazy_pyrs.png",
    plot = p_crazy_pyrs, width = 32, height = 18, units = "cm"
)
