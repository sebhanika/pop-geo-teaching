# Title: Population pyramids
# Date: 2023-10-10
# Purpose: Script Purpose

# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(gganimate)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Age pyramid data  --------------

swe_pop_pyr <- readHMDweb(
    CNTRY = "SWE",
    "Population",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(c(Year, Age, Female2, Male2)) %>%
    rename(
        male = Male2,
        female = Female2
    ) %>%
    pivot_longer(
        cols = c(male, female),
        values_to = "pop", names_to = "sex"
    ) %>%
    janitor::clean_names() %>%
    mutate(sex = factor(sex, levels = c("male", "female")))


# Static graph/four years --------------

##### old pyramid code

# Settings for labeling and filter for plot
years_p <- c(1872, 1922, 1972, 2022)
max_pop <- max(swe_pop_pyr$pop)

# get first two digits of rounded max pop. expressed in thousands
max_pop_lim <- round(max_pop, -4)
max_pop_label <- as.numeric(substr(max_pop_lim, 1, 2))

# get breaks for axis, needs negative values
pop_brks <- seq(-max_pop_lim, max_pop_lim, max_pop_lim / 2)
# get nice labels
pop_labels <- abs(seq(-max_pop_label, max_pop_label, max_pop_label / 2))


# age pyramid plot
age_pyrs <- swe_pop_pyr %>%
    filter(year %in% years_p) %>%
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        fill = sex
    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
        limits = c(-max(swe_pop_pyr$pop), max(swe_pop_pyr$pop)),
        breaks = pop_brks,
        labels = pop_labels
    ) +
    scale_fill_manual(
        values = park_palette("ArcticGates", 2),
        labels = c("Male", "Female")
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand",
        caption = "Source: Human Mortality Database"
    ) +
    facet_wrap(~year) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    )
age_pyrs

# save plot
ggsave(
    filename = "graphs/age_pyr_swe.png",
    plot = age_pyrs,
    width = 25, height = 25, units = "cm"
)


# GGanimate --------------

# create plot for animation
pyr_anim <- swe_pop_pyr %>%
    filter(year %in% 1872:2022) %>%
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        pop, fill = sex
    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
        limits = c(-max(swe_pop_pyr$pop), max(swe_pop_pyr$pop)),
        breaks = pop_brks,
        labels = pop_labels
    ) +
    scale_fill_manual(
        values = c("#cfb470", "#678096"),
        labels = c("Male", "Female")
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand",
        caption = "Source: Human Mortality Database"
    ) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    )


# create animation
age_pyr_gif <- pyr_anim +
    transition_time(year) +
    labs(title = "Year: {frame_time}")

anim_save("graphs/age_pyr_animated.gif", age_pyr_gif,
    fps = 7,
    renderer = gifski_renderer(loop = TRUE),
    height = 18, width = 32, units = "cm", res = 150
)
