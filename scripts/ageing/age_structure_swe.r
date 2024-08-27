# Title: age_structure_swe
# Date: 2024-08-21
# Purpose: Animated population pyramid sweden

# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(gganimate)

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

# Settings for labeling and filter for plot
max_pop <- max(swe_pop_pyr$pop)

# get first two digits of rounded max pop. expressed in thousands
max_pop_lim <- round(max_pop, -4)
max_pop_label <- as.numeric(substr(max_pop_lim, 1, 2))

# get breaks for axis, needs negative values
pop_brks <- seq(-max_pop_lim, max_pop_lim, max_pop_lim / 2)
# get nice labels
pop_labels <- abs(seq(-max_pop_label, max_pop_label, max_pop_label / 2))

# GGanimate --------------

# create plot for animation
pyr_anim <- swe_pop_pyr %>%
    filter(year %in% 1872:2022) %>%
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        pop,
        fill = sex
    )) +
    geom_bar(stat = "identity", width = 1, alpha = 0.8) +
    scale_y_continuous(
        limits = c(-max(swe_pop_pyr$pop), max(swe_pop_pyr$pop)),
        breaks = pop_brks,
        labels = pop_labels
    ) +
    scale_fill_manual(
        values = c("#8392b6", "#c3a088"),
        labels = c("Male", "Female")
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand",
        caption = "Source: Human Mortality Database (2024)"
    ) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    )

# create animation
age_pyr_gif <- pyr_anim +
    transition_time(year) +
    labs(title = "Age Structure in Sweden in: {frame_time}")

anim_save("viszs/age_pyr_swe_animated.gif", age_pyr_gif,
    fps = 6,
    renderer = gifski_renderer(loop = TRUE),
    height = 18, width = 32, units = "cm", res = 150
)
