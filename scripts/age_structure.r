# Title: Population pyramids
# Date: 2023-10-10
# Purpose: Script Purpose

# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(gganimate)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# load data --------------


# death rate
swe_deaths <- readHMDweb(
    CNTRY = "SWE",
    "Deaths_1x1",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(-c(Female, Male)) %>%
    rename(deaths = Total) %>%
    group_by(Year) %>%
    summarise(across(
        .cols = c(deaths),
        .fns = ~ sum(.x, na.rm = TRUE)
    )) %>%
    ungroup()



swe_pop <- readHMDweb(
    CNTRY = "SWE",
    "Population",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(c(Year, Age, Total2)) %>%
    rename(pop = Total2) %>%
    group_by(Year) %>%
    summarise(across(
        .cols = c(pop),
        .fns = ~ sum(.x, na.rm = TRUE)
    )) %>%
    ungroup()


# crude birth rate
swe_births <- readHMDweb(
    CNTRY = "SWE",
    "Births",
    username = hfd_username,
    password = hfd_password,
    fixup = TRUE
) %>%
    select(-c(Female, Male)) %>%
    rename(births = Total)


swe_dat <- swe_deaths %>%
    left_join(swe_births, by = c("Year")) %>%
    left_join(swe_pop, by = c("Year")) %>%
    mutate(
        cbr = births / (pop / 1000),
        cdr = deaths / (pop / 1000)
    )


plot(swe_dat$Year, swe_dat$cdr, type = "l")
lines(swe_dat$Year, swe_dat$cbr, lty = 2)





# Age pyramid data  --------------

swe_pop <- readHMDweb(
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

head(swe_pop)




# Four years --------------

##### old pyramid code

try_lab <- swe_pop %>%
    filter(year %in% c(1872, 1922, 1972, 2022)) %>%
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        pop, fill = sex
    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
        limits = c(-83000, 83000),
        breaks = seq(-80000, 80000, 40000),
        labels = c(80, 40, 0, 40, 80)
    ) + # manual labeling
    scale_fill_manual(
        values = c("female" = "#01665E", "male" = "#DABF7F"),
        labels = c("Male", "Female"),
        name = "Sex"
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand"
    ) +
    theme_bw() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10, color = "grey20"),
        strip.text.x = element_text(size = 15)
    ) +
    facet_wrap(~year)
try_lab




# GGanimate --------------

plot_pyr <- swe_pop %>%
    filter(year %in% 1850:2022) %>%
    ggplot(aes(
        x = age,
        y = ifelse(sex == "male", -pop, pop),
        pop, fill = sex
    )) +
    geom_bar(stat = "identity") +
    scale_y_continuous(
        limits = c(-83000, 83000),
        breaks = seq(-80000, 80000, 40000),
        labels = c(80, 40, 0, 40, 80)
    ) + # manual labeling
    scale_fill_manual(
        values = c("female" = "#01665E", "male" = "#DABF7F"),
        labels = c("Male", "Female"),
        name = "Sex"
    ) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Population in Thousand"
    ) +
    theme_bw() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 10, color = "grey20")
    ) +
    theme(strip.text.x = element_text(size = 15))

plot_pyr

gif <- plot_pyr +
    transition_time(year) +
    labs(title = "Year: {frame_time}")


anim_save("try2.gif", gif,
    fps = 7,
    renderer = gifski_renderer(loop = TRUE),
    height = 32, width = 18, units = "cm", res = 150
)
