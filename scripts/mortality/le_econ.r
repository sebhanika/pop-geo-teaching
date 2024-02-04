# Title: Le_econ
# Date: 2024-01-19
# Purpose: Life expectancy and GDP per capita
# /* cSpell:disable */

# Libraries --------------
library(dplyr)
library(tidyr)
library(wbstats)
library(ggplot2)
library(countrycode)
library(gganimate)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------

le_dat <- wb_data("SP.DYN.LE00.FE.IN", start_date = 1960, end_date = 2021) %>%
    rename(le_female = SP.DYN.LE00.FE.IN) %>%
    select(c(le_female, date, iso3c))


gdp_dat <- wb_data("NY.GDP.PCAP.CD", start_date = 1960, end_date = 2021) %>%
    rename(gdp_cap = "NY.GDP.PCAP.CD") %>%
    select(c(gdp_cap, date, iso3c, country)) %>%
    mutate(region = countrycode(
        sourcevar = country,
        origin = "country.name",
        destination = "region"
    ))

pop_dat <- wb_data("SP.POP.TOTL", start_date = 1960, end_date = 2021) %>%
    rename(pop = "SP.POP.TOTL") %>%
    select(c(pop, date, iso3c))


dat_comb <- le_dat %>%
    full_join(gdp_dat) %>%
    left_join(pop_dat)


plot_le_econ <-
    dat_comb %>%
    filter(date == 2021) %>%
    ggplot(aes(x = gdp_cap, y = le_female, color = region, size = pop)) +
    geom_point(alpha = 0.7) +
    scale_color_manual("Region",
        values = park_palette("CraterLake")
    ) +
    scale_size("Population size in 100 mio",
        range = c(2, 20), labels = c(0, 500, 1000, 0)
    ) +
    labs(
        x = "GDP per capita in Current US$",
        y = "Female Life expectancy in years",
        caption = "Source: World Bank (2023)",
        title = "GDP per Captia and Female Life Expectancy in 2021"
    ) +
    theme(
        legend.position = c(0.87, 0.3),
        legend.text = element_text(size = 16)
    ) +
    guides(
        colour = guide_legend(override.aes = list(size = 8)),
        alpha = "none",
    )

plot_le_econ



# save plot for presentation
ggsave(
    filename = "viszs/le_econ.png",
    plot = plot_le_econ,
    width = 42, height = 24, units = "cm"
)



# Animated graph --------------

plot_anim <- dat_comb %>%
    mutate(date = as.integer(date)) %>%
    ggplot(aes(x = gdp_cap, y = le_female, color = region, size = pop)) +
    geom_point(alpha = 0.7) +
    scale_color_manual("Region",
        values = park_palette("CraterLake")
    ) +
    scale_size("Population size in 100 mio",
        range = c(2, 20), labels = c(0, 500, 1000, 0)
    ) +
    labs(
        x = "GDP per capita in Current US$",
        y = "Female Life expectancy in years",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        legend.position = c(0.87, 0.3),
        legend.text = element_text(size = 16)
    ) +
    guides(
        colour = guide_legend(override.aes = list(size = 8)),
        alpha = "none",
    )

gif_le_econ <- plot_anim +
    transition_time(date) +
    labs(title = "GDP per Captia and Female Life Expectancy in: {frame_time}") +
    shadow_mark(alpha = 0.3, size = 0.5)

anim_save("viszs/le_econ.gif", gif_le_econ,
    fps = 7,
    renderer = gifski_renderer(loop = TRUE),
    width = 42, height = 24, units = "cm", res = 300
)
