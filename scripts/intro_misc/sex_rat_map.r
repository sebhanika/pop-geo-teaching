# Title: sex_rat_nap
# Date: 2024-02-06
# Purpose: World map of Sex Ratios
# /* cSpell:disable */

# Libraries --------------
library(dplyr)
library(tidyr)
library(wbstats)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(countrycode)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# palette for maps
self_palette <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")



# Load data --------------

pop_f_dat <- wb_data("SP.POP.TOTL.FE.IN", start_date = 2021, end_date = 2021) %>%
    rename(f_pop = SP.POP.TOTL.FE.IN) %>%
    select(c(iso3c, f_pop))

pop_m_dat <- wb_data("SP.POP.TOTL.MA.IN", start_date = 2021, end_date = 2021) %>%
    rename(m_pop = SP.POP.TOTL.MA.IN) %>%
    select(c(iso3c, m_pop))

pop_dat <- pop_f_dat |>
    left_join(pop_m_dat) |>
    mutate(sex_rat = m_pop / f_pop)


map_world <- ne_countries(
    scale = "medium",
    type = "countries",
    returnclass = "sf"
) %>%
    mutate(iso3c = countrycode(
        name,
        origin = "country.name",
        destination = "iso3c"
    )) %>%
    select(c(iso3c, name, geometry)) %>%
    filter(name != "Antarctica")

# Map 2021 --------------

pop_sf <- map_world %>%
    left_join(pop_dat)


data_bins <- BAMMtools::getJenksBreaks(pop_sf$sex_rat, k = 6)

data_bins <- c(0.81, 0.92, 0.99, 1.01, 1.16, 2.66)


data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)


pop_sf_2 <- pop_sf %>%
    mutate(val_int = cut(sex_rat,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))

self_palette <- c("#a6611a", "#d8b365", "#f9f9f9", "#80cdc1", "#018571")


plot_pop_sf_2 <- pop_sf_2 %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    scale_fill_manual("Sex ratio\n(Male/Female)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(
        title = "Sex Ratio by country in 2021",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
    )

plot_pop_sf_2

ggsave(
    filename = "viszs/sex_rat_map.png",
    plot = plot_pop_sf_2, width = 32, height = 18, units = "cm"
)
