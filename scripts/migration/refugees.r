# Title: Refugees
# Date: 2024-02-06
# Purpose: GLobal refugee populations
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
library(gganimate)
library(transformr)
library(gifski)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# palette for maps
self_palette <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")


# Load data --------------

refg_dat <- wb_data("SM.POP.REFG", start_date = 2021, end_date = 2021) %>%
    rename(refg_stock = SM.POP.REFG) %>%
    select(c(iso3c, refg_stock))

pop_dat <- wb_data("SP.POP.TOTL", start_date = 2021, end_date = 2021) %>%
    rename(pop = SP.POP.TOTL) %>%
    select(c(iso3c, pop))

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

refg_sf2021 <- map_world %>%
    left_join(refg_dat) %>%
    left_join(pop_dat) %>%
    mutate(refg_pop_rel = (refg_stock / pop) * 100)

data_bins <- BAMMtools::getJenksBreaks(refg_sf2021$refg_pop_rel, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

refg_sf2021_2 <- refg_sf2021 %>%
    mutate(val_int = cut(refg_pop_rel,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))

plot_refg_stock <- refg_sf2021_2 %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    scale_fill_manual("Refugees in % of population ",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(
        title = "Refugee population by territory of asylum in 2021",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
    )

plot_refg_stock


ggsave(
    filename = "viszs/refg_stock_map.png",
    plot = plot_refg_stock, width = 32, height = 18, units = "cm"
)
