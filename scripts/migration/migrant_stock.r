# Title: Migrant stock
# Date: 2024-01-19
# Purpose: Life expectancy map world
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

mig_dat <- wb_data("SM.POP.TOTL.ZS", start_date = 1960, end_date = 2021) %>%
    rename(mig_stock = SM.POP.TOTL.ZS)

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

# Map 2014 --------------

mig_sf2019 <- map_world %>%
    left_join(subset(mig_dat, date == "2015"))

data_bins <- BAMMtools::getJenksBreaks(mig_sf2019$mig_stock, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

mig_sf2019_2 <- mig_sf2019 %>%
    mutate(val_int = cut(mig_stock,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))

plot_mig_stock <- mig_sf2019_2 %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    scale_fill_manual("Migrant Stock in %",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(
        title = "International Migrant Stock in 2015",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
    )

plot_mig_stock


ggsave(
    filename = "viszs/mig_stock_map.png",
    plot = plot_mig_stock, width = 32, height = 18, units = "cm"
)
