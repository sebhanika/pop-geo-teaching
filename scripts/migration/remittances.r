# Title: Remittances
# Date: 2024-01-19
# Purpose: Remittances received, share of pop
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

rem_dat <- wb_data("BX.TRF.PWKR.DT.GD.ZS",
    start_date = 2000, end_date = 2021
) %>%
    rename(rem_stock = BX.TRF.PWKR.DT.GD.ZS)

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

# Map 2019 --------------

rem_sf2019 <- map_world %>%
    left_join(subset(rem_dat, date == "2019"))

data_bins <- BAMMtools::getJenksBreaks(rem_sf2019$rem_stock, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

rem_sf2019_2 <- rem_sf2019 %>%
    mutate(val_int = cut(rem_stock,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))

plot_rem_stock <- rem_sf2019_2 %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    scale_fill_manual("Remittances in % of GDP",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(
        title = "Personal remittances, received in 2019",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
    )

plot_rem_stock


ggsave(
    filename = "viszs/rem_stock_map.png",
    plot = plot_rem_stock, width = 32, height = 18, units = "cm"
)
