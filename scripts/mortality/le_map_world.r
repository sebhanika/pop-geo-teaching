# Title: Le Map Word
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

le_dat <- wb_data("SP.DYN.LE00.FE.IN", start_date = 1960, end_date = 2021) %>%
    rename(le_female = SP.DYN.LE00.FE.IN)

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

le_sf2019 <- map_world %>%
    left_join(subset(le_dat, date == "2019"))

data_bins <- BAMMtools::getJenksBreaks(le_sf2019$le_female, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

le_sf2019_2 <- le_sf2019 %>%
    mutate(val_int = cut(le_female,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))

plot_le_sf2 <- le_sf2019_2 %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    scale_fill_manual("Female Life Expectancy",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(
        title = "Female Life Expectancy in 2019",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom"
    )

plot_le_sf2


ggsave(
    filename = "viszs/le_world_map.png",
    plot = plot_le_sf2, width = 32, height = 18, units = "cm"
)


# Animated map --------------

# This code is currently a work around due to a bug
# in the gganimate/transformr package
# however installing previous versions does not work
# on this laptop. W


# making map frames manually and add them with gfiski
le_dat_gif <- map_world %>%
    left_join(le_dat)

# create bins for chrolopeth map
data_bins_gif <- BAMMtools::getJenksBreaks(le_dat_gif$le_female, k = 6)

data_labs_gif <- paste(
    round(data_bins_gif[-length(data_bins_gif)], 2),
    " - ",
    round(data_bins_gif[-1], 2)
)

le_dat_gif2 <- le_dat_gif %>%
    mutate(val_int = cut(le_female,
        breaks = data_bins_gif, ,
        labels = data_labs_gif,
        include.lowest = TRUE
    ))

yrs <- 1960:2021
map_list <- list()

for (i in yrs) {
    map_list[[as.character(i)]] <- le_dat_gif2 %>%
        filter(date == i) %>%
        ggplot() +
        geom_sf(aes(fill = as.factor(val_int)),
            linewidth = 0.1, alpha = 1
        ) +
        scale_fill_manual("Female Life Expectancy",
            values = self_palette,
            breaks = data_labs_gif,
            na.value = "#a7a7a7", drop = FALSE
        ) +
        labs(
            title = paste0("Female Life Expectancy in: ", i),
            caption = "Source: World Bank (2023)"
        ) +
        theme(
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = "bottom"
        ) +
        guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}


# save all plots in list as pngs
invisible(
    lapply(
        seq_along(map_list),
        function(x) {
            ggsave(
                filename = paste0(
                    "viszs/le_map/le_map_",
                    names(map_list[x]), ".png"
                ),
                plot = map_list[[x]],
                bg = "white",
                width = 32,
                height = 18,
                unit = "cm"
            )
        }
    )
)

png_files <- list.files("viszs/le_map/", pattern = ".*png$", full.names = TRUE)
gifski(png_files,
    gif_file = "viszs/le_world_map.gif",
    width = 1209, height = 680, delay = 0.25
)
