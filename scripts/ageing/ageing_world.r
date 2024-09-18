# Title: ageing_world
# Date: 2024-09-18
# Purpose: Ageing Map Global

# /* cSpell:disable */

# Load libraries --------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(wpp2022)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(countrycode)
library(gganimate)
library(transformr)
library(gifski)

source("scripts/0_config.R")
source("scripts/0_settings.R")


data(popAge1dt)
self_palette <- c(
    "#f7fbff",
    "#deebf7",
    "#c6dbef",
    "#9ecae1",
    "#6baed6",
    "#4292c6",
    "#2171b5",
    "#084594"
)







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

cntries <- unique(map_world$iso3c)


weighted.median <- function(x, w) {
    w <- w[order(x)]
    x <- x[order(x)]

    prob <- cumsum(w) / sum(w, na.rm = TRUE)
    ps <- which(abs(prob - .5) == min(abs(prob - .5)))
    return(x[ps[1]])
}


pop_dat <- popAge1dt |>
    as_tibble() |>
    select(-c(popM, popF)) |>
    mutate(iso3c = countrycode(name, "country.name", "iso3c")) |>
    group_by(iso3c, year) |>
    mutate(median_age = weighted.median(age, pop)) |>
    filter(age == 100)


median_age_sf <- map_world %>%
    left_join(pop_dat, by = "iso3c")


# create bins for chrolopeth map
data_bins_gif <- BAMMtools::getJenksBreaks(pop_dat$median_age, k = 9)

data_labs_gif <- paste(
    round(data_bins_gif[-length(data_bins_gif)], 2),
    " - ",
    round(data_bins_gif[-1], 2)
)


median_age_sf2 <- median_age_sf %>%
    mutate(val_int = cut(median_age,
        breaks = data_bins_gif, ,
        labels = data_labs_gif,
        include.lowest = TRUE
    ))


yrs <- 1960:2021
map_list <- list()

for (i in yrs) {
    map_list[[as.character(i)]] <- median_age_sf2 %>%
        filter(year == i) %>%
        ggplot() +
        geom_sf(aes(fill = as.factor(val_int)),
            linewidth = 0.1, alpha = 1
        ) +
        scale_fill_manual("Median Age",
            values = self_palette,
            breaks = data_labs_gif,
            na.value = "#a7a7a7", drop = FALSE
        ) +
        labs(
            title = paste0("Median Age in: ", i),
            caption = "Source: World Population Prospects (2023)"
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
                    "viszs/med_age_map/med_age_map_",
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

png_files <- list.files("viszs/med_age_map/", pattern = ".*png$", full.names = TRUE)
gifski(png_files,
    gif_file = "viszs/med_age_map.gif",
    width = 1209, height = 680, delay = 0.25
)
