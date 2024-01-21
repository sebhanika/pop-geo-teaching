# Title: Le Map Word
# Date: 2024-01-19
# Purpose: Life expectancy map world
# /* cSpell:disable */

library(dplyr)
library(tidyr)
library(wbstats)
library(cshapes)
library(sf)
library(ggplot2)
library(countrycode)
library(ggspatial)


source("scripts/0_config.R")
source("scripts/0_settings.R")

le_dat <- wb_data("SP.DYN.LE00.FE.IN", start_date = 2021, end_date = 2021)

le_world <- cshp(date = as.Date("2019-01-01")) %>%
    mutate(iso3c = countrycode(gwcode,
        origin = "gwn",
        destination = "iso3c"
    )) %>%
    st_transform(le_world, "ESRI:54042")

le_sf <- le_world %>%
    left_join(le_dat) %>%
    rename(le_female = SP.DYN.LE00.FE.IN)


# creating custom color palette
self_palette <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")


# create bins for chrolopeth map
data_bins <- BAMMtools::getJenksBreaks(le_sf$le_female, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

le_sf2 <- le_sf %>%
    mutate(val_int = cut(le_female,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))





plot_le_sf2 <- le_sf2 %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    scale_fill_manual("Female Life Expectancy",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: World Bank (2023)") +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.2, 0.3)
    ) +
    annotation_scale(height = unit(0.15, "cm"))
plot_le_sf2
