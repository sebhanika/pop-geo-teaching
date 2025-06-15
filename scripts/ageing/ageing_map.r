# Title: spatial_pop_age
# Date: 2024-02-06
# Purpose: Map Ageing
# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(eurostat)
library(sf)
library(ggspatial)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Download data -----------------------------------------------------------

# nut2 geodata
nuts3 <-
    get_eurostat_geospatial(
        output_class = "sf",
        resolution = "03",
        nuts_level = "3",
        crs = 3857,
        year = "2021",
        make_valid = TRUE
    ) %>%
    janitor::clean_names() %>%
    filter(levl_code == 3) %>%
    subset(!grepl("^FRY|^FR$", nuts_id)) %>% # Exclude Oversee territories
    subset(cntr_code != "TR") %>% # Exclude Turkey territories
    select(c(cntr_code, name_latn, geo, geometry)) %>%
    st_transform(3035)

age_dat <- get_eurostat("demo_r_pjanind3", time_format = "num") %>%
    filter(indic_de == "MEDAGEPOP", TIME_PERIOD == 2023)

# map settings

# creating custom color palette
self_palette <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# bounding box
xlim <- c(2426378.0132, 6593974.6215)
ylim <- c(1328101.2618, 5446513.5222)

# create bins for chrolopeth map
dat_map <- nuts3 %>% left_join(age_dat, by = c("geo"))

data_bins <- BAMMtools::getJenksBreaks(dat_map$values, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

med_age <- dat_map %>%
    mutate(val_int = cut(values,
        breaks = data_bins,
        labels = data_labs,
        include.lowest = TRUE
    ))

# create map object
age_map <- med_age %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_fill_manual("Median Age (2023)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: Eurostat (2025)") +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.88, 0.88),
        plot.background = element_blank()
    ) +
    annotation_scale(height = unit(0.15, "cm"))

# save map
ggsave(
    filename = "viszs/age_map.png",
    plot = age_map,
    width = 28, height = 28, units = "cm"
)
