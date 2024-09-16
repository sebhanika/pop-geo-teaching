# Title: TFR Europe
# Date: 2024-01-24
# Purpose: TFR at NUTS2
# /* cSpell:disable */

# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(eurostat)
library(sf)
library(ggspatial)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------
nuts2_v1 <-
    get_eurostat_geospatial(
        output_class = "sf",
        resolution = "03",
        nuts_level = "2",
        crs = 3857,
        year = "2021",
        make_valid = TRUE
    ) %>%
    janitor::clean_names() %>%
    filter(!grepl("^FRY|^FR$", nuts_id)) %>% # rm colonies
    rename(nuts2_name = name_latn) %>%
    st_transform(3035)



dat_tfr <- get_eurostat("tgs00100", time_format = "num") %>%
    filter(
        nchar(geo) == 4,
        TIME_PERIOD == 2022
    ) %>%
    select(c(geo, values, TIME_PERIOD)) %>%
    rename(tfr = values)


dat_comb <- nuts2_v1 %>%
    left_join(dat_tfr)


# Create Map --------------

# creating custom color palette
self_palette <- c(
    "#eff3ff", "#bdd7e7", "#6baed6",
    "#3182bd", "#08519c", "#022a4d"
)

# bounding box
xlim <- c(2426378.0132, 7353974.6215)
ylim <- c(1328101.2618, 5446513.5222)

data_bins <- BAMMtools::getJenksBreaks(dat_tfr$tfr, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

tfr_sf <- dat_comb %>%
    mutate(val_int = cut(tfr,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))


# create map object
p_tfr_map <- tfr_sf %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_fill_manual("Total Fertility Rate (2022)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: Eurostat (2024)") +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.86, 0.88)
    ) +
    annotation_scale(height = unit(0.15, "cm"))


p_tfr_map

ggsave(
    filename = "viszs/tfr_maps_NUTS21.png",
    plot = p_tfr_map, width = 28, height = 28, units = "cm"
)
