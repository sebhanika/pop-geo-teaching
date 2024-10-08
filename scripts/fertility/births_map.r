# Title: births_map
# Date: 2024-01-16
# Purpose: Map of births
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
    select(c(cntr_code, name_latn, geo, geometry)) %>%
    rename(nuts2_name = name_latn) %>%
    st_transform(3035)


pop <- get_eurostat("demo_r_d2jan", time_format = "num") %>%
    filter(
        TIME_PERIOD == 2022,
        nchar(geo) == 4,
        age == "TOTAL",
        sex == "T"
    ) %>%
    mutate(sex = ifelse(sex == "T", "pop", "no")) %>%
    pivot_wider(names_from = sex, values_from = values) %>%
    select(c(geo, pop))

dat_births <- get_eurostat("demo_r_fagec", time_format = "num") %>%
    filter(
        TIME_PERIOD == 2022,
        nchar(geo) == 4,
        age == "TOTAL"
    ) %>%
    select(-c(freq, unit, age, TIME_PERIOD)) %>%
    rename(births = values)


dat_comb <- nuts2_v1 %>%
    left_join(dat_births) %>%
    left_join(pop) %>%
    mutate(cbr = births / pop * 1000)


# Create Map --------------

# creating custom color palette
self_palette <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# bounding box
xlim <- c(2426378.0132, 6593974.6215)
ylim <- c(1328101.2618, 5446513.5222)

# create bins for chrolopeth map

data_bins <- BAMMtools::getJenksBreaks(dat_comb$cbr, k = 6)

data_labs <- paste(
    round(data_bins[-length(data_bins)], 2),
    " - ",
    round(data_bins[-1], 2)
)

births_sf <- dat_comb %>%
    mutate(val_int = cut(cbr,
        breaks = data_bins, ,
        labels = data_labs,
        include.lowest = TRUE
    ))

# create map object
p_cbr_map <- births_sf %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_fill_manual("CBR (2022)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: Eurostat (2024)") +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.88, 0.88)
    ) +
    annotation_scale(height = unit(0.15, "cm"))

p_cbr_map


ggsave(
    filename = "viszs/cbr_maps_NUTS2.png",
    plot = p_cbr_map, width = 28, height = 28, units = "cm"
)
