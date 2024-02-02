# Title: Map_Migration_Europe
# Date: 2024-02-02
# Purpose: Net migration rate Europe
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



dat_mig <- get_eurostat("tgs00099", time_format = "num") %>%
    filter(
        nchar(geo) == 4,
        TIME_PERIOD == 2019,
        indic_de == "CNMIGRATRT"
    ) %>%
    select(c(geo, values, TIME_PERIOD)) %>%
    rename(mig_rate = values)


dat_comb <- nuts2_v1 %>%
    left_join(dat_mig)


# Create Map --------------

# creating custom color palette
self_palette <- c(
    "#ca0020", "#f4a582", "#f7f7f7",
    "#92c5de", "#0571b0"
)

# bounding box
xlim <- c(2426378.0132, 6593974.6215)
ylim <- c(1328101.2618, 5446513.5222)

data_bins <- BAMMtools::getJenksBreaks(dat_mig$mig_rate, k = 6)
data_bins_clean <- c(-32.8, -9.1, -1, 1, 8.1, 40.4)

data_labs <- paste(
    round(data_bins_clean[-length(data_bins_clean)], 2),
    " - ",
    round(data_bins_clean[-1], 2)
)

mig_rate_sf <- dat_comb %>%
    mutate(val_int = cut(mig_rate,
        breaks = data_bins_clean, ,
        labels = data_labs,
        include.lowest = TRUE
    ))


# create map object
p_mig_map <- mig_rate_sf %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_fill_manual("Net Migration Rate (2019)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: Eurostat (2023)") +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.86, 0.88)
    ) +
    annotation_scale(height = unit(0.15, "cm"))


p_mig_map


ggsave(
    filename = "viszs/mig_map_NUTS2.png",
    plot = p_mig_map, width = 28, height = 28, units = "cm"
)
