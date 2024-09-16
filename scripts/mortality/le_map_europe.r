# Title: Life Expectancy
# Date: 2024-01-04
# Purpose: Life expectancy for selected countries
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



le <- get_eurostat("demo_r_mlifexp", time_format = "num") %>%
    filter(
        age == "Y_LT1",
        nchar(geo) == 4,
        TIME_PERIOD == 2022
    ) %>%
    pivot_wider(
        names_from = sex,
        names_prefix = "le_",
        values_from = values
    ) %>%
    select(c(geo, le_T, TIME_PERIOD))


dat_comb <- nuts2_v1 %>%
    left_join(le)


# Create Map --------------

# creating custom color palette
self_palette <- c(
    "#eff3ff", "#bdd7e7", "#6baed6",
    "#3182bd", "#08519c", "#022a4d"
)

# bounding box
xlim <- c(2426378.0132, 6593974.6215)
ylim <- c(1328101.2618, 5446513.5222)

# create bins for chrolopeth map
data_bins <- c(69.2, 75, 80, 81, 82, 83, 85.7)

le_map <- dat_comb %>%
    mutate(val_int = cut(le_T,
        breaks = data_bins,
        labels = c(
            "< 75", "75 - < 80",
            "80 - < 81", "81 - < 82",
            "82 - < 83", " ≥ 83"
        ),
        include.lowest = TRUE,
        right = FALSE
    ))

# create map object
p_le_map <- le_map %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_fill_manual("Total Life Expectancy\n(2022)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: Eurostat (2024)") +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.86, 0.86)
    ) +
    annotation_scale(height = unit(0.15, "cm"))


p_le_map


ggsave(
    filename = "viszs/le_maps_NUTS2.png",
    plot = p_le_map, width = 25, height = 25, units = "cm"
)
