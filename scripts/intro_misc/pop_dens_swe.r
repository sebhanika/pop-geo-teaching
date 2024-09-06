# Title: pop_dens_swe
# Date: 2024-09-04
# Purpose: Population density map Sweden

# /* cSpell:disable */
# Load libraries --------------

library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Sweden --------------

# based on Eurostat Grid

# url_kontur_data_swe <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_SE_20231101.gpkg.gz"
# download(
#     url = url_kontur_data_swe,
#     dest = "kontur_data_swe.gz",
#     mode = "wb"
# ) # downloads zip folder into current directory

swe_borders <- getbb("Sweden", format_out = "polygon", featuretype = "country")

borders <- st_polygon(swe_borders) %>%
    st_sfc(crs = 4326) %>%
    st_transform(3035) %>%
    st_geometry() %>%
    st_as_text()

dat_swe <- st_read(
    dsn = "data_download/grid_1km_surf.gpkg",
    wkt_filter = borders
)

# Create list of plots
plot_swe <- dat_swe %>%
    filter(TOT_P_2021 > 1) |>
    ggplot() +
    geom_sf(aes(fill = (TOT_P_2021)), color = NA) +
    scale_fill_gradient(
        name = "Population",
        low = "#e8e3f8", high = "#1b153d",
    ) +
    labs(
        title = "Population Density Sweden 2021",
        caption = "Source: Eurostat (2024)"
    ) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.height = unit(2, "cm")
    )

ggsave(
    filename = "viszs/plot_swe4.png",
    plot = plot_swe,
    bg = "white",
    width = 22,
    height = 22,
    unit = "cm",
    dpi = 400
)
