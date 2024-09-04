# /* cSpell:disable */

# Libraries and setup -----------------------------------------------------

library(tidyverse)
library(sf)
library(osmdata)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# download kontur data ----------------------------------------------------

# this script downloads the entire dataset
# sourced from https://data.humdata.org/dataset/kontur-population-dataset

# url.kontur.data <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_20220630.gpkg.gz"
# download(
#     url = url.kontur.data,
#     dest = "kontur_data.gz",
#     mode = "wb"
# ) # downloads zip folder into current directory

# # unziping the dataset
# R.utils::gunzip("kontur_data.gz", destname = "data_download/kontur_data.gpkg")


# OSM boundries -----------------------------------------------------------

# Specify countries here
country.names <- c(
    "Rwanda", "Ecuador",
    "Botswana", "Andorra"
)

# initate lists
borders <- list()
data.pop <- list()
xplot <- list()


# loop through user specified place names
for (i in country.names) {
    # get polygon of country
    pol.borders <- getbb(i, format_out = "polygon", featuretype = "country")

    # Checking if borders are saved as a list or not, depends on complexity of country shape
    if (is.list(pol.borders)) {
        # converting polygons into well-known-text for filtering
        # ordering of list is important otherwise it does not work for some countries (e.g UK)
        # Reproject of CRS is also neccessary
        borders <- st_polygon(pol.borders[order(sapply(pol.borders, length),
            decreasing = TRUE
        )]) %>%
            st_sfc(crs = 4326) %>%
            st_transform(3857) %>%
            st_geometry() %>%
            st_as_text()
    } else {
        # execute with simple country shapes, simlar steps as above
        borders <- st_polygon(list(pol.borders)) %>%
            st_sfc(crs = 4326) %>%
            st_transform(3857) %>%
            st_geometry() %>%
            st_as_text()
    }

    # load country specific data into R
    data.pop[[i]] <- st_read(
        dsn = "data_download/kontur_data.gpkg", layer = "population",
        wkt_filter = borders
    )

    # Create list of plots
    xplot[[i]] <- data.pop[[i]] %>%
        ggplot(aes(fill = population)) +
        geom_sf(color = NA) +
        scale_fill_gradient(
            name = "Population",
            low = "#e3daff", high = "#1b153d",
            guide = guide_colourbar(
                direction = "horizontal",
                barwidth = 10
            )
        ) +
        theme_bw() +
        theme(
            axis.text = element_blank(),
            panel.background = element_rect(fill = "white"),
            legend.position = "bottom",
            panel.grid = element_blank(),
            axis.ticks = element_blank()
        )

    # End loop
}

# save all plots in list as pngs
invisible(
    lapply(
        seq_along(xplot),
        function(x) {
            ggsave(
                filename = paste0("viszs/plot_", names(xplot[x]), ".png"),
                plot = xplot[[x]],
                bg = "white",
                width = 22,
                height = 22,
                unit = "cm"
            )
        }
    )
)



# Egypt, somethow does not work:axis.text
egypt_borders <- getbb("Egypt", format_out = "polygon", featuretype = "country")


g <- egypt_borders[[2]]


borders <- st_polygon(g) %>%
    st_sfc(crs = 4326) %>%
    st_transform(3857) %>%
    st_geometry() %>%
    st_as_text()

# load country specific data into R
dat_egypt <- st_read(
    dsn = "data_download/kontur_data.gpkg", layer = "population",
    wkt_filter = borders
)

# Create list of plots
plot_egypt <- dat_egypt %>%
    ggplot(aes(fill = population)) +
    geom_sf(color = NA) +
    scale_fill_gradient(
        name = "Population",
        low = "#e8e3f8", high = "#1b153d",
        guide = guide_colourbar(
            direction = "horizontal",
            barwidth = 10
        )
    ) +
    theme_bw() +
    theme(
        axis.text = element_blank(),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom",
        panel.grid = element_blank(),
        axis.ticks = element_blank()
    )


ggsave(
    filename = "viszs/plot_egypt.png",
    plot = plot_egypt,
    bg = "white",
    width = 22,
    height = 22,
    unit = "cm"
)
