# Title: Internal_Migration_UK
# Date: 2024-01-31
# Purpose: Script Purpose
# /* cSpell:disable */


# Libraries --------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(geojsonsf)
library(sf)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# data downloaded from ONS.gov.uk
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset # nolint

# geodata from: https://geoportal.statistics.gov.uk/datasets/7ceb69f99a024752b97ddac6b0323ab0_0/explore?location=55.215503%2C-3.316939%2C6.98 #nolint


dat1 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_1.csv") # nolint
dat2 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_2.csv") # nolint

bng <- 'PROJCS["OSGB36 / British National Grid",
    GEOGCS["OSGB36",
        DATUM["Ordnance_Survey_of_Great_Britain_1936",
            SPHEROID["Airy 1830",6377563.396,299.3249646],
            EXTENSION["PROJ4_GRIDS","OSTN15_NTv2_OSGBtoETRS.gsb"]],
        PRIMEM["Greenwich",0,
            AUTHORITY["EPSG","8901"]],
        UNIT["degree",0.0174532925199433,
            AUTHORITY["EPSG","9122"]],
        AUTHORITY["EPSG","4277"]],
    PROJECTION["Transverse_Mercator"],
    PARAMETER["latitude_of_origin",49],
    PARAMETER["central_meridian",-2],
    PARAMETER["scale_factor",0.9996012717],
    PARAMETER["false_easting",400000],
    PARAMETER["false_northing",-100000],
    UNIT["metre",1,
        AUTHORITY["EPSG","9001"]],
    AXIS["Easting",EAST],
    AXIS["Northing",NORTH],
    AUTHORITY["EPSG","27700"]]'

la_boundries <- geojson_sf("data_download/Local_Authority_Districts_December_2021_UK_BGC_2022_4923559779027843470.geojson", wkt = bng, input = 27700) # nolint


la_coords <- la_boundries %>%
    st_drop_geometry() %>%
    select(c(LAD21CD, LONG, LAT))


dat_comb <- bind_rows(dat1, dat2) %>%
    group_by(outla, inla) %>%
    summarise(moves = sum(moves, na.rm = TRUE)) %>%
    ungroup()


origin <- dat_comb %>%
    left_join(la_coords, by = c("outla" = "LAD21CD"))
names(origin) <- c("outla", "inla", "moves", "origin_x", "origin_y")

dest <- origin %>%
    left_join(la_coords, by = c("inla" = "LAD21CD")) %>%
    rename(dest_x = LONG) %>%
    rename(dest_y = LAT) %>%
    filter(moves > 250)


xquiet <- scale_x_continuous("", breaks = NULL)
yquiet <- scale_y_continuous("", breaks = NULL)
quiet <- list(xquiet, yquiet)

plot_map <-
    ggplot(dest, aes(origin_x, origin_y)) +
    geom_segment(aes(
        x = origin_x, y = origin_y,
        xend = dest_x, yend = dest_y, alpha = moves
    ), col = "#1f1f1f") +
    scale_alpha_continuous(range = c(0.03, 0.3)) +
    theme_void() +
    theme(panel.background = element_rect(
        fill = "#ffffff",
        colour = "#ffffff"
    ))

plot_map

ggsave(
    filename = "viszs/int_mig_uk.png", plot = plot_map,
    units = "cm", dpi = 600
)




# Trying out stuff --------------

try1 <- dest %>%
    select(origin_x, origin_y, dest_x, dest_y) %>%
    drop_na()


st_segment <- function(r) {
    st_linestring(t(matrix(unlist(r), 2, 2)))
}


try1$geometry <- st_sfc(sapply(1:nrow(try1),
    function(i) {
        st_segment(try1[i, ])
    },
    simplify = FALSE
), crs = "27700")

try1 <- st_as_sf(try1)

try1 <- try1 %>% st_set_crs(st_crs(la_boundries))


try2 <- try1 %>%
    left_join(dest) %>%
    select(c(geometry, moves))


ggplot() +
    geom_sf(data = try2, aes(alpha = moves), col = "#1f1f1f") +
    scale_alpha_continuous(range = c(0.03, 0.3))
