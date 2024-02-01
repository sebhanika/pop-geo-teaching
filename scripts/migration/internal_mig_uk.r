# Title: Internal_Migration_UK
# Date: 2024-01-31
# Purpose: Script Purpose
# /* cSpell:disable */

# Libraries --------------
library(tidyverse)
library(geojsonsf)
library(sf)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# data downloaded from ONS.gov.uk
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset # nolint
# geodata from: https://geoportal.statistics.gov.uk/datasets/7ceb69f99a024752b97ddac6b0323ab0_0/explore?location=55.215503%2C-3.316939%2C6.98 #nolint

dat1 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_1.csv") # nolint
dat2 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_2.csv") # nolint

dat_comb <- bind_rows(dat1, dat2) %>%
    group_by(outla, inla) %>%
    summarise(moves = sum(moves, na.rm = TRUE)) %>%
    ungroup()

# Wkt for geosjon right projection
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

la_boundries <- geojson_sf("data_download/Local_Authority_Districts_December_2021_UK_BGC_2022_4923559779027843470.geojson", # nolint
    wkt = bng, input = 27700
)

# Calc Centroid and extract coordinates
la_coords <- la_boundries %>%
    select(c(LAD21CD, geometry)) %>%
    mutate(
        LONG = map_dbl(geometry, ~ st_point_on_surface(.x)[[1]]),
        LAT = map_dbl(geometry, ~ st_point_on_surface(.x)[[2]])
    ) %>%
    st_drop_geometry()


# Create dataset with in and out coords
origin <- dat_comb %>%
    left_join(la_coords, by = c("outla" = "LAD21CD"))

names(origin) <- c("outla", "inla", "moves", "origin_x", "origin_y")

dest <- origin %>%
    left_join(la_coords, by = c("inla" = "LAD21CD")) %>%
    rename(dest_x = LONG) %>%
    rename(dest_y = LAT) %>%
    filter(moves > 80) # filter needed for visualization


# Creating multilines from coordinates
mig_coord <- dest %>%
    select(origin_x, origin_y, dest_x, dest_y) %>%
    drop_na()

st_segment <- function(r) {
    st_linestring(t(matrix(unlist(r), 2, 2)))
}

mig_coord$geometry <- st_sfc(sapply(1:nrow(mig_coord), # nolint
    function(i) {
        st_segment(mig_coord[i, ])
    },
    simplify = FALSE
))

mig_dat <- st_as_sf(mig_coord) %>%
    st_set_crs(st_crs(la_boundries)) %>%
    left_join(dest) %>%
    select(c(geometry, moves))

# set bounding box manually
xlim <- c(107007.7280, 657325.1220)
ylim <- c(-14765.8815, 669163.4051)

plot_map <-
    ggplot() +
    geom_sf(
        data = la_boundries, fill = "white",
        lwd = 0.1, color = "#b6b6b6"
    ) +
    geom_sf(
        data = mig_dat, aes(alpha = moves, linewidth = moves),
        col = "#630101"
    ) +
    scale_alpha_continuous("Number of moves", range = c(0.06, 0.45)) +
    scale_linewidth_continuous("Number of moves", range = c(0.5, 2)) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.87, 0.9)
    )

plot_map

ggsave(
    filename = "viszs/int_mig_uk.png", plot = plot_map,
    units = "cm", dpi = 400
)





# really trying stuff
mig <- bind_rows(dat1, dat2)


try2 <- bind_rows(dat1, dat2) %>%
    group_by(outla, inla) %>%
    summarise(moves = sum(moves, na.rm = TRUE)) %>%
    ungroup()

# old
try2 %>%
    pivot_wider(names_from = outla, values_from = moves) %>%
    arrange(inla) %>%
    column_to_rownames(var = "inla") %>%
    as.matrix()
