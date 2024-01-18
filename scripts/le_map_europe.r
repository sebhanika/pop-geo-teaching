# Title: Life Expectancy
# Date: 2024-01-04
# Purpose: Life expectancy for selected countries
# /* cSpell:disable */

# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)
library(countrycode)
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
    relocate(cntr_code, .before = region) %>%
    relocate(country, .before = cntr_code) %>%
    st_transform(3035)
