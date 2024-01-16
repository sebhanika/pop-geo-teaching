# Title: deaths_map
# Date: 2024-01-16
# Purpose: Map of deaths
# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)
library(countrycode)
library(nationalparkcolors)
library(eurostat)
library(ggspatial)

source("scripts/0_config.R")
source("scripts/0_settings.R")


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
    mutate(region = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "un.regionsub.name"
    )) %>%
    mutate(country = countrycode(
        sourcevar = cntr_code,
        origin = "eurostat",
        destination = "country.name"
    )) %>%
    select(c(cntr_code, name_latn, geo, geometry, region, country)) %>%
    rename(nuts2_name = name_latn) %>%
    relocate(cntr_code, .before = region) %>%
    relocate(country, .before = cntr_code)


deaths_raw <- get_eurostat("demo_r_magec", time_format = "num")

pop <- get_eurostat("demo_r_d2jan", time_format = "num") %>%
    filter(TIME_PERIOD == 2019, nchar(geo) == 4, age == "TOTAL", sex == "T") %>%
    mutate(sex = ifelse(sex == "T", "pop", "no")) %>%
    pivot_wider(names_from = sex, values_from = values) %>%
    select(c(geo, pop))

deaths <- deaths_raw %>%
    filter(TIME_PERIOD == 2019, nchar(geo) == 4, age == "TOTAL") %>%
    pivot_wider(names_from = sex, values_from = values) %>%
    select(-c(freq, unit, age, TIME_PERIOD))


dat_comb <- nuts2_v1 %>%
    left_join(deaths) %>%
    left_join(pop) %>%
    filter(pop > 0) %>%
    mutate(
        cdr_f = F / pop * 1000,
        cdr_m = M / pop * 1000,
        cdr_t = T / pop * 1000
    )


dat_comb %>% ggplot() +
    geom_sf(aes(fill = cdr_f))




##### OLD CODE

# creating custom color palette
self_palette <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

# bounding box
xlim <- c(2426378.0132, 6593974.6215)
ylim <- c(1328101.2618, 5446513.5222)

# create bins for chrolopeth map

data_bins <- BAMMtools::getJenksBreaks(dat_comb$cdr_t, k = 6)

cdr_map <- dat_comb %>%
    mutate(val_int = cut(cdr_t,
        breaks = data_bins, ,
        labels = c(
            "3.02 - 6.55", "6.55 - 9.03",
            "9.03 - 10.97", "10.97 - 13.24",
            "13-24 - 20.35"
        ),
        include.lowest = TRUE
    ))

# create map object
p_cdr_map <- cdr_map %>%
    ggplot() +
    geom_sf(aes(fill = as.factor(val_int)),
        linewidth = 0.1, alpha = 1
    ) +
    # coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    scale_fill_manual("CDR (2020)",
        values = self_palette,
        na.value = "#a7a7a7"
    ) +
    labs(caption = "Source: Eurostat") +
    theme_base() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.88, 0.88)
    ) +
    annotation_scale(height = unit(0.15, "cm"))


p_cdr_map


head(x)
unique(x$sex)
