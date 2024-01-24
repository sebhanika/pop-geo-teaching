# Title: Fert_econ
# Date: 2024-01-24
# Purpose: Life expectancy and GDP per capita
# /* cSpell:disable */

# Libraries --------------
library(dplyr)
library(tidyr)
library(wbstats)
library(ggplot2)
library(countrycode)
library(gganimate)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------

tfr_dat <- wb_data("SP.DYN.TFRT.IN", start_date = 1960, end_date = 2021) %>%
    rename(tfr = SP.DYN.TFRT.IN) %>%
    select(c(tfr, date, iso3c))


gdp_dat <- wb_data("NY.GDP.PCAP.CD", start_date = 1960, end_date = 2021) %>%
    rename(gdp_cap = "NY.GDP.PCAP.CD") %>%
    select(c(gdp_cap, date, iso3c, country)) %>%
    mutate(region = countrycode(
        sourcevar = country,
        origin = "country.name",
        destination = "region"
    ))

pop_dat <- wb_data("SP.POP.TOTL", start_date = 1960, end_date = 2021) %>%
    rename(pop = "SP.POP.TOTL") %>%
    select(c(pop, date, iso3c))


lit_dat <- wb_data("SE.ADT.LITR.FE.ZS", start_date = 1960, end_date = 2021) %>%
    rename(female_lit = "SE.ADT.LITR.FE.ZS") %>%
    select(c(female_lit, date, iso3c))


try <- lit_dat %>% filter(date == 2020)



dat_comb <- tfr_dat %>%
    full_join(gdp_dat) %>%
    left_join(lit_dat)



dat_comb %>%
    filter(date == 2015) %>%
    ggplot(aes(x = female_lit, y = tfr)) +
    geom_point()
