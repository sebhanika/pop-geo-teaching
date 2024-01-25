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


# School enrollment, primary, female (% net)
prim_edu_f <-
    wb_data("SE.PRM.NENR.FE",
        start_date = 1960, end_date = 2021
    ) %>%
    rename(prim_edu = "SE.PRM.NENR.FE") %>%
    select(c(prim_edu, date, iso3c))


dat_comb <- tfr_dat %>%
    full_join(prim_edu_f) %>%
    mutate(region = countrycode(
        sourcevar = iso3c,
        origin = "iso3c",
        destination = "region"
    )) %>%
    drop_na(region)


eval_na <- dat_comb %>%
    group_by(date) %>%
    summarise(across(.cols = where(is.numeric), .fns = ~ (sum(is.na(.x) / n())
    )))


tfr_edu <- dat_comb %>%
    filter(date == 2014) %>%
    ggplot(aes(x = prim_edu, y = tfr)) +
    geom_point(aes(color = region), size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, color = "#5a5a5a") +
    scale_color_manual(values = park_palette("CraterLake")) +
    labs(
        x = "Total Fertilty Rate",
        y = "Primary School Enrollment, Female (%)",
        title = "Total Fertility Rate and Female Education in 2014",
        caption = "Source: World Bank (2023)"
    ) +
    theme(
        legend.position = c(0.16, 0.2),
        legend.text = element_text(size = 16),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.title = element_blank()
    )
tfr_edu


# save plot for presentation
ggsave(
    filename = "viszs/tfr_edu.png",
    plot = tfr_edu,
    width = 32, height = 18, units = "cm"
)
