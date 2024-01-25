# Title: TFR Sweden
# Date: 2024-01-24
# Purpose: Graph for Sweden TFR
# /* cSpell:disable */


# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(countrycode)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# TFR --------------

# Specify countries of interest
tfr_countries <- c("SWE")


# create labels
cntry_labels <- setNames(
    countrycode(tfr_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    tfr_countries
)

# download data
tfr <- list()
for (i in seq_along(tfr_countries)) {
    tfr[[i]] <- readHFDweb(
        CNTRY = tfr_countries[i],
        username = hfd_username,
        password = hfd_password,
        item = "tfrRR",
        fixup = TRUE
    )
    tfr[[i]]$CNTRY <- tfr_countries[i]
}

# combine data
tfr_comb <- do.call(dplyr::bind_rows, tfr) %>%
    janitor::clean_names()

# create plot
tfr_plot <- tfr_comb %>%
    filter(year %in% 1900:2021) %>%
    ggplot() +
    geom_line(aes(
        x = year, y = tfr,
        color = cntry, linetype = cntry
    ), lwd = 1.25) +
    geom_hline(yintercept = 2.1, color = "black", lty = 2) +
    scale_x_continuous(
        limits = c(1900, 2021),
        breaks = seq(1900, 2020, 20)
    ) +
    scale_color_manual(
        values = park_palette("Saguaro"),
        labels = cntry_labels
    ) +
    scale_linetype_manual(
        values = c(1, 2, 3, 4, 5, 6),
        labels = cntry_labels
    ) +
    labs(
        x = "", y = "Total Fertility Rate",
        caption = "Source: Human Fertility Database (2023)",
        title = "Total Fertility Rate Sweden"
    ) +
    annotate("text",
        x = 1907.5, y = 2.15,
        label = "Replacement-level fertility"
    ) +
    theme(
        legend.position = "None"
    )
tfr_plot


# save plot presentation
ggsave(
    filename = "viszs/tfr_sweden.png",
    plot = tfr_plot,
    width = 32, height = 18, units = "cm"
)
