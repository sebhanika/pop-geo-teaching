# Title: Births
# Date: 2023-09-22
# Purpose: births in sweden
# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)
library(countrycode)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------

births <-
    readHFDweb(
        CNTRY = "SWE",
        item = "birthsRR",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )

plot_births <- births %>%
    filter(Year %in% seq(1982, 2022, 20)) %>%
    ggplot(aes(
        x = Age, y = Total,
        color = as.factor(Year),
        linetype = as.factor(Year)
    )) +
    geom_line(linewidth = 1.2, alpha = 0.85) +
    scale_color_manual(values = park_palette("GeneralGrant")) +
    labs(
        x = "Age of mother",
        y = "Births",
        title = "Number of births in Sweden in selected years",
        caption = "Source: Human Fertility Database (2023)"
    ) +
    theme(
        legend.position = c(0.92, 0.89),
        legend.title = element_blank(),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.key.width = unit(2, "cm"),
    )

plot_births


ggsave(
    filename = "viszs/births.png",
    plot = plot_births, width = 32, height = 18, units = "cm"
)
