# Title: birth_rates
# Date: 2024-01-25
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


# Age-specific fertility rates --------------
# Specify countries of interest
asfr_countries <- c("SWE", "USA")


# create labels
cntry_labels <- setNames(
    countrycode(asfr_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    asfr_countries
)

# download data
asfr <- list()
for (i in seq_along(asfr_countries)) {
    asfr[[i]] <- readHFDweb(
        CNTRY = asfr_countries[i],
        username = hfd_username,
        password = hfd_password,
        item = "asfrTR",
        fixup = TRUE
    )
    asfr[[i]]$CNTRY <- asfr_countries[i]
}

# combine data
asfr_comb <- do.call(dplyr::bind_rows, asfr) %>%
    janitor::clean_names()



# Plots Sweden-USA --------------
plot_asfr <- asfr_comb %>%
    filter(year == 2019) %>%
    ggplot(aes(x = age, y = asfr, linetype = cntry, color = cntry)) +
    geom_line(linewidth = 1.1, alpha = 0.75) +
    scale_color_manual(
        values = (c("#9C6114", "#000080")),
        labels = cntry_labels
    ) +
    scale_linetype_manual(
        values = (c(1, 3)),
        labels = cntry_labels
    ) +
    theme(
        legend.position = c(0.13, 0.91),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.key.width = unit(2, "cm"),
        legend.title = element_blank()
    ) +
    labs(
        x = "Age",
        y = "Age-specfic fertility rates",
        title = "Age-specfic death rates for females in 2019",
        caption = "Source: Human Mortality Database (2023)"
    )
plot_asfr


ggsave(
    filename = "viszs/asfr.png",
    plot = plot_asfr, width = 32, height = 18, units = "cm"
)
