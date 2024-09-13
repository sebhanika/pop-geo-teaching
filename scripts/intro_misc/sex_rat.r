# Title: sex_rat
# Date: 2024-09-13
# Purpose: Sex ratio for selected countries population pyramid sweden

# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(countrycode)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Specify countries of interest
sex_countries <- c("SWE", "JPN", "ESP", "NLD")

# create labels
cntry_labels <- setNames(
    countrycode(sex_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    sex_countries
)

# download data
sex_rat <- list()

for (i in seq_along(sex_countries)) {
    sex_rat[[i]] <- readHMDweb(
        CNTRY = sex_countries[i],
        "Population",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )
    sex_rat[[i]]$CNTRY <- sex_countries[i]
}

# combine data
sex_rat2 <- do.call(dplyr::bind_rows, sex_rat) %>%
    janitor::clean_names() |>
    group_by(year, cntry) |>
    summarize(across(.cols = where(is.numeric), .fns = ~ sum(.x))) |>
    mutate(rat_sex = male2 / female2) |>
    ungroup()

saguaro <- c("#847CA3", "#E45A5A", "#F4A65E", "#80792B", "#F2D56F", "#1A1237")


# create plot object
sex_rat_plot <- sex_rat2 %>%
    filter(year %in% 1900:2021) %>%
    ggplot() +
    geom_line(aes(
        x = year, y = rat_sex,
        color = cntry, linetype = cntry
    ), lwd = 1.25) +
    scale_x_continuous(
        limits = c(1900, 2021),
        breaks = seq(1900, 2020, 20)
    ) +
    scale_color_manual(
        values = saguaro,
        labels = cntry_labels
    ) +
    scale_linetype_manual(
        values = c(1, 2, 3, 4, 6),
        labels = cntry_labels
    ) +
    labs(
        x = "", y = "Sex ratio (male/females)",
        title = "Sex ratio in selected countries",
        caption = "Source: Human Mortality Database (2024)"
    ) +
    theme(
        legend.position = c(0.89, 0.15),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.title = element_blank(),
        legend.key.width = unit(1.5, "cm")
    )
sex_rat_plot


ggsave(
    filename = "viszs/sex_rat_plot.png",
    plot = sex_rat_plot, width = 32, height = 18, units = "cm"
)
