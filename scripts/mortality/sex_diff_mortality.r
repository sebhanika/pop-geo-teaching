# Title: Mortality Sex
# Date: 2024-01-19
# Purpose: Sex differences in mortality
# /* cSpell:disable */

# Library --------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(countrycode)
library(gganimate)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Load data --------------

# Specify countries of interest
mort_countries <- c("SWE", "USA", "JPN", "POL")


cntry_labels <- setNames(
    countrycode(mort_countries,
        origin = "iso3c",
        destination = "country.name"
    ),
    mort_countries
)

mort <- list()

for (i in seq_along(mort_countries)) {
    mort[[i]] <- readHMDweb(
        CNTRY = mort_countries[i],
        "Mx_1x1",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )
    mort[[i]]$CNTRY <- mort_countries[i]
}

# combine data
mort_comb <- do.call(dplyr::bind_rows, mort) %>%
    select(-c(Total)) %>%
    pivot_longer(
        cols = c(Female, Male),
        names_to = "Sex"
    ) %>%
    mutate(value = ifelse(value == 0, NA, value))


# mid age mortality hump across different countries



# Mid-age hump countries --------------

mid_age_hump <- mort_comb %>%
    filter(Year == 2019) %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, col = Sex),
        linewidth = 1.1, alpha = 0.75
    ) +
    scale_y_log10() +
    scale_color_manual(values = (c("#9C6114", "#000080"))) +
    scale_linetype_manual(values = (c(1, 3))) +
    labs(
        x = "Age",
        y = "Age-specfic death rates (log)",
        title = "Age-specfic death rates in 2019",
        caption = "Source: Human Mortality Database (2023)"
    ) +
    facet_wrap(~CNTRY, labeller = as_labeller(cntry_labels)) +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom"
    )
mid_age_hump


ggsave(
    filename = "viszs/mid_age_hump.png",
    plot = mid_age_hump, width = 25, height = 20, units = "cm"
)

# Mid-age hump gif --------------


usa <- mort_comb %>%
    filter(Year %in% c(1933:2019), CNTRY == "USA") %>%
    ggplot() +
    geom_point(aes(
        x = Age, y = value,
        color = Sex, shape = Sex
    )) +
    scale_color_manual(values = (c("#9C6114", "#000080"))) +
    scale_y_log10() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = c(0.905, 0.07),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        )
    ) +
    labs(
        x = "Age",
        y = "Age-specfic death rates (log)",
        caption = "Source: Human Mortality Database (2023)"
    )

usa_gif <- usa +
    transition_time(Year) +
    labs(title = "Age-specfic death rates in USA in: {frame_time}")


anim_save("viszs/mid_age_hump_usa.gif", usa_gif,
    fps = 5.5,
    renderer = gifski_renderer(loop = TRUE),
    height = 20, width = 25, units = "cm", res = 150
)
