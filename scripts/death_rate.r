# Title: Death rate
# Date: 2023-09-22
# Purpose: death rates in countries
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

# Specify countries of interest
mort_countries <- c("SWE", "USA", "JPN", "ESP")


# create labels
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


# Plots --------------

# Plot force of mortality SWE

swe_plot <- mort_comb %>%
    filter(Year == 2019, CNTRY == "SWE") %>%
    ggplot(aes(x = Age, y = value, col = Sex, linetype = Sex)) +
    geom_line(linewidth = 1.1, alpha = 0.75) +
    scale_y_log10() +
    scale_color_manual(values = (c("#9C6114", "#000080"))) +
    scale_linetype_manual(values = (c(1, 3))) +
    theme(
        legend.position = c(0.094, 0.91),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
        legend.key.width = unit(2, "cm"),
    ) +
    labs(
        x = "Age",
        y = "Age-specfic death rates (log)",
        title = "Age-specfic death rates in Sweden in 2019",
        caption = "Source: Human Mortality Database (2023)"
    )

swe_plot


ggsave(
    filename = "viszs/asdr.png",
    plot = swe_plot, width = 32, height = 18, units = "cm"
)




# Country comparison --------------

asdr_cntr_f <- mort_comb %>%
    filter(Year == 2019, Sex == "Female", CNTRY %in% c("SWE", "USA")) %>%
    ggplot(aes(x = Age, y = value, linetype = CNTRY, color = CNTRY)) +
    geom_line(linewidth = 1.1, alpha = 0.75) +
    scale_y_log10() +
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
    ) +
    labs(
        x = "Age",
        y = "Age-specfic death rates (log)",
        title = "Age-specfic death rates for females in 2019",
        caption = "Source: Human Mortality Database (2023)"
    )

asdr_cntr_f

ggsave(
    filename = "viszs/asdr_cntr_F.png",
    plot = asdr_cntr_f, width = 32, height = 18, units = "cm"
)




# Males

asdr_cntr_m <- mort_comb %>%
    filter(Year == 2019, Sex == "Male", CNTRY %in% c("SWE", "USA")) %>%
    ggplot(aes(x = Age, y = value, linetype = CNTRY, color = CNTRY)) +
    geom_line(linewidth = 1.1, alpha = 0.75) +
    scale_y_log10() +
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
    ) +
    labs(
        x = "Age",
        y = "Age-specfic death rates (log)",
        title = "Age-specfic death rates for females in 2019",
        caption = "Source: Human Mortality Database (2023)"
    )

asdr_cntr_m

ggsave(
    filename = "viszs/asdr_cntr_m.png",
    plot = asdr_cntr_m, width = 32, height = 18, units = "cm"
)
















# Plot force of mortality USA
mort_comb %>%
    filter(Year == 2019, CNTRY == "USA") %>%
    ggplot(aes(x = Age, y = value, col = Sex)) +
    geom_line() +
    scale_y_log10() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = c(0.92, 0.05),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        )
    ) +
    labs(
        x = "Age",
        y = "Death rates (log)", title = "Death rates USA in 2019"
    )



# mid age mortality hump across different countries

mort_comb %>%
    filter(Year == 2019) %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, col = Sex)) +
    facet_wrap(~CNTRY, labeller = as_labeller(cntry_labels)) +
    scale_y_log10() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom"
    )
