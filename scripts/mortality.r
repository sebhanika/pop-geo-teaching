# Title: Mortality
# Date: 2023-09-22
# Purpose: Mortality visualizations for teaching

# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)


source("scripts/0_config.R")
source("scripts/0_settings.R")


# Load data --------------

# Specify countries of interest
mort_countries <- c("SWE", "USA", "JPN", "AUS", "POL", "ESP")
mort <- list()

# download data
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
    facet_wrap(~CNTRY) +
    scale_y_log10() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom"
    )



# mid age mortality hump across time for USA

# set years, not more than 4
yrs <- seq(1959, 2019, 20)

# create plot
usa <- mort_comb %>%
    filter(Year %in% yrs, CNTRY == "USA") %>%
    ggplot() +
    geom_line(aes(
        x = Age, y = value,
        color = as.factor(Year),
        linetype = as.factor(Year)
    )) +
    facet_wrap(~Sex) +
    scale_y_log10() +
    theme_base() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.position = "bottom"
    ) +
    labs(
        x = "Age",
        y = "Death rates (log)"
    )


# Demographic transition --------------


# death rate
swe_deaths <- readHMDweb(
    CNTRY = "SWE",
    "Deaths_1x1",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(-c(Female, Male)) %>%
    rename(deaths = Total) %>%
    group_by(Year) %>%
    summarise(across(
        .cols = c(deaths),
        .fns = ~ sum(.x, na.rm = TRUE)
    )) %>%
    ungroup()



swe_pop <- readHMDweb(
    CNTRY = "SWE",
    "Population",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
) %>%
    select(c(Year, Age, Total2)) %>%
    rename(pop = Total2) %>%
    group_by(Year) %>%
    summarise(across(
        .cols = c(pop),
        .fns = ~ sum(.x, na.rm = TRUE)
    )) %>%
    ungroup()


# crude birth rate
swe_births <- readHMDweb(
    CNTRY = "SWE",
    "Births",
    username = hfd_username,
    password = hfd_password,
    fixup = TRUE
) %>%
    select(-c(Female, Male)) %>%
    rename(births = Total)


swe_dat <- swe_deaths %>%
    left_join(swe_births, by = c("Year")) %>%
    left_join(swe_pop, by = c("Year")) %>%
    mutate(
        cbr = births / (pop / 1000),
        cdr = deaths / (pop / 1000)
    )


plot(swe_dat$Year, swe_dat$cdr, type = "l")
lines(swe_dat$Year, swe_dat$cbr, lty = 2)
