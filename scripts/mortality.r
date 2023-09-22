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




mort_countries <- c("SWE", "USA", "JPN", "AUS", "POL", "ESP")
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
    pivot_longer(cols = c(Female, Male), names_to = "Sex") %>%
    mutate(value = ifelse(value == 0, NA, value))





mort_comb %>%
    filter(Year == 2019, CNTRY == "POL") %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, col = Sex)) +
    scale_y_log10() +
    theme_base()




# mid age mortality hump across different countries

mort_comb %>%
    filter(Year == 2019) %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, col = Sex)) +
    facet_wrap(~CNTRY) +
    scale_y_log10()



# mid age mortality hump across time for USA

# set years, not more than 4
yrs <- seq(1959, 2019, 20)

# create plot
usa <- mort_comb %>%
    filter(Year %in% yrs, CNTRY == "USA") %>%
    ggplot() +
    geom_line(aes(x = Age, y = value, color = as.factor(Year))) +
    theme_bw() +
    facet_wrap(~Sex) +
    scale_y_log10()
usa
