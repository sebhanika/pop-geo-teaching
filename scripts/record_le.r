# Title: Record life Expectancy
# Date: 2024-01-04
# Purpose: Record life expectancy
# /* cSpell:disable */

# Library --------------
library(dplyr)
library(tidyr)
library(HMDHFDplus)
library(countrycode)
library(nationalparkcolors)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load le data --------------
# download life expectancy for all countries

# get country list (codes)
hmd_cntr <- getHMDcountries()$CNTRY

le <- list()

for (i in seq_along(hmd_cntr)) {
    le[[i]] <- readHMDweb(
        CNTRY = hmd_cntr[i],
        "E0per",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )
    le[[i]]$CNTRY <- hmd_cntr[i]
}

# combine data
le_comb <- do.call(dplyr::bind_rows, le)

# Max female life expectancy  --------------

# recreating the analysis/plot by Oeppen and Vaupel (2002)
# https://www.science.org/doi/10.1126/science.1069675

# filters for max value in each year
# however in 1861 there are two max values (DKK, SWE)
le_max <- le_comb %>%
    group_by(Year) %>%
    filter(Female == max(Female, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(Year) %>%
    distinct(Year, Female, .keep_all = TRUE) %>%
    filter(Year %in% c(1840:2021))

# create a numerrical factor for plotting
max_cntrs <- (unique(le_max$CNTRY))
country_numbers <- as.integer(factor(max_cntrs))
le_max$cntry_fac <- factor(le_max$CNTRY,
    levels = unique(le_max$CNTRY), labels = country_numbers
)


# create plot and export
png(
    filename = "viszs/record_le.png",
    width = 32, height = 18, units = "cm",
    res = 300
)
plot(
    x = le_max$Year,
    y = le_max$Female,
    pch = as.integer(le_max$cntry_fac),
    col = as.integer(le_max$cntry_fac),
    xlab = "Year",
    ylab = "Life Expecantcy at birth in years",
    main = "Record Female Life Expectancy"
)

legend(
    x = "bottomright",
    inset = 0.01,
    legend = max_cntrs,
    pch = factor(levels(factor(le_max$CNTRY))),
    col = factor(levels(factor(le_max$CNTRY))),
    cex = 1.5
)
dev.off()
