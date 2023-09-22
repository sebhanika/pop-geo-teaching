# HMD Test
# Title: Life expectancy data
# Date: 2023-09-21
# Purpose: This scripts uses the HMD to create graphs for teaching

# /* cSpell:disable */

# Libraries --------------

libray(tidyverse)
library(HMDHFDplus)

source("scripts/configs.R")

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
png(filename = "viszs/record_le.png")
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


# life expectnacy over time --------------
# for selected countries

jpn <- subset(le_comb, CNTRY == "JPN")
swe <- subset(le_comb, CNTRY == "SWE")
fra <- subset(le_comb, CNTRY == "FRATNP")
dnk <- subset(le_comb, CNTRY == "DNK")


# export image
png(file = "viszs/le_countries.png")

plot(
    x = swe$Year, swe$Female,
    type = "l", col = "#000000", lwd = 2.2,
    xlim = c(1900, 2022), ylim = c(43, 87.5),
    xlab = "Year",
    ylab = "Female period life expectancy at birth in years",
    main = "Life expectancy across countries"
)
lines(x = jpn$Year, jpn$Female, col = "#ac57fc", lty = 2, lwd = 2.2)
lines(x = fra$Year, fra$Female, col = "red", lty = 3, lwd = 2.2)
lines(x = dnk$Year, dnk$Female, col = "#36f5fc", lty = 4, lwd = 2.2)

legend(
    x = "bottomright",
    inset = 0.01,
    legend = c("Sweden", "Japan", "France", "Denmark"),
    col = c("black", "#ac57fc", "red", "#36f5fc"),
    lty = c(1, 2, 3, 4),
    cex = 1.7
)

dev.off()
