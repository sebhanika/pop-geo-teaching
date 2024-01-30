# Title: dtt_critique
# Date: 2024-01-30
# Purpose: Examples where DTT does not work
# /* cSpell:disable */

# Libraries --------------
library(dplyr)
library(tidyr)
library(wbstats)
library(ggplot2)
library(countrycode)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# Load data --------------

# The data was downloaded from the UN Data portal Population Division
# General Link: https://population.un.org/dataportal/data/
# Specific link: https://population.un.org/dataportal/data/indicators/59,55/locations/156,710,724/start/1950/end/2021/table/pivotbylocation?df=a9de9119-e9f3-42f2-8609-98b14f4a6db7  # nolint: line_length_linter.

csv_path <- "data_download/unpopulation_dataportal_20240130143325.csv"

un_dat <- read.csv(csv_path) %>%
    janitor::clean_names() %>%
    filter(time %in% 1950:2021) %>%
    select(c(time, indicator_name, value, location))


countries <- unique(un_dat$location)
plot_list <- list()

for (i in countries) {
    plot_list[[i]] <-
        un_dat %>%
        filter(location == i) %>%
        ggplot(aes(
            x = time, y = value,
            color = indicator_name, linetype = indicator_name
        )) +
        geom_line() +
        scale_color_manual(
            values = (c("#9C6114", "#000080")),
            label = c("Crude Birth Rate", "Crude Death Rate")
        ) +
        scale_linetype_manual(
            values = (c(1, 3)),
            label = c("Crude Birth Rate", "Crude Death Rate")
        ) +
        labs(
            x = "Year",
            y = "Birth and death rates per 1000 persons",
            title = paste0("Exceptions to the DTT - ", i),
            caption = "Source: Unidted Nations Populations Prospects (2023)"
        ) +
        scale_x_continuous(breaks = seq(1950, 2020, 10)) +
        theme(
            legend.key.width = unit(2, "cm"),
            legend.position = c(0.85, 0.915),
            legend.title = element_blank(),
            legend.background = element_rect(
                linetype = "solid",
                color = "black"
            )
        )
}


# save all plots in list as pngs
invisible(
    lapply(
        seq_along(plot_list),
        function(x) {
            ggsave(
                filename = paste0(
                    "viszs/dtt_",
                    names(plot_list[x]), ".png"
                ),
                plot = plot_list[[x]],
                bg = "white",
                width = 32,
                height = 18,
                unit = "cm"
            )
        }
    )
)
