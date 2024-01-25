# Title: COD Graphs
# Date: 2024-01-22
# Purpose: Visualize Causes of Death
# /* cSpell:disable */


# Libraries --------------

library(tidyr)
library(ggplot2)
library(dplyr)
library(nationalparkcolors)
library(gganimate)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Causes of death --------------

# icd-10 recoding to 10 categories
categs_cod <- function(x) {
    case_when(
        x == 2 ~ "Cancer",
        x == 3 ~ "Circulatory Diseases",
        x == 5 ~ "Mental Diseases",
        x == 6 ~ "Diseases of the Nervous System",
        x == 7 ~ "Circulatory Diseases",
        x == 8 ~ "Cerebrovascular Diseases",
        x == 9 ~ "Circulatory Diseases",
        x == 10 ~ "Resperatory Diseases",
        x == 11 ~ "Resperatory Diseases",
        x == 14 ~ "Perinatal, Congenital and Genitourinary Conditions",
        x == 15 ~ "Perinatal, Congenital and Genitourinary Conditions",
        TRUE ~ "Other"
    )
}

# corresponding pallete
cod_palette <- c(
    "Cancer" = "#a6cee3",
    "Circulatory Diseases" = "#176294",
    "Mental Diseases" = "#b2df8a",
    "Diseases of the Nervous System" = "#33a02c",
    "Cerebrovascular Diseases" = "#fb9a99",
    "Resperatory Diseases" = "#942527",
    "Perinatal, Congenital and Genitourinary Conditions" = "#fdbf6f",
    "Other" = "#acacac"
)

# Factor order
cod_order <- c(
    "Cancer", "Cerebrovascular Diseases", "Circulatory Diseases",
    "Diseases of the Nervous System", "Mental Diseases",
    "Perinatal, Congenital and Genitourinary Conditions",
    "Resperatory Diseases", "Other"
)


# Load data --------------

# Sourced from the Human CoD database

cod_ger <- read.csv(file = "data_download/DEUTNP_d_short_idr.csv") %>%
    filter(
        year == 2016,
        cause != 0, # rm all cause
        sex != 3 # rm total sex
    ) %>%
    select(-c(d95, d85p, d90p, d100p, list, agf)) %>%
    pivot_longer(cols = starts_with("d"), names_to = "age") %>%
    mutate(
        # age categ to numeric, last open interval
        age = as.numeric(gsub("[^0-9.]", "", age)),
        sex = as.factor(sex),
        # recode CoD
        cause_categ = categs_cod(cause),
        cause_categ = factor(cause_categ, levels = cod_order)
    ) %>%
    group_by(cause_categ, age, sex) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()



# Axis labelling for CoD "pyramid"
max_death_age <- cod_ger %>%
    group_by(age, sex) %>%
    summarise(value = sum(value, na.rm = TRUE))

max_death <- max(max_death_age$value)

# get first two digits of rounded max death. expressed in thousands
max_death_lim <- round(max_death, -4)
max_death_label <- as.numeric(substr(max_death_lim, 1, 3))
# get breaks for axis, needs negative values
death_brks <- seq(-max_death_lim, max_death_lim, max_death_lim / 2)
# get nice labels
death_labels <- abs(seq(-max_death_label, max_death_label, max_death_label / 2))



# Cod Pyramid plot
cod_pyr <- cod_ger %>%
    ggplot(aes(
        x = as.factor(age),
        y = ifelse(sex == 1, -value, value),
        fill = cause_categ
    )) +
    geom_bar(stat = "identity", width = 0.8, alpha = 0.8) +
    geom_hline(yintercept = 0, colour = "#1a0a0a") +
    scale_y_continuous(
        limits = c(-max_death, max_death),
        breaks = death_brks,
        labels = death_labels
    ) +
    scale_fill_manual("Cause of Death", values = cod_palette) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Deaths in Thousand",
        caption = "Source: Human CoD Database (2023)",
        title = "Causes of Death in Germany in 2016"
    ) +
    theme(
        legend.text = element_text(size = 14),
        legend.position = c(0.82, 0.3),
        legend.background = element_rect(
            linetype = "solid",
            color = "black"
        ),
    ) +
    annotate("text", x = 1, y = -max_death / 2, label = "Male", size = 7) +
    annotate("text", x = 1, y = max_death / 2, label = "Female", size = 7)
cod_pyr

ggsave(
    filename = "viszs/cod_pyr.png",
    plot = cod_pyr, width = 40, height = 20, units = "cm"
)
