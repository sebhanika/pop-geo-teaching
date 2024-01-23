# Title: COD Graphs
# Date: 2024-01-22
# Purpose: Visualize Causes of Death
# /* cSpell:disable */


# Libraries --------------

library(tidyr)
library(ggplot2)
library(dplyr)
library(nationalparkcolors)

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
        x == 14 ~ "Perinatal & Congenital Conditions",
        x == 15 ~ "Perinatal & Congenital Conditions",
        TRUE ~ "Other"
    )
}


cpal <- c(
    "Cancer" = "#FF6059",
    "Circulatory Diseases" = "#E3B135",
    "Mental Diseases" = "#79C8CC",
    "Diseases of the Nervous System" = "#FF93F2",
    "Cerebrovascular Diseases" = "#FF8500",
    "Resperatory Diseases" = "#59C580",
    "Perinatal & Congenital Conditions" = "#799BCC",
    "Other" = "#9A9A9A"
)





# Load data --------------

x <- read.csv(file = "data_download/DEUTNP_m_short_idr.csv")

y <- read.csv(file = "data_download/DEUTNP_d_short_idr.csv")

y_try <- y %>%
    filter(cause != 0, sex != 3) %>%
    select(-c(d95, d85p, d90p, d100p, list, agf)) %>%
    pivot_longer(cols = starts_with("d"), names_to = "age") %>%
    mutate(
        age = as.numeric(gsub("[^0-9.]", "", age)),
        open_int = ifelse(age == 95, TRUE, FALSE)
    ) %>%
    mutate(cause_categ = categs_cod(cause)) %>%
    group_by(cause_categ, age, sex, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(sex = as.factor(sex))

x_try <- x %>%
    filter(cause != 0, sex != 3) %>%
    select(-c(m95, m85p, m90p, m100p, list, agf)) %>%
    pivot_longer(cols = starts_with("m"), names_to = "age") %>%
    mutate(
        age = as.numeric(gsub("[^0-9.]", "", age)),
        open_int = ifelse(age == 95, TRUE, FALSE)
    ) %>%
    mutate(cause_categ = categs_cod(cause)) %>%
    group_by(cause_categ, age, sex, year) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup()





x_try %>%
    filter(year == 1998) %>%
    ggplot(aes(x = (age), y = value, fill = as.factor(cause_categ))) +
    geom_area() +
    scale_y_log10() +
    facet_wrap(~sex)




y_try %>%
    filter(year == 2016) %>%
    ggplot(aes(x = age, y = value, fill = cause_categ)) +
    geom_area() +
    facet_wrap(~sex)

?geom_density



max_pop <- max(y_try$value)

# get first two digits of rounded max pop. expressed in thousands
max_pop_lim <- round(max_pop, -4)
max_pop_label <- as.numeric(substr(max_pop_lim, 1, 2))

# get breaks for axis, needs negative values
pop_brks <- seq(-max_pop_lim, max_pop_lim, max_pop_lim / 2)
# get nice labels
pop_labels <- abs(seq(-max_pop_label, max_pop_label, max_pop_label / 2))



# age pyramid plot
age_pyrs <- y_try %>%
    filter(year == 1998) %>%
    ggplot(aes(
        x = age,
        y = ifelse(sex == 1, -value, value),
        fill = cause_categ
    )) +
    geom_bar(stat = "identity", width = 3) +
    geom_hline(yintercept = 0, colour = "#1a0a0a") +
    scale_y_continuous(
        limits = c(-max(y_try$value), max(y_try$value)),
        breaks = pop_brks,
        labels = pop_labels
    ) +
    scale_fill_manual("Cause of Death", values = cpal) +
    coord_flip() +
    labs(
        x = "Age",
        y = "Deaths in Thousand",
        caption = "Source: Human CoD Database"
    ) +
    theme(
        legend.position = "bottom",
        legend.title = element_blank()
    ) +
    guides(fill = guide_legend(nrow = 4, byrow = TRUE))

age_pyrs
