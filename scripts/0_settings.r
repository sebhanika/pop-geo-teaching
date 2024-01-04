# Title: Settings file
# Date: 2024-01-04
# Purpose: This is a settings file that will be tracked by git
# /* cSpell:disable */

library(ggthemes)
options(scipen = 999)

# GGplot Settings  --------------

basic_theme <- theme_set(theme_base() +
    theme(
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 16, color = "grey30"),
        axis.text = element_text(size = 16, color = "grey30"),
        legend.text = element_text(size = 18, color = "grey30"),
        legend.title = element_blank()
    ))

# other code snippets

`%!in%` <- Negate(`%in%`) # function needed for later
