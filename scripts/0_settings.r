# Title: Settings file
# Date: 2023-09-22
# Purpose: This is a settings file that will be tracked by git


# GGplot Settings  --------------
basic_theme <- theme_set(theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title = element_text(size = 16, color = "grey30"),
        axis.text = element_text(size = 16, color = "grey30"),
        legend.text = element_text(size = 18, color = "grey30"),
        legend.title = element_blank()
    ))
