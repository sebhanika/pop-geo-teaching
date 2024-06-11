# Title: Lexis Diagram
# Date: 2024-06-05
# Purpose: Lexis Diagram Sweden
# /* cSpell:disable */

# Libraries --------------

library(ggplot2)
library(HMDHFDplus)
library(ggthemes)
library(janitor)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Load data --------------

# get country list
hmd_cntr <- c(
    "SWE", "FRATNP", "NLD", "DNK", "ITA",
    "FIN", "GBRTENW", "CHE", "ESP"
)
dat <- list()

for (i in seq_along(hmd_cntr)) {
    dat[[i]] <- readHMDweb(
        CNTRY = hmd_cntr[i],
        "Mx_1x1",
        username = hmd_username,
        password = hmd_password,
        fixup = TRUE
    )

    # clean names for easier handling
    dat[[i]] <- clean_names(dat[[i]])

    dat[[i]]$rat <- dat[[i]]$male / dat[[i]]$female

    dat[[i]] <- subset(dat[[i]],
        year %in% 1900:2023 & age < 101,
        select = c(year, age, rat)
    )

    dat[[i]]$cntry <- hmd_cntr[i]
}


# Prep data --------------

dat_comb <- do.call(rbind, dat)
dat_comb <- subset(dat_comb, !is.na(dat_comb$rat))

dat_comb$cntry_name <- sapply(dat_comb$cntry, switch,
    SWE = "Sweden",
    FRATNP = "France",
    DNK = "Denmark",
    NLD = "Netherlands",
    ITA = "Italy",
    FIN = "Finland",
    GBRTENW = "England & Wales",
    CHE = "Switzerland",
    ESP = "Spain"
)

# cut data into bins for percentage
dat_comb$rat_group <- cut(dat_comb$rat,
    breaks = c(
        -Inf, (1 / 2), (1 / 1.75), (1 / 1.5), (1 / 1.25), (1 / 1.02),
        1.02, 1.25, 1.5, 1.75, 2, Inf
    )
)


# Labels and colors for grpahs --------------

# cutting groups
grp_rat <- c(
    "(-Inf,0.5]", "(0.5,0.571]", "(0.571,0.667]", "(0.667,0.8]", "(0.8,0.98]",
    "(0.98,1.02]",
    "(1.02,1.25]", "(1.25,1.5]", "(1.5,1.75]", "(1.75,2]", "(2, Inf]"
)

# labels for legend
grp_labs <- c(
    ">100% excess female mortality", "75 to 100%", "50 to 75%", "25 to 50%", "2 to 25%",
    "~ Equal mortality",
    "2 to 25%", "25 to 50%", "50to 75%", "75 to 100%", ">100% excess male mortality"
)

# colors for graphs
colors_rat <- c(
    "#67001f",
    "#b2182b",
    "#d6604d",
    "#f4a582",
    "#fddbc7",
    "#f7f7f7",
    "#d1e5f0",
    "#92c5de",
    "#4393c3",
    "#2166ac",
    "#053061"
)

# Combine as named vectors
nv_grp_labs <- setNames(grp_labs, grp_rat)
nv_grp_cols <- setNames(colors_rat, grp_rat)

# Set levels order for facet_plot
dat_comb$cntry_name <- factor(dat_comb$cntry_name,
    levels = c(
        "Denmark", "Finland", "Sweden",
        "France", "Italy", "Spain",
        "England & Wales", "Netherlands", "Switzerland"
    )
)


# Individal plots --------------

p_list <- list()

for (i in hmd_cntr) {
    df_cntry <- subset(dat_comb, cntry == i)

    p_list[[i]] <- df_cntry |>
        ggplot(aes(x = year, y = age, fill = rat_group)) +
        geom_tile() +
        geom_abline(
            slope = 1, intercept = seq(-1800, -2020, by = -10),
            linetype = 3, color = "#666666"
        ) +
        coord_fixed() +
        scale_fill_manual(
            "Male-female mortality ratio",
            values = nv_grp_cols,
            labels = nv_grp_labs
        ) +
        scale_x_continuous(
            limits = c(1899, 2024),
            breaks = seq(1900, 2023, 20),
            expand = c(0, 0)
        ) +
        scale_y_continuous(
            limits = c(-1, 101),
            breaks = seq(0, 100, 20),
            expand = c(0, 0)
        ) +
        labs(
            x = "Year",
            y = "Age",
            title = paste("Lexis-Diagram Male-female mortality ratio -", unique(df_cntry$cntry_name))
        ) +
        theme(
            axis.ticks = element_blank(),
            panel.grid = element_line(linetype = 3, color = "#666666"),
            panel.ontop = TRUE,
            axis.line = element_blank(),
            rect = element_blank(),
            panel.border = element_blank(),
            line = element_blank(),
            legend.text = element_text(size = 14),
            axis.title.x = element_text(margin = margin(t = 5))
        )
}

# save all plots in list as pngs
invisible(
    lapply(
        seq_along(p_list),
        function(x) {
            ggsave(
                filename = paste0("viszs/lexis/plot_", names(p_list[x]), ".png"),
                plot = p_list[[x]],
                bg = "white",
                width = 32,
                height = 19,
                unit = "cm"
            )
        }
    )
)


# Combined plots --------------

p_comb <- dat_comb |>
    ggplot(aes(x = year, y = age, fill = rat_group)) +
    geom_tile() +
    geom_abline(
        slope = 1, intercept = seq(-1800, -2020, by = -10),
        linetype = 3, color = "#666666"
    ) +
    facet_wrap(~cntry_name) +
    coord_fixed() +
    scale_fill_manual(
        "Male-female mortality ratio",
        values = nv_grp_cols,
        labels = nv_grp_labs
    ) +
    scale_x_continuous(
        limits = c(1899, 2024),
        breaks = seq(1910, 2023, 20),
        expand = c(0, 0)
    ) +
    scale_y_continuous(
        limits = c(-1, 101),
        breaks = seq(0, 100, 20),
        expand = c(0, 0)
    ) +
    labs(
        x = "Year",
        y = "Age",
        title = "Lexis-Diagram Male-female mortality ratio - selected countries"
    ) +
    theme(
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_line(linetype = 3, color = "#666666"),
        panel.ontop = TRUE,
        rect = element_blank(),
        panel.border = element_blank(),
        line = element_blank(),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 5)),
        plot.background = element_rect(fill = "white"),
        strip.text = element_text(size = 16)
    )

ggsave("viszs/lexis/combined_plot.png", p_comb,
    width = 50, height = 40, units = "cm"
)
