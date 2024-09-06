# Title: depend_rat_euro
# Date: 2024-09-06
# Purpose: Dependecy Ratio in Europe

# /* cSpell:disable */
# Load libraries --------------

library(dplyr)
library(ggplot2)
library(eurostat)

source("scripts/0_config.R")
source("scripts/0_settings.R")


# Load data --------------
dat_raw <- get_eurostat("demo_pjanbroad", time_format = "num")
