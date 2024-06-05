# Title: Lexis Diagram
# Date: 2024-06-05
# Purpose: Lexis Diagram Sweden
# /* cSpell:disable */

library(dplyr)
library(ggplot2)
library(tidyr)
library(HMDHFDplus)
library(ggthemes)
library(countrycode)

source("scripts/0_config.R")
source("scripts/0_settings.R")


mort <- readHMDweb(
    CNTRY = "SWE",
    "Mx_1x1",
    username = hmd_username,
    password = hmd_password,
    fixup = TRUE
)
