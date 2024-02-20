# Title: network_mig_UK
# Date: 2024-02-15
# Purpose: Network plot of migration
# /* cSpell:disable */

# This code is copied from an online tutorial:
# https://www.ownkng.dev/thoughts/exploring-internal-migration-ggraph


# Libraries --------------
library(tidyverse)
library(geojsonsf)
library(sf)
library(ggraph)
library(igraph)

source("scripts/0_config.R")
source("scripts/0_settings.R")

# data downloaded from ONS.gov.uk
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/migrationwithintheuk/datasets/internalmigrationbyoriginanddestinationlocalauthoritiessexandsingleyearofagedetailedestimatesdataset # nolint
# geodata from: https://geoportal.statistics.gov.uk/datasets/7ceb69f99a024752b97ddac6b0323ab0_0/explore?location=55.215503%2C-3.316939%2C6.98 #nolint

dat1 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_1.csv") # nolint
dat2 <- read.csv(file = "data_download/Detailed_Estimates_2020_LA_2021_Dataset_2.csv") # nolint

migration <- bind_rows(dat1, dat2) %>%
    janitor::clean_names() %>%
    rename(out_la = outla) %>%
    rename(in_la = inla) %>%
    # Counting total number of moves by age
    group_by(out_la, in_la, age) %>%
    summarise(moves = sum(moves) %>% round(0)) %>%
    ungroup()

local_authority_codes <-
    readxl::read_xlsx("data_download/lasregionew2018.xlsx", skip = 4) %>%
    select("la_code" = 1, "la_name" = 2, "region" = 4)

london_codes <- local_authority_codes %>% filter(region == "London")

towns_and_cities <- read_csv("data_download/lauth-classification-csv.csv") %>%
    filter(classification %in% c(
        "Core City (London)", "Core City (outside London)",
        "Other City", "Large Town"
    )) %>%
    filter(percent_of_localauth > 0.7) %>%
    distinct(localauth_code, localauth_name) %>%
    rename("la_code" = localauth_code, "la_name" = localauth_name)

# Merging the towns and cities into the migration data
migration <- migration %>%
    inner_join(local_authority_codes %>% select(la_code, la_name), by = c("in_la" = "la_code")) %>%
    semi_join(towns_and_cities, by = c("in_la" = "la_code")) %>%
    select(-in_la) %>%
    rename("in_la" = la_name) %>%
    inner_join(local_authority_codes %>% select(la_code, la_name), by = c("out_la" = "la_code")) %>%
    semi_join(towns_and_cities, by = c("out_la" = "la_code")) %>%
    select(-out_la) %>%
    rename("out_la" = la_name) %>%
    select(out_la, in_la, age, moves)

moves_to_london <- migration %>%
    filter(in_la %in% london_codes$la_name) %>%
    filter(!(out_la %in% london_codes$la_name)) %>%
    group_by(out_la, age) %>%
    summarise(moves = sum(moves)) %>%
    mutate(in_la = "London")

moves_from_london <- migration %>%
    filter(out_la %in% london_codes$la_name) %>%
    filter(!(in_la %in% london_codes$la_name)) %>%
    group_by(in_la, age) %>%
    summarise(moves = sum(moves)) %>%
    mutate(out_la = "London")

moves_london <- bind_rows(moves_to_london, moves_from_london)

migration <- migration %>%
    filter(!(in_la %in% london_codes$la_name)) %>%
    filter(!(out_la %in% london_codes$la_name)) %>%
    bind_rows(moves_london)


# Creating the nodes data
nodes <- migration %>%
    select(out_la) %>%
    distinct() %>%
    rowid_to_column(var = "id") %>%
    rename("city" = out_la)

# Creating the edges data
edges <- migration %>%
    group_by(out_la, in_la) %>%
    # filtering those records where people are between 23 and 30 years old.
    filter(age %in% c(23:30)) %>%
    summarise(moves = sum(moves)) %>%
    ungroup() %>%
    left_join(nodes, by = c("out_la" = "city")) %>%
    rename("from" = id)

edges <- edges %>%
    left_join(nodes, by = c("in_la" = "city")) %>%
    rename("to" = id)

edges <- edges %>% select(from, to, moves)

# Calculate the total moves into each area, and merging into the migration data
moves_total <- edges %>%
    group_by(to) %>%
    summarise(total_moves = sum(moves)) %>%
    ungroup()

nodes <- nodes %>%
    left_join(moves_total, by = c("id" = "to")) %>%
    left_join(local_authority_codes %>% select(la_name, region), by = c("city" = "la_name"))

# Doing some basic cleaning
nodes[is.na(nodes$region), ]$region <- "London"

nodes <- nodes %>%
    arrange(region, id) %>%
    mutate(city = fct_inorder(city))


#### GGRAPH

migration_net <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

ggraph(migration_net, layout = "kk") +
    geom_edge_link(aes(alpha = moves), show.legend = FALSE) +
    geom_node_point(aes(size = total_moves), show.legend = FALSE) +
    scale_edge_width(range = c(0.2, 1))
