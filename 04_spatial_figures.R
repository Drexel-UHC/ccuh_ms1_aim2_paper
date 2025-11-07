library(tidyverse)
library(patchwork)
library(glue)
library(SALURhelper)

# Spatial Libraries
library(arrow)
library(geoarrow)
library(sf)
library(leaflet)

# ggplot2 theme
set_theme(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             legend.title = element_text(hjust = 0.5))

# Load helper functions
source("helper_functions.R")

# Load data of nbhd_type for each nbhd_geoid
list_df_city_nbhd <- readRDS("data/list_df_city_nbhd.rds")


# Extract spatial boundaries ----------------------------------------------

# Pennsylvania
phl_sf <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze//schema_ccuh_phl_metro_nbhd/schema_ccuh_phl_metro_nbhd__nad83.parquet" %>%
  arrow::open_dataset() %>% 
  sf::st_as_sf()

# SALURBAL Cities 
df_salurbal_nbhd <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/salurbal_boundaries__v1/salurbal_boundaries_v1.parquet" %>% 
  arrow::open_dataset() |> 
  sf::st_as_sf()

# Create city specific sf object
rio_sf <- df_salurbal_nbhd |> filter(str_detect(geoid, "102190"), geo == "l2_5") |> mutate(nbhd_geoid = geoid)
sao_sf <- df_salurbal_nbhd |> filter(str_detect(geoid, "102248"), geo == "l2_5") |> mutate(nbhd_geoid = geoid)
gua_sf <- df_salurbal_nbhd |> filter(str_detect(geoid, "203102"), geo == "l2")   |> mutate(nbhd_geoid = geoid)
pan_sf <- df_salurbal_nbhd |> filter(str_detect(geoid, "206101"), geo == "l2")   |> mutate(nbhd_geoid = geoid)

# Bind into list
list_sf <- list(gua_sf, pan_sf, phl_sf, rio_sf, sao_sf)


# Create spatial plots ----------------------------------------------------

list_plot_spatial <- 
  pmap(list(sf = list_sf,
            df = list_df_city_nbhd,
            city_name = names(list_df_city_nbhd)), 
       \(sf, df, city_name) {
         sf |> 
           left_join(df, by = "nbhd_geoid") |> 
           ggplot(aes(fill = nbhd_type)) +
           geom_sf() +
           scale_fill_brewer(type = "qual", drop = FALSE) +
           theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
           labs(title = city_name, fill = "Neighborhood Type")
       })

wrap_plots(list_plot_spatial, guides = "collect", heights = 1, widths = 1)

ggsave("results/figures_spatial.pdf", units = "cm", width = 30, height = 15)
