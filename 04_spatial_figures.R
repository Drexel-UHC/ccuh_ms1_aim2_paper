library(tidyverse)
library(patchwork)
library(glue)
library(SALURhelper)

# Spatial Libraries
library(sf)
library(geoarrow)

# ggplot2 theme
set_theme(theme_bw())
theme_update(
  plot.title = element_text(size = 14, hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  legend.title = element_text(size = 14, hjust = 0.5),
  legend.key.height = unit(1.2, "cm"),
  legend.text = element_text(size = 12),
  axis.text.x = element_text(size = 12, hjust = .2),
  axis.text.y = element_text(size = 12)
)

color_pal <- rev(RColorBrewer::brewer.pal(4, "BrBG"))

# Load helper functions
source("helper_functions.R")

# Load data of nbhd_type for each nbhd_geoid
list_df_city_nbhd <- readRDS("data/list_df_city_nbhd.rds")


# Extract spatial boundaries ----------------------------------------------

# Pennsylvania
phl_sf <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze//schema_ccuh_phl_metro_nbhd/schema_ccuh_phl_metro_nbhd__nad83.parquet" |>
  arrow::open_dataset() |>
  sf::st_as_sf()

# SALURBAL Cities
df_salurbal_nbhd <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/salurbal_boundaries__v1/salurbal_boundaries_v1.parquet" |>
  arrow::open_dataset() |>
  sf::st_as_sf()

# Create city specific sf object
rio_sf <- df_salurbal_nbhd |>
  filter(str_detect(geoid, "102190"), geo == "l2_5") |>
  mutate(nbhd_geoid = geoid)
sao_sf <- df_salurbal_nbhd |>
  filter(str_detect(geoid, "102248"), geo == "l2_5") |>
  mutate(nbhd_geoid = geoid)
gua_sf <- df_salurbal_nbhd |>
  filter(str_detect(geoid, "203102"), geo == "l2") |>
  mutate(nbhd_geoid = geoid)
pan_sf <- df_salurbal_nbhd |>
  filter(str_detect(geoid, "206101"), geo == "l2") |>
  mutate(nbhd_geoid = geoid)

# Bind into list
list_sf <- list(phl_sf, rio_sf, sao_sf, pan_sf, gua_sf)


# Spatial plots specification ---------------------------------------------

create_list_plot_spatial <- function(fill_var = "nbhd_type") {
  # Create fill label for legend
  fill_lab <- case_when(
    fill_var == "nbhd_type" ~ "Neighborhood Type",
    fill_var == "pct_green" ~ "% Greenspace",
    fill_var == "pct_educated" ~ "% Educated"
  )

  # Map over each city
  pmap(
    list(
      sf = list_sf,
      df = list_df_city_nbhd,
      city_name = names(list_df_city_nbhd)
    ),
    \(sf, df, city_name) {
      # Create plot with spatial boundaries
      sf |>
        left_join(df, by = "nbhd_geoid") |>
        ggplot(aes(fill = .data[[fill_var]])) +
        geom_sf() +
        scale_x_continuous(n.breaks = 4) +
        scale_y_continuous(n.breaks = 4) +
        # Check if fill scale is discrete or continuous
        {
          if (fill_var == "nbhd_type") {
            scale_fill_manual(values = rev(color_pal))
          } else {
            scale_fill_viridis_c(limits = c(0, 100))
          }
        } +
        labs(title = city_name, fill = fill_lab)
    }
  )
}

# Specify plot layout
design <- "ABB
           CDE"

widths <- c(1, 1, 1)
heights <- c(1, 1)

# Specify size of saved figure
f_width <- 30
f_height <- 15


# Spatial plots of neighborhood type --------------------------------------

# Render plots
wrap_plots(
  create_list_plot_spatial("nbhd_type"),
  widths = widths,
  heights = heights,
  design = design,
  guides = "collect"
)

# Save plots
ggsave(
  "results/figures_spatial.pdf",
  units = "cm",
  width = f_width,
  height = f_height
)


# Spatial plots of continuous variables -----------------------------------

# Render plots
wrap_plots(
  create_list_plot_spatial("pct_green"),
  widths = widths,
  heights = heights,
  design = design,
  guides = "collect"
)

# Save plots
ggsave(
  "sup_results/figures_spatial_green.pdf",
  units = "cm",
  width = f_width,
  height = f_height
)

# Render plots
wrap_plots(
  create_list_plot_spatial("pct_educated"),
  widths = widths,
  heights = heights,
  design = design,
  guides = "collect"
)

# Save plots
ggsave(
  "sup_results/figures_spatial_edu.pdf",
  units = "cm",
  width = f_width,
  height = f_height
)
