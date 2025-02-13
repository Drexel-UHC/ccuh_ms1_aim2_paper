library(tidyverse)

context = lst(
  server = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_manuscripts/ccuh-server-MS1/v1.0",
  dbt = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/dev",
  ## Component Files
  df_pct_green =    file.path(server,'df_pct_green.parquet'),
  df_pct_primary =    file.path(server,'df_pct_primary.parquet'),
  df_nbhd_pop_dens =    file.path(server,'df_nbhd_pop_dens.parquet'),
  df_mort = file.path(server, 'df_mort.parquet'),
  df_mort_age = file.path(server, 'df_mort_age.parquet'),
  df_city_temp = file.path(server, 'df_city_temp.parquet'),
  ## Final Files
  df_subcity_daily = file.path(server, 'df_subcity_daily.parquet'),
  df_subcity_daily_age = file.path(server, 'df_subcity_daily.parquet'),
  df_nbhd_cross_sectional = file.path(server, 'df_nbhd_cross_sectional.parquet')
)
 

## Crosswalks
xwalk_tract10_dsphNbhd = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/xwalk_tract10_dsphNbhd__v1/xwalk_tract10_dsphNbhd__v1.parquet" |> 
  arrow::open_dataset() %>% 
  dplyr::collect() %>% 
  select(tract10, subcity_geoid = nbhd_id) %>% 
  mutate( subcity_geo = 'DSPH_NBHD')
xwalk_l2 = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/xwalk_l2_v1/xwalk_l2_v1.parquet" |> 
  arrow::open_dataset() |>
  dplyr::collect()

## SALURBAL Cities of interest
city_IDs <-  c(`Belo Horizonte` = 102136,
               `Rio de Janeiro` = 102190,
               `Campinas`       = 102226,
               `Sao Paulo`      = 102248,
               `Guatemala City` = 203102,
               `Panama City`    = 206101)

 