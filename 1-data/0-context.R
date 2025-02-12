library(tidyverse)

context = lst(
  server = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_manuscripts/ccuh-server-MS1/v1.0",
  dbt = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/dev",
  ## Files
  df_pct_green =    file.path(server,'df_pct_green.parquet'),
  df_pct_primary =    file.path(server,'df_pct_primary.parquet'),
  df_nbhd_pop_dens =    file.path(server,'df_nbhd_pop_dens.parquet'),
)
 

## Philadelphia tracts
xwalk_tract10 = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/xwalk_tract10_v1/xwalk_tract10_v1.parquet" |> 
  arrow::read_parquet()
vec__philadelphia_tracts = xwalk_tract10 %>% 
  filter( metro.census.cbsa10.name == 'Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metropolitan Statistical Area',
          state.abb == 'PA') %>% 
  pull(tract10.census.geoid)
