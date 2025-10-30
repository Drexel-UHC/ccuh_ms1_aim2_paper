library(tidyverse)

# Load data
df_data_age = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/v_0_1_0/ccuh_ms1__final_bundle_age__v1.parquet" |> 
  arrow::open_dataset() |>  
  dplyr::collect()

# Create list of data frames by city
list_df <- 
  df_data_age |> 
  split(df_data_age$city_name) |>
  discard_at(c("Belo Horizonte", "Campinas")) |>
  map(\(x) {
    mutate(x, across(c(age_category, nbhd_geoid, city_geoid, iso2), factor))
  })

# Save formatted list of dfs
saveRDS(list_df, "data/list_city_df.rds")
