library(tidyverse)
library(glue)

# Load data
df_data_age = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/v_0_1_0/ccuh_ms1__final_bundle_age__v1.parquet" |> 
  arrow::open_dataset() |>  
  dplyr::collect()

# Create list of data frames by city
list_city_df <- 
  df_data_age |> 
  split(df_data_age$city_name) |>
  discard_at(c("Belo Horizonte", "Campinas")) |>
  map(\(df_city) {
    df_city |> 
      mutate(strata = glue("{nbhd_geoid}:{age_category}:{year(date)}:{month(date)}:{wday(date, label = TRUE)}"),
             group = glue("{nbhd_geoid}:{age_category}"),
             across(c(nbhd_geoid, strata, group, age_category), factor)) |> 
      arrange(across(c(nbhd_geoid, date, age_category)))
  })

# Save formatted list of dfs
saveRDS(list_city_df, "data/list_city_df.rds")
