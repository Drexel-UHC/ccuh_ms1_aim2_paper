library(tidyverse)
library(glue)


# Import and format data --------------------------------------------------

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


# Add neighborhood type (1 factor of 4 levels) ----------------------------

# Define possible neighborhood types
names_nbhd_type <- c("low_green_low_edu",
                     "low_green_high_edu",
                     "high_green_low_edu", 
                     "high_green_high_edu")

# Define neighborhoods by type per city
list_city_df <- list_city_df |> 
  map(\(df_city) {
    # Cutoff values for nbhd definitions
    cutoff_grn <- median(df_city$pct_green)
    cutoff_edu <- median(df_city$pct_educated)
    
    # Create nbhd_type factor
    df_city <- df_city |> 
      mutate(nbhd_type = case_when(
        pct_green < cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[1],
        pct_green < cutoff_grn & pct_educated >=cutoff_edu ~ names_nbhd_type[2],
        pct_green >=cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[3],
        pct_green >=cutoff_grn & pct_educated >=cutoff_edu ~ names_nbhd_type[4],
        .default = "None") |> factor(levels = names_nbhd_type))
    
    # Create df of dummy variables
    df_dummy_vars <- model.matrix(~ nbhd_type + 0, data = df_city) |> 
      as_tibble() |> 
      rename_with(\(x) str_remove(x, "nbhd_type"))
    
    # Return new df with factor & dummy variables
    bind_cols(df_city, df_dummy_vars)
  })


# Save final list of data frames ------------------------------------------

# Save formatted list of dfs
saveRDS(list_city_df, "data/list_city_df.rds")
