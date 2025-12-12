library(tidyverse)
library(glue)

source("helper_functions.R")


# Import & format daily data ----------------------------------------------

# Load daily data
df_data_age = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/v_0_1_0/ccuh_ms1__final_bundle_age__v1.parquet" |>
  arrow::open_dataset() |>
  dplyr::collect()

# Create list of data frames by city
list_df_city <-
  df_data_age |>
  split(df_data_age$city_name) |>
  discard_at(c("Belo Horizonte", "Campinas")) |>
  map(\(df_city) {
    df_city |>
      mutate(
        strata = glue(
          "{nbhd_geoid}:{age_category}:{year(date)}:{month(date)}:{wday(date, label = TRUE)}"
        ),
        group = glue("{nbhd_geoid}:{age_category}"),
        across(c(nbhd_geoid, strata, group, age_category), factor)
      ) |>
      arrange(across(c(nbhd_geoid, date, age_category)))
  })


# Add neighborhood type (1 factor of 4 levels) ----------------------------

# Define possible neighborhood types
names_nbhd_type <- c(
  "low_green_low_edu",
  "low_green_high_edu",
  "high_green_low_edu",
  "high_green_high_edu"
)

# Define neighborhoods by type per city
list_df_city <- list_df_city |>
  map(\(df_city) {
    # Cutoff values for nbhd definitions
    cutoff_grn <- median(df_city$pct_green)
    cutoff_edu <- median(df_city$pct_educated)

    # Create nbhd_type factor
    df_city <- df_city |>
      mutate(
        nbhd_type = case_when(
          pct_green < cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[1],
          pct_green < cutoff_grn & pct_educated >= cutoff_edu ~ names_nbhd_type[2],
          pct_green >= cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[3],
          pct_green >= cutoff_grn & pct_educated >= cutoff_edu ~ names_nbhd_type[4],
          .default = "None"
        ) |>
          factor(levels = names_nbhd_type)
      )

    # Create df of dummy variables
    df_dummy_vars <- model.matrix(~ nbhd_type + 0, data = df_city) |>
      as_tibble() |>
      rename_with(\(x) str_remove(x, "nbhd_type"))

    # Return new df with factor & dummy variables
    bind_cols(df_city, df_dummy_vars)
  })

# Create list of dfs of nbhd types with 1 row per nbhd
list_df_city_nbhd <-
  list_df_city |>
  map(\(df_city) {
    df_city |>
      summarize(across(contains(c("pct_", "nbhd_type")), first), .by = nbhd_geoid) |>
      mutate(nbhd_type = convert_to_label(nbhd_type, label_nbhd_type))
  })


# Import & format population data -----------------------------------------

# Load population counts
df_aim2_pop_age = '//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/v_0_1_1/harmonized__ccuh_aim_2__nbhd_pop_age_5_yr_counts__v1.parquet' |> 
  arrow::open_dataset() |> 
  dplyr::collect()

# Get city name for each neighborhood
df_city_name <- df_data_age |> 
  summarize(across(c(city_name), first), .by = nbhd_geoid)

# Create pop count dataframe compatible with daily data
ages_under_65 <- paste0(seq(0, 60, by = 5),"-", seq(4, 64, by = 5))
ages_over_65 <- paste0(seq(65, 75, by = 5),"-", seq(69, 79, by = 5)) |> c("80+")

df_pop <- df_aim2_pop_age |> 
  mutate(age_category = 
           fct_collapse(age_range,
                        `<65` = ages_under_65, 
                        `65+` = ages_over_65),
         nbhd_geoid = factor(nbhd_geoid)) |> 
  summarize(pop = sum(pop_age), .by = c(iso2, nbhd_geoid, year, age_category)) |> 
  arrange(across(c(nbhd_geoid, year, age_category))) |> 
  left_join(df_city_name, by = "nbhd_geoid") |> 
  filter(!(city_name %in% c("Belo Horizonte", "Campinas")))


# Import & format nbhd area data ------------------------------------------

# Import neighborhood spatial data
sf_aim2_nbhd = "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/schema_ccuh_aim2_city_nbhd/sf_nbhd__schema_ccuh_aim2_city_nbhd.parquet" %>% 
  arrow::open_dataset() %>% 
  sf::st_as_sf() 

# Problem nbhd indexes: 609, 961
for(i in 1:nrow(sf_aim2_nbhd)) {
  print(i)
  sf_aim2_nbhd$geometry[i] |> st_area()
}

# Problem nbhd_geoid: 10224823123, 10219010150
sf_aim2_nbhd$nbhd_geoid[c(609, 961)]

# Compute nbhd area exluding problem nbhds for now
df_area <- sf_aim2_nbhd[-c(609, 961), ] |> 
  mutate(nbhd_area = st_area(geometry)) |> 
  as_tibble() |> 
  select(c(city_name, nbhd_geoid, nbhd_area)) |> 
  arrange(across(c(city_name, nbhd_geoid)))


# Save formatted data -----------------------------------------------------

# Save formatted list of dfs
saveRDS(list_df_city, "data/list_df_city.rds")
saveRDS(list_df_city_nbhd, "data/list_df_city_nbhd.rds")

# Save population df
saveRDS(df_pop, "data/df_pop.rds")
saveRDS(df_area, "data/df_area.rds")
