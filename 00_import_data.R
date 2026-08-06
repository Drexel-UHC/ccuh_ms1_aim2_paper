library(tidyverse)
library(glue)

# Spatial Libraries
library(sf)
library(geoarrow)

source("helper_functions.R")
source("run_config.R")


# Import & format daily data ----------------------------------------------

# Load daily data
df_data_age <- file.path(dir_bundle, "ccuh_ms1__final_bundle_age__v1.parquet") |>
  arrow::open_dataset() |>
  dplyr::collect() |>
  # Mortality now runs to 2022 but LATAM city temperature ends 2020-12-31
  filter(!is.na(popwt_temp)) |>
  # Belo Horizonte & Campinas are in the bundle only because WRI-specific marts did
  # not exist when it was built; they are not study cities (STAT-MAJ-04)
  filter_out(city_name %in% c("Belo Horizonte", "Campinas")) |>
  mutate(city_name = city_to_factor(city_name))

# City-date temperature, taken before the year filter so gap-filled days keep their
# temperature and the days following a gap stay usable (STAT-MAJ-03)
df_city_temp <- df_data_age |>
  summarize(popwt_temp = first(popwt_temp), .by = c(city_name, date))

# Restrict to this run's per-city year window (#18 Phase 3)
if (!is.null(run_years)) {
  df_data_age <- df_data_age |>
    semi_join(
      run_years |>
        enframe(name = "city_name", value = "year") |>
        unnest(year) |>
        mutate(city_name = city_to_factor(city_name)),
      by = c("city_name", "year")
    )
}

# Create list of data frames by city
list_df_city <-
  df_data_age %>%
  split(.$city_name) |>
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
          pct_green < cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[
            1
          ],
          pct_green < cutoff_grn & pct_educated >= cutoff_edu ~ names_nbhd_type[
            2
          ],
          pct_green >= cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[
            3
          ],
          pct_green >= cutoff_grn &
            pct_educated >= cutoff_edu ~ names_nbhd_type[4],
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
      summarize(
        across(contains(c("pct_", "nbhd_type")), first),
        .by = nbhd_geoid
      ) |>
      mutate(nbhd_type = convert_to_label(nbhd_type, label_nbhd_type))
  })


# Fill gaps in the daily series -------------------------------------------

# Complete every nbhd:age series with NA deaths, keeping the day's temperature, so
# the cross-basis never lags across a missing stretch (STAT-MAJ-03)
list_df_city <- list_df_city |>
  map(\(df_city) {
    df_city |>
      select(-popwt_temp) |>
      group_by(group) |>
      complete(date = full_seq(date, 1)) |>
      ungroup() |>
      # These columns are constant within a nbhd:age series
      mutate(
        across(
          c(
            city_name,
            nbhd_geoid,
            age_category,
            pct_green,
            pct_educated,
            nbhd_type,
            all_of(names_nbhd_type)
          ),
          \(x) first(na.omit(x))
        ),
        .by = group
      ) |>
      left_join(df_city_temp, by = c("city_name", "date")) |>
      mutate(
        year = as.integer(year(date)),
        strata = glue(
          "{nbhd_geoid}:{age_category}:{year(date)}:{month(date)}:{wday(date, label = TRUE)}"
        ) |>
          factor()
      ) |>
      arrange(across(c(nbhd_geoid, date, age_category)))
  })

# Regression test for the failure mode above: every series must be complete, ordered
# and equally spaced by day before it reaches the cross-basis (STAT-MAJ-03)
df_series_check <- list_df_city |>
  map(\(df_city) {
    df_city |>
      summarize(daily = all(diff(date) == 1), .by = group) |>
      filter(!daily)
  }) |>
  list_rbind(names_to = "city_name")

if (nrow(df_series_check) > 0) {
  stop(
    "Non-contiguous daily series after gap filling in: ",
    paste(unique(df_series_check$city_name), collapse = ", "),
    " (", nrow(df_series_check), " nbhd:age groups)"
  )
}


# Import & format population data -----------------------------------------

# Load population counts
df_aim2_pop_age <- dir_bundle |>
  file.path("harmonized__ccuh_aim_2__nbhd_pop_age_5_yr_counts__v1.parquet") |>
  arrow::open_dataset() |>
  dplyr::collect()

# Get city name for each neighborhood, from the whole bundle so that neighborhoods
# dropped from the daily data above still resolve to a city here
df_city_name <- dir_bundle |>
  file.path("ccuh_ms1__final_bundle_age__v1.parquet") |>
  arrow::open_dataset() |>
  select(nbhd_geoid, city_name) |>
  distinct() |>
  dplyr::collect()

# Create pop count dataframe compatible with daily data
ages_under_65 <- paste0(seq(0, 60, by = 5), "-", seq(4, 64, by = 5))
ages_over_65 <- paste0(seq(65, 75, by = 5), "-", seq(69, 79, by = 5)) |>
  c("80+")

df_pop <- df_aim2_pop_age |>
  mutate(
    age_category = fct_collapse(
      age_range,
      `<65` = ages_under_65,
      `65+` = ages_over_65
    ),
    nbhd_geoid = factor(nbhd_geoid)
  ) |>
  summarize(
    pop = sum(pop_age),
    .by = c(iso2, nbhd_geoid, year, age_category)
  ) |>
  left_join(df_city_name, by = "nbhd_geoid") |>
  # Not study cities, see above (STAT-MAJ-04)
  filter_out(city_name %in% c("Belo Horizonte", "Campinas")) |>
  mutate(city_name = city_to_factor(city_name)) |>
  arrange(across(c(city_name, nbhd_geoid, year, age_category)))


# Import & format nbhd area data ------------------------------------------

# Import neighborhood spatial data
sf_aim2_nbhd <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/schema_ccuh_aim2_city_nbhd/sf_nbhd__schema_ccuh_aim2_city_nbhd.parquet" |>
  arrow::open_dataset() |>
  sf::st_as_sf()

# Compute nbhd area, excluding the two nbhds whose geometries are invalid
# (São Paulo & Rio); matched on geoid, not row index, because the source is
# reordered by data refreshes (REPRO-MIN-04)
df_area <- sf_aim2_nbhd |>
  filter_out(nbhd_geoid %in% c("10224823123", "10219010150")) |>
  # Not study cities, see above (STAT-MAJ-04)
  filter_out(city_name %in% c("Belo Horizonte", "Campinas")) |>
  mutate(
    nbhd_area = as.numeric(units::set_units(st_area(geometry), km^2)),
    city_name = city_to_factor(city_name)
  ) |>
  as_tibble() |>
  select(c(city_name, nbhd_geoid, nbhd_area)) |>
  arrange(across(c(city_name, nbhd_geoid)))


# Save formatted data -----------------------------------------------------

# Save formatted list of dfs
saveRDS(list_df_city, glue("data/list_df_city_{run_tag}.rds"))
saveRDS(list_df_city_nbhd, glue("data/list_df_city_nbhd_{run_tag}.rds"))

# Save population df
saveRDS(df_pop, glue("data/df_pop_{run_tag}.rds"))
saveRDS(df_area, glue("data/df_area_{run_tag}.rds"))
