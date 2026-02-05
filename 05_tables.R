library(tidyverse)
library(patchwork)
library(glue)
library(stringi)
library(gnm)
library(dlnm)
library(splines)
library(gt)
library(SALURhelper)

# Load helper functions
source("helper_functions.R")

# Load data
list_df_city <- readRDS("data/list_df_city.rds")
list_df_city_nbhd <- readRDS("data/list_df_city_nbhd.rds")
df_pop <- readRDS("data/df_pop.rds")
df_area <- readRDS("data/df_area.rds")

# Study city descriptives table -------------------------------------------

# View potential population years
df_pop |>
  count(iso2, year) |>
  view()

# Brazil's only year is 2010 & this is a mid point for the other LA countries
# The earliest US year is 2012. Other approaches can be explored.
df_psy <- df_pop |> # df population single year
  filter(
    (iso2 %in% c("BR", "GT", "PA") & year == 2010) |
      (iso2 == "US" & year == 2012)
  )

# Bind list_df_city into a single df for quicker calculations
df_city <- list_df_city |> list_rbind()

# Add an "All cities" category for easier total calculations
# df_psy <- bind_rows(df_psy, mutate(df_psy, city_name = "All cities"))
# df_city <- bind_rows(df_city, mutate(df_city, city_name = "All cities"))
# df_area <- bind_rows(df_area, mutate(df_area, city_name = "All cities"))

# Calculate rows for descriptive table

# Total city population
tbl_11 <- df_psy |>
  summarize(pop = format_number(sum(pop) / 1000, 0), .by = city_name)

# Percent over 65 in each city
tbl_12 <- df_psy |>
  filter(age_category == "65+") |>
  summarize(pop_65 = sum(pop), .by = city_name) |>
  left_join(tbl_11, by = "city_name") |>
  mutate(pct_65 = format_number(100 * pop_65 / pop), .keep = "unused")

# Total deaths per year in each city
tbl_13 <- df_city |>
  summarize(death = sum(death), .by = c(city_name, year)) |>
  summarize(death = get_percentiles(death / 1000, 1), .by = city_name)

# Temperature range over entire study period in each city
tbl_14 <- df_city |>
  summarize(popwt_temp = get_percentiles(popwt_temp), .by = city_name)

# Number of distinct neighborhoods by city
tbl_15 <- df_psy |>
  summarize(n_neighborhoods = n_distinct(nbhd_geoid), .by = city_name)

# Neighborhood population by city
tbl_16 <- df_psy |>
  summarize(pop_nbhd = sum(pop), .by = c(city_name, nbhd_geoid)) |>
  summarize(pop_nbhd = get_percentiles(pop_nbhd / 1000, 1), .by = city_name)

# Neighborhood area by city
tbl_17 <- df_area |>
  summarize(nbhd_area = get_percentiles(nbhd_area, 1), .by = city_name)

# Neighborhood percent greenspace & education by city
tbl_18 <- df_city |>
  summarize(
    across(c(pct_green, pct_educated), get_percentiles),
    .by = city_name
  )

# Combine all tables
tbl_1 <-
  list(tbl_11, tbl_12, tbl_13, tbl_14, tbl_15, tbl_16, tbl_17, tbl_18) |>
  reduce(\(x, y) left_join(x, y, by = "city_name"))

# Define row labels
row_labels <- c(
  pop = "Population (thousands)",
  pct_65 = "% 65+",
  death = "Deaths per year (thousands)",
  popwt_temp = "Temperature",
  n_neighborhoods = "Number of neighborhoods",
  pop_nbhd = "Neighborhood population (thousands)",
  nbhd_area = "Neighborhood area",
  pct_green = "% Greenspace",
  pct_educated = "% Educated"
)

# Format descriptive table (cities as columns)
tbl_descriptive <- tbl_1 |>
  data.table::transpose(keep.names = "descriptors", make.names = "city_name") |>
  mutate(
    descriptors = stri_replace_all_fixed(
      descriptors,
      names(row_labels),
      row_labels
    )
  ) |>
  mutate(
    row_groups = rep(
      c("City level characteristics", "Neighborhood level characteristics"),
      c(4, 5)
    )
  ) |>
  gt(
    rowname_col = "descriptors",
    groupname_col = "row_groups",
    row_group_as_column = TRUE
  ) |>
  tab_header(title = "Table 1: Study cities descriptives")

# Format descriptive table (cities as rows)
tbl_descriptive <- tbl_1 |>
  gt(rowname_col = "city_name") |>
  tab_header(title = "Table 1: Study cities descriptives") |>
  tab_spanner(
    label = "City level characteristics",
    columns = c(pop, pct_65, death, popwt_temp)
  ) |>
  tab_spanner(
    label = "Neighborhood level characteristics",
    columns = c(n_neighborhoods, pop_nbhd, nbhd_area, pct_green, pct_educated)
  ) |>
  cols_label(
    pop = "Population (thousands)",
    pct_65 = "% 65+",
    death = "Deaths per year (thousands)",
    popwt_temp = "Temperature",
    n_neighborhoods = "Number of neighborhoods",
    pop_nbhd = "Neighborhood population (thousands)",
    nbhd_area = "Neighborhood area",
    pct_green = "% Greenspace",
    pct_educated = "% Educated"
  )

gtsave(tbl_descriptive, "results/table_descriptive.html")

# Category count table ----------------------------------------------------

tbl_count_nbhd_type <- list_df_city_nbhd |>
  list_rbind(names_to = "city_name") |>
  summarize(count = n(), .by = c(city_name, nbhd_type)) |>
  pivot_wider(values_from = count, names_from = city_name) |>
  arrange(nbhd_type) |>
  gt(rowname_col = "nbhd_type") |>
  tab_header(title = "Count of observed combinations of categories by city") |>
  gtExtras::gt_highlight_rows(rows = c(1, 4)) |>
  tab_footnote(
    footnote = "Expected worst case scenario.",
    locations = cells_stub(rows = c(1))
  ) |>
  tab_footnote(
    footnote = "Expected best case scenario.",
    locations = cells_stub(rows = c(4))
  ) |>
  tab_options(footnotes.marks = "letters")

gtsave(tbl_count_nbhd_type, "results/table_count_nbhd_type.html")

# P75 & P99 by city -------------------------------------------------------

list_df_city |>
  map(\(df) {
    tibble(
      p75 = quantile(df$popwt_temp, .75),
      p99 = quantile(df$popwt_temp, .99)
    )
  })
