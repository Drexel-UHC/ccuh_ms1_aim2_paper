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
source("run_config.R")

# Load data
list_df_city <- readRDS(glue("data/list_df_city_{run_tag}.rds"))
list_df_city_nbhd <- readRDS(glue("data/list_df_city_nbhd_{run_tag}.rds"))
df_pop <- readRDS(glue("data/df_pop_{run_tag}.rds"))
df_RR <- read_csv(glue("results/RR_p99_interaction_{run_tag}.csv"))
df_RR_ratio <- read_csv(glue("results/RR_p99_ratio_{run_tag}.csv"))

# Study city descriptives table -------------------------------------------

# Extract population counts in 2010 from LA coutries and 2012 from US
df_psy <- df_pop |>
  filter(when_any(
    iso2 %in% c("BR", "GT", "PA") & year == 2010,
    iso2 == "US" & year == 2012
  ))

# Bind list_df_city into a single df for quicker calculations; gap-filled days carry
# no death and are not part of the described sample (STAT-MAJ-03)
df_city <- list_df_city |>
  list_rbind() |>
  filter(!is.na(death))

# Calculate rows for descriptive table

# Study period, taken from the analysed dates so the row cannot drift from the data,
# with any interior year that has no data reported rather than hidden (STAT-CRIT-01)
tbl_years <- df_city |>
  summarize(year = list(sort(unique(year))), .by = city_name) |>
  mutate(
    years = map_chr(year, \(y) {
      gap <- setdiff(seq(min(y), max(y)), y)
      paste0(
        min(y),
        "-",
        max(y),
        if (length(gap) == 0) {
          ""
        } else {
          paste0(" [no data: ", paste(gap, collapse = ", "), "]")
        }
      )
    }),
    .keep = "unused"
  )

# Total city population
tbl_11 <- df_psy |>
  summarize(pop = sum(pop), .by = city_name)

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

# Neighorhood level deaths per year in each city
tbl_nhd <- df_city |>
  summarize(death = sum(death), .by = c(city_name, nbhd_geoid, year)) |>
  summarize(nbhd_death = get_percentiles(death, 0), .by = city_name)

# Temperature range over entire study period in each city
tbl_14 <- df_city |>
  summarize(popwt_temp = get_percentiles(popwt_temp), .by = city_name)

# Within city correlation of education and greenspace
tbl_cor <- df_city |>
  summarize(
    spear_cor = format_number(
      cor(pct_green, pct_educated, method = "spearman"),
      digits = 2
    ),
    .by = city_name
  )

# Number of distinct neighborhoods by city
tbl_15 <- df_psy |>
  summarize(n_neighborhoods = n_distinct(nbhd_geoid), .by = city_name)

# Neighborhood population by city
tbl_16 <- df_psy |>
  summarize(pop_nbhd = sum(pop), .by = c(city_name, nbhd_geoid)) |>
  summarize(pop_nbhd = get_percentiles(pop_nbhd / 1000, 1), .by = city_name)

# Neighborhood percent greenspace & education by city
tbl_18 <- df_city |>
  summarize(
    across(c(pct_green, pct_educated), get_percentiles),
    .by = city_name
  )

# Combine all tables, in the order & units the manuscript reports (REPRO-MAJ-01)
tbl_1 <-
  list(
    tbl_years,
    mutate(tbl_11, pop = format_number(pop / 1e6, 1)),
    tbl_12,
    tbl_13,
    tbl_14,
    tbl_cor,
    tbl_15,
    tbl_16,
    tbl_nhd,
    tbl_18
  ) |>
  reduce(\(x, y) left_join(x, y, by = "city_name"))

# Define row labels (order must match the column order of tbl_1)
row_labels <- c(
  years = "Years",
  pop = "Population (millions)",
  pct_65 = "% 65+ years",
  death = "Total deaths per year* (thousands)",
  popwt_temp = "Temperature, daily* (°C)",
  spear_cor = "Greenspace & education correlation coefficient+",
  n_neighborhoods = "Number of neighborhoods",
  pop_nbhd = "Neighborhood population* (thousands)",
  nbhd_death = "Deaths per year*",
  pct_green = "% Greenspace*",
  pct_educated = "% Educational attainment*#"
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
      c("City characteristics", "Neighborhood characteristics"),
      c(6, 5)
    )
  ) |>
  gt(
    rowname_col = "descriptors",
    groupname_col = "row_groups",
    row_group_as_column = TRUE
  ) |>
  tab_header(
    title = "Table 1: Population, mortality, and urban environment characteristics at the city and neighborhood levels for five study cities in the Americas"
  ) |>
  tab_source_note("* Median (10th percentile, 90th percentile)") |>
  tab_source_note(
    "+ Spearman correlation coefficient of neighborhood % greenspace and neighborhood education within each city"
  ) |>
  tab_source_note(
    "# In Philadelphia, this represented % of the population 25+ years who completed a college degree; in Latin America, this represented % population 25 and over who completed secondary education"
  )

gtsave(tbl_descriptive, glue("results/table_descriptive_{run_tag}.html"))


# Supplemental Table S1: category counts -----------------------------------

tbl_count_nbhd_type <- list_df_city_nbhd |>
  list_rbind(names_to = "city_name") |>
  summarize(count = n(), .by = c(city_name, nbhd_type)) |>
  pivot_wider(values_from = count, names_from = city_name) |>
  arrange(nbhd_type) |>
  gt(rowname_col = "nbhd_type") |>
  tab_header(
    title = "Supplemental Table S1: Neighborhood counts for each two-way greenspace/education profile by city"
  ) |>
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

gtsave(tbl_count_nbhd_type, glue("results/table_S1_count_nbhd_type_{run_tag}.html"))


# Supplemental Table S2: RRs by neighborhood profile -----------------------

# Formatted here rather than hand-rounded from the CSV (STAT-CRIT-01)
table_RR_nbhd_type <- df_RR |>
  mutate(
    city_name = city_to_factor(city_name),
    nbhd_type = convert_to_label(nbhd_type, label_nbhd_type),
    across(c(RR, RR_low, RR_high), \(x) format_number(x, digits = 3))
  ) |>
  arrange(city_name, nbhd_type) |>
  select(city_name, nbhd_type, RR, RR_low, RR_high) |>
  gt(rowname_col = "nbhd_type", groupname_col = "city_name") |>
  cols_label(
    RR = "Relative Risk",
    RR_low = "95% CI lower limit",
    RR_high = "95% CI upper limit"
  ) |>
  tab_header(
    title = "Supplemental Table S2: Effect estimates for neighborhood profiles (Figure 2 data)"
  )

gtsave(table_RR_nbhd_type, glue("results/table_S2_RR_nbhd_type_{run_tag}.html"))


# Supplemental Table S3: ratio of RRs --------------------------------------

table_RR_ratio <- df_RR_ratio |>
  pivot_wider(
    names_from = estimate,
    values_from = c(low_edu, low_green, low_green_low_edu)
  ) |>
  mutate(across(-city_name, \(x) format_number(x, digits = 3))) |>
  mutate(
    low_edu = glue(
      "{low_edu_RR_ratio} ({low_edu_RR_ratio_low}, {low_edu_RR_ratio_high})"
    ),
    low_green = glue(
      "{low_green_RR_ratio} ({low_green_RR_ratio_low}, {low_green_RR_ratio_high})"
    ),
    low_green_low_edu = glue(
      "{low_green_low_edu_RR_ratio} ({low_green_low_edu_RR_ratio_low}, {low_green_low_edu_RR_ratio_high})"
    ),
    .keep = "unused"
  ) |>
  gt(rowname_col = "city_name") |>
  cols_label(
    low_edu = "Low edu",
    low_green = "Low green",
    low_green_low_edu = "Low green/low edu"
  ) |>
  tab_header(
    title = "Supplemental Table S3: Effect estimates for neighborhood profiles (Figure 3 data)"
  )

gtsave(table_RR_ratio, glue("results/table_S3_RR_ratio_{run_tag}.html"))


# P75 & P99 by city -------------------------------------------------------

df_city |>
  summarize(
    p75 = quantile(popwt_temp, .75),
    p99 = quantile(popwt_temp, .99),
    .by = city_name
  ) |>
  write_csv(glue("results/p75_p99_by_city_{run_tag}.csv"))

# Total number of deaths -------------------------------------------------

df_city |>
  summarize(total_deaths = sum(death), .by = city_name) |>
  mutate(city_name = as.character(city_name)) %>%
  add_row(city_name = "All", total_deaths = sum(.$total_deaths)) |>
  write_csv(glue("results/total_deaths_{run_tag}.csv"))
