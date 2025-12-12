library(tidyverse)
library(patchwork)
library(glue)
library(gnm)
library(dlnm)
library(splines)
library(gt)
library(SALURhelper)

# Load data
list_df_city_nbhd <- readRDS("data/list_df_city_nbhd.rds")
df_pop <- readRDS("data/df_pop.rds")

# Study city descriptives table -------------------------------------------

# View potential population years 
df_pop |> 
  count(iso2, year) |> 
  view()

# Brazil's only year is 2010 & this is a mid point for the other LA countries
# The earliest US year is 2012. Other approaches can be explored.
df_psy <- df_pop |> # df population single year
  filter((iso2 %in% c("BR", "GT", "PA") & year == 2010) | (iso2 == "US" & year == 2012))

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
