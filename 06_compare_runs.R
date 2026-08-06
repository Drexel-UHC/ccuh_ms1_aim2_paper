library(tidyverse)
library(glue)
library(gt)

# Load helper functions
source("helper_functions.R")

# Comparison deliverable for the staged runs of #18 Phase 3: one table of the p99
# estimates across every run whose CSVs are present in results/

# Read one 02_RRs.R output across all run tags found on disk
read_run_csvs <- function(prefix) {
  files <- list.files(
    "results",
    pattern = glue("^{prefix}_.+\\.csv$"),
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("No results/", prefix, "_*.csv found; run 02_RRs.R first.")
  }

  files |>
    set_names(\(f) basename(f) |> str_remove(glue("^{prefix}_")) |> str_remove("\\.csv$")) |>
    map(\(f) read_csv(f, show_col_types = FALSE)) |>
    list_rbind(names_to = "run")
}

# Estimate with its 95% CI, as reported
fmt_ci <- function(est, low, high, digits = 3) {
  glue(
    "{format_number(est, digits)} ({format_number(low, digits)}, {format_number(high, digits)})"
  )
}

df_overall <- read_run_csvs("RR_p99_overall") |>
  transmute(
    quantity = "RR_p99_overall",
    city_name,
    profile = "overall",
    run,
    value = fmt_ci(RR, RR_low, RR_high)
  )

df_int <- read_run_csvs("RR_p99_interaction") |>
  transmute(
    quantity = "RR_p99_interaction",
    city_name,
    profile = nbhd_type,
    run,
    value = fmt_ci(RR, RR_low, RR_high)
  )

df_ratio <- read_run_csvs("RR_p99_ratio") |>
  pivot_longer(
    c(low_green_low_edu, low_green, low_edu),
    names_to = "profile"
  ) |>
  pivot_wider(names_from = estimate, values_from = value) |>
  transmute(
    quantity = "RR_p99_ratio",
    city_name,
    profile,
    run,
    value = fmt_ci(RR_ratio, RR_ratio_low, RR_ratio_high)
  )

df_compare <-
  bind_rows(df_overall, df_int, df_ratio) |>
  mutate(city_name = city_to_factor(city_name)) |>
  arrange(quantity, city_name, profile, run) |>
  pivot_wider(names_from = run, values_from = value)

write_csv(df_compare, "results/comparison_runs.csv")

df_compare |>
  gt(rowname_col = "profile", groupname_col = c("quantity", "city_name")) |>
  tab_header(title = "RRs at the 99th temperature percentile across staged runs") |>
  gtsave("results/comparison_runs.html")
