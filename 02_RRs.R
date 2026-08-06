library(tidyverse)
library(glue)
library(gnm)
library(dlnm)
library(splines)
library(SALURhelper)

# Load helper functions
source("helper_functions.R")
source("run_config.R")

# Load models
list_model_city <- readRDS(glue("models/list_model_city_{run_tag}.rds"))
list_model_city_int <- readRDS(glue("models/list_model_city_int_{run_tag}.rds"))

# Predictions for overall curve -------------------------------------------

# Get overall RRs at 99th percentile for each city
df_pred <- list_model_city |>
  map(\(model_city) {
    # Estimate RRs at 99th temperature percentile
    city_pred <-
      crossreduce(
        model_city$cbt,
        coef = model_city$coef,
        vcov = model_city$vcov,
        model.link = model_city$link,
        at = model_city$p99,
        cen = model_city$p75
      ) # Can change between p75 and MMT

    # Format df to return
    get_df_RRs(city_pred)
  }) |>
  list_rbind(names_to = "city_name")

write_csv(df_pred, glue("results/RR_p99_overall_{run_tag}.csv"))


# Predictions for neighborhood types --------------------------------------

# Get RRs at 99th percentile for each city and neighborhood type
df_pred_int <-
  map2(list_model_city, list_model_city_int, \(model_city, model_city_int) {
    # Estimate RRs at 99th temperature percentile for each nbhd type
    df_city_pred_int <- model_city_int |>
      map(\(nbhd_type_model) {
        city_pred <-
          # Use coef & vcov from interaction model instead of overall model
          crossreduce(
            model_city$cbt,
            coef = nbhd_type_model$coef,
            vcov = nbhd_type_model$vcov,
            model.link = nbhd_type_model$link,
            at = model_city$p99,
            cen = model_city$p75
          )

        # Format df to return
        get_df_RRs(city_pred)
      }) |>
      list_rbind(names_to = "nbhd_type")
  }) |>
  list_rbind(names_to = "city_name")

write_csv(df_pred_int, glue("results/RR_p99_interaction_{run_tag}.csv"))


# Get ratio of RRs --------------------------------------------------------

# Calculate ratio of RRs
df_RR_ratio <-
  df_pred_int |>
  pivot_wider(
    names_from = nbhd_type,
    names_glue = "{.value}_{nbhd_type}",
    values_from = c(fit, se),
    id_cols = "city_name"
  ) |>
  reframe(
    # RR_ratio_Est takes four arguments: fit1, fit2 [reference], se1, se2
    estimate = names(RR_ratio_est(1, 1, .1, .1)),
    low_green_low_edu = RR_ratio_est(
      fit_low_green_low_edu,
      fit_high_green_high_edu,
      se_low_green_low_edu,
      se_high_green_high_edu
    ),
    low_green = RR_ratio_est(
      fit_low_green_high_edu,
      fit_high_green_high_edu,
      se_low_green_high_edu,
      se_high_green_high_edu
    ),
    low_edu = RR_ratio_est(
      fit_high_green_low_edu,
      fit_high_green_high_edu,
      se_high_green_low_edu,
      se_high_green_high_edu
    ),
    .by = city_name
  )

write_csv(df_RR_ratio, glue("results/RR_p99_ratio_{run_tag}.csv"))
