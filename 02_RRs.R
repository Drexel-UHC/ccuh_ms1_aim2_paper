library(tidyverse)
library(glue)
library(gnm); library(dlnm); library(splines)
library(SALURhelper)

# Load models
list_city_model <- readRDS("models/list_city_model.rds")
list_city_int_model <- readRDS("models/list_city_int_model.rds")

# Predictions for overall curve -------------------------------------------

# Get overall RRs at 99th percentile
df_pred <- list_city_model |> 
  map(\(city_model) {
    
    # Estimate RRs at 99th temperature percentile
    city_pred <- 
      crossreduce(city_model$cbt,
                  coef = city_model$coef,
                  vcov = city_model$vcov,
                  model.link = city_model$link,
                  at = city_model$p99,
                  cen = city_model$p75) # Can change between p75 and MMT
    
    # Format df to return
    get_df_RRs(city_pred)
  }) |> 
  list_rbind(names_to = "city_name")

write_csv(df_pred, "results/RR_p99_overall.csv")


# Predictions for neighborhood types --------------------------------------

# Get RRs at 99th percentile for each neighborhood type
df_int_pred <- 
  map2(list_city_model, list_city_int_model,
       \(city_model, city_int_model) {
         
         # Estimate RRs at 99th temperature percentile for each nbhd type
         df_city_int_pred <- city_int_model |> 
           map(\(nbhd_type_model) {
             city_pred <- 
               # Use coef & vcov from interaction model instead of overall model
               crossreduce(city_model$cbt,
                           coef = nbhd_type_model$coef,
                           vcov = nbhd_type_model$vcov,
                           model.link = nbhd_type_model$link,
                           at = city_model$p99,
                           cen = city_model$p75) 
             
             # Format df to return
             get_df_RRs(city_pred)
           }) |> 
           list_rbind(names_to = "nbhd_type")
       }) |> 
  list_rbind(names_to = "city_name")

write_csv(df_int_pred, "results/RR_p99_interaction.csv")


# Get ratio of RRs --------------------------------------------------------

# Helper function
RR_ratio_est <- function(fit1, fit2, se1, se2) {
  c(RR_ratio = exp(fit1 - fit2),
    RR_ratio_low = exp(fit1 - fit2 - qnorm(.975)*sqrt(se1^2 + se2^2)),
    RR_ratio_high = exp(fit1 - fit2 + qnorm(.975)*sqrt(se1^2 + se2^2)))
}

# Calculate ratio of RRs
df_RR_ratio <- 
  df_int_pred |> 
  pivot_wider(names_from = nbhd_type,
              names_glue = "{.value}_{nbhd_type}",
              values_from = c(fit, se), 
              id_cols = "city_name") |> 
  reframe(estimate = names(RR_ratio_est(1, 1, .1, .1)),
          low_green_low_edu = RR_ratio_est(fit_low_green_low_edu, fit_high_green_high_edu, se_low_green_low_edu, se_high_green_high_edu),
          low_green         = RR_ratio_est(fit_low_green_high_edu, fit_high_green_high_edu, se_low_green_high_edu, se_high_green_high_edu),
          low_edu           = RR_ratio_est(fit_high_green_low_edu, fit_high_green_high_edu, se_high_green_low_edu, se_high_green_high_edu),
          .by = city_name)

write_csv(df_RR_ratio, "results/RR_p99_ratio.csv")
