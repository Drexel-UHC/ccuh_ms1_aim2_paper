library(tidyverse)
library(glue)
library(gnm); library(dlnm); library(splines)

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
                  cen = city_model$MMT) # Can change between p75 and MMT
    
    # Format df to return
    tibble(prediction = c("center", "lower", "upper", "fit", "se"),
           RR = c(city_pred$RRfit, city_pred$RRlow, city_pred$RRhigh, city_pred$fit, city_pred$se))
  }) |> 
  list_rbind(names_to = "city_name")


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
                           cen = city_model$MMT) 
             
             # Format df to return
             tibble(prediction = c("center", "lower", "upper", "fit", "se"),
                    RR = c(city_pred$RRfit, city_pred$RRlow, city_pred$RRhigh, city_pred$fit, city_pred$se))
           }) |> 
           list_rbind(names_to = "nbhd_type")
       }) |> 
  list_rbind(names_to = "city_name")


# Get difference in RRs ---------------------------------------------------


