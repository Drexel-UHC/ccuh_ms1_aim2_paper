library(tidyverse)
library(glue)
library(gnm); library(dlnm); library(splines)

# Data formatting ---------------------------------------------------------

# Load data
df <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/v_0_1_0/ccuh_ms1__final_bundle_age__v1.parquet" |> 
  arrow::open_dataset() |>  
  dplyr::collect()

# Create list of data frames by city
list_city_df <- 
  df_data_age |> 
  split(df_data_age$city_name) |>
  discard_at(c("Belo Horizonte", "Campinas")) |>
  map(\(df_city) {
    df_city |> 
      mutate(strata = glue("{nbhd_geoid}:{year(date)}:{month(date)}:{wday(date, label = TRUE)}:{age_category}"),
             group = glue("{nbhd_geoid}:{age_category}"),
             across(c(nbhd_geoid, strata, group, age_category), factor)) |> 
      arrange(across(c(nbhd_geoid, date, age_category)))
  })



# Fit base models ---------------------------------------------------------

# Number of lagged temperatures considered in model
n_lags <- 21

# Fit model sans modifiers
list_city_model <- list_city_df |> 
  map(\(df_city) {
    
    # Specify knots placement
    var_knots <- quantile(df_city$popwt_temp, c(.1, .75, .9), na.rm = TRUE)
    lag_knots <- logknots(n_lags, nk = 3)
    
    # Specify cross-basis
    cbt <- crossbasis(df_city$popwt_temp, lag = n_lags,
                      argvar = list(fun = "ns", knots = var_knots),
                      arglag = list(fun = "ns", knots = lag_knots),
                      group = df_city$group)
    
    # Define reduced cross-basis for future predictions
    cbt_red <- onebasis(df_city$popwt_temp, fun = "ns", knots = var_knots)
    
    # Fit city-specific model
    model <- gnm(death ~ cbt, 
                 family = quasipoisson(),
                 eliminate = strata,
                 data = df_city)
    
    # This list contains the relevant information calculated for model fitting
    list(cb = cbt,
         cb_red = cbt_red,
         pred_seq = seq(min(df_city$popwt_temp),
                        max(df_city$popwt_temp),
                        length.out = 101), # Number sequence for crosspred `at` parameter
         coef = coef(model),
         vcov = vcov(model),
         link = model$family$link)
  })




# Fit interaction model (1 factor of 4 levels) ----------------------------


# Interaction models
df_city_rotating_pred <- 
  # Map over cities
  map(list_city_df,
       \(df_city) {
         
         # Add binary indicator columns for each modifier of interest
         df_city <- df_city |> 
           mutate(across(all_of(modifiers_of_interest), \(x) ifelse(x - median(x) <  0, 1, 0),
                         .names = "{.col}_low"), # Below median
                  across(all_of(modifiers_of_interest), \(x) ifelse(x - median(x) >= 0, 1, 0),
                         .names = "{.col}_high"))# Above median
         
         # Set P99 and P75
         p99 <- quantile(df_city$popwt_temp, .99)
         p75 <- quantile(df_city$popwt_temp, .75)
         
         
         # Map over modifiers of interest
         modifiers_of_interest[-1] |> 
           map(\(var) {
             # Original cross-basis
             cbt <- city_model$cb
             
             # Interaction cross-bases
             grn_1 <- cbt * df_city[["pct_green_low"]]
             grn_2 <- cbt * df_city[["pct_green_high"]]
             int_1 <- cbt * df_city[[glue("{var}_low")]]
             int_2 <- cbt * df_city[[glue("{var}_high")]]
             
             # Fit new interaction models with low and high reference groups
             list_models <- 
               list(model_11 = update(city_model$model, . ~ . + grn_2 + int_2), # Reference group: low green low int
                    model_12 = update(city_model$model, . ~ . + grn_2 + int_1), # Reference group: low green high int
                    model_21 = update(city_model$model, . ~ . + grn_1 + int_2), # Reference group: high green low int
                    model_22 = update(city_model$model, . ~ . + grn_1 + int_1)) # Reference group: high green high int
             
             # Get RR at P99 vs P75
             df_RR_p99 <- list_models |> 
               map(\(model) {
                 p99_v_p75 <- crossreduce(cbt, model, at = p99, cen = p75)
                 
                 tibble(prediction = c('fit', 'se', 'center', 'lower', 'upper'),
                        p99_v_p75 = c(p99_v_p75$fit, p99_v_p75$se, p99_v_p75$RRfit, p99_v_p75$RRlow, p99_v_p75$RRhigh))
               }) |> list_rbind(names_to = 'reference_group')
             
             df_RR_p99
             
           }, .progress = TRUE) |> set_names(modifiers_of_interest[-1]) |> list_rbind(names_to = 'modifiers_of_interest')
         
       }) |> list_rbind(names_to = 'city_name')


# Fit interaction model (2 factors of 2 levels) ---------------------------


