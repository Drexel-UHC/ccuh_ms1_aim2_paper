library(tidyverse)
library(glue)
library(gnm); library(dlnm); library(splines)
library(SALURhelper)


# Load data
list_city_df <- readRDS("data/list_city_df.rds")

# Specify model parameters ------------------------------------------------

# Number of lagged temperatures considered in model
n_lag <- 21

knot_quantiles <- c(.1, .75, .9)


# Fit base models ---------------------------------------------------------

# Fit overall models
list_city_model <- list_city_df |> 
  map(\(df_city) {
    
    # Specify knots placement
    var_knots <- quantile(df_city$popwt_temp, knot_quantiles, na.rm = TRUE)
    lag_knots <- logknots(n_lag, nk = 3)
    
    # Sequence of exposures for crosspred `at` parameter
    pred_seq <- seq(min(df_city$popwt_temp), 
                    max(df_city$popwt_temp),
                    length.out = 101)
    
    # Specify cross-basis
    cbt <- crossbasis(df_city$popwt_temp, lag = n_lag,
                      argvar = list(fun = "ns", knots = var_knots),
                      arglag = list(fun = "ns", knots = lag_knots),
                      group = df_city$group)
    
    # Define reduced cross-basis
    cbt_red <- onebasis(df_city$popwt_temp, fun = "ns", knots = var_knots)
    
    # Fit city-specific model
    model <- gnm(death ~ cbt, 
                 family = quasipoisson,
                 eliminate = strata,
                 data = df_city)
    
    # This list contains the relevant information calculated from model fitting
    # (The model itself is not saved for memory reasons)
    list(cbt      = cbt,
         cbt_red  = cbt_red,
         pred_seq = pred_seq,
         p99  = quantile(df_city$popwt_temp, .99),
         p75  = quantile(df_city$popwt_temp, .75),
         MMT  = get_MMT(crossreduce(cbt, model, at = pred_seq)),
         coef = coef(model),
         vcov = vcov(model),
         link = model$family$link)
  })


saveRDS(list_city_model, "models/list_city_model.rds")


# Fit interaction model (1 factor of 4 levels) ----------------------------

# Define possible neighborhood types
names_nbhd_type <- c("low_green_low_edu",
                     "low_green_high_edu",
                     "high_green_low_edu", 
                     "high_green_high_edu")

# Define neighborhoods by type per city
list_city_df <- list_city_df |> 
  map(\(df_city) {
    # Cutoff values for nbhd definitions
    cutoff_grn <- median(df_city$pct_green)
    cutoff_edu <- median(df_city$pct_educated)
    
    # Create nbhd_type factor
    df_city <- df_city |> 
      mutate(nbhd_type = case_when(
        pct_green < cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[1],
        pct_green < cutoff_grn & pct_educated >=cutoff_edu ~ names_nbhd_type[2],
        pct_green >=cutoff_grn & pct_educated < cutoff_edu ~ names_nbhd_type[3],
        pct_green >=cutoff_grn & pct_educated >=cutoff_edu ~ names_nbhd_type[4],
        .default = "None") |> factor(levels = names_nbhd_type))
    
    # Create df of dummy variables
    df_dummy_vars <- model.matrix(~ nbhd_type + 0, data = df_city) |> 
      as_tibble() |> 
      rename_with(\(x) str_remove(x, "nbhd_type"))
    
    # Return new df with factor & dummy variables
    bind_cols(df_city, df_dummy_vars)
  })

# Fit interaction models
list_city_int_model <- 
  pmap(list(df_city = list_city_df,
            model_city = list_city_model),
       \(df_city, model_city) {
         
         # Fit initial model
         model <- gnm(death ~ model_city$cbt,
                      family = quasipoisson,
                      eliminate = strata,
                      data = df_city)
         
         # Number of coefficients to extract
         n_coef <- ncol(model_city$cbt)
         
         # Specify interaction cross-bases
         cb_lgle <- model_city$cbt * df_city$low_green_low_edu
         cb_lghe <- model_city$cbt * df_city$low_green_high_edu
         cb_hgle <- model_city$cbt * df_city$high_green_low_edu
         cb_hghe <- model_city$cbt * df_city$high_green_high_edu
         
         vec_int_terms <- c("cb_lgle", "cb_lghe", "cb_hgle", "cb_hghe")
         
         # Specify interaction model formulas
         list_int_formula <- 
           list(low_green_low_edu   = "cb_lgle",
                low_green_high_edu  = "cb_lghe",
                high_green_low_edu  = "cb_hgle",
                high_green_high_edu = "cb_hghe") |> 
           map(\(x) {
             # Extract all terms except the reference group
             int_terms <- str_subset(vec_int_terms, x, negate = TRUE) |> paste(collapse = " + ")
             
             # Create formula
             glue(". ~ . + {int_terms}") |> as.formula()
             })
         
         # Fit interaction models & extract coefs & vcovs
         list_int_model <- 
           list_int_formula |> 
           map(\(int_formula) {
             # Fit interaction model
             city_int_model <- update(model, int_formula)
             
             # Extract reference group's coefs & associated vcov
             list(coef = coef(city_int_model)[1:n_coef],
                  vcov = vcov(city_int_model)[1:n_coef, 1:n_coef],
                  link = city_int_model$family$link)
           })
         
       }, .progress = TRUE)

saveRDS(list_city_int_model, "models/list_city_int_model.rds")
