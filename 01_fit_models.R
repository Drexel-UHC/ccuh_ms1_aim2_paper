library(tidyverse)
library(glue)
library(gnm); library(dlnm); library(splines)
library(SALURhelper)


# Load data
list_city_df <- readRDS("data/list_city_df.rds")


# Fit base models ---------------------------------------------------------

# Specify model parameters
n_lag <- 21
knot_quantiles <- c(.1, .75, .9)

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
