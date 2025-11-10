library(tidyverse)
library(glue)
library(gnm)
library(dlnm)
library(splines)
library(SALURhelper)


# Load data
list_df_city <- readRDS("data/list_df_city.rds")


# Fit base models ---------------------------------------------------------

# Specify model parameters
n_lag <- 21
knot_quantiles <- c(.1, .75, .9)
n_lag_knots <- 3

# Fit overall models
list_model_city <- list_df_city |>
  map(\(df_city) {
    # Specify knots placement
    var_knots <- quantile(df_city$popwt_temp, knot_quantiles, na.rm = TRUE)
    lag_knots <- logknots(n_lag, nk = n_lag_knots)

    # Sequence of exposures for crosspred `at` parameter
    pred_seq <- seq(
      min(df_city$popwt_temp),
      max(df_city$popwt_temp),
      length.out = 101
    )

    # Specify cross-basis
    cbt <- crossbasis(
      df_city$popwt_temp,
      lag = n_lag,
      argvar = list(fun = "ns", knots = var_knots),
      arglag = list(fun = "ns", knots = lag_knots),
      group = df_city$group
    )

    # Define reduced cross-basis
    cbt_red <- onebasis(df_city$popwt_temp, fun = "ns", knots = var_knots)

    # Fit city-specific model
    model <- gnm(
      death ~ cbt,
      family = quasipoisson,
      eliminate = strata,
      data = df_city
    )

    # This list contains the relevant information calculated from model fitting
    # (The model itself is not saved for memory reasons)
    list(
      cbt = cbt,
      cbt_red = cbt_red,
      pred_seq = pred_seq,
      p99 = quantile(df_city$popwt_temp, .99),
      p75 = quantile(df_city$popwt_temp, .75),
      MMT = get_MMT(crossreduce(cbt, model, at = pred_seq)),
      coef = coef(model),
      vcov = vcov(model),
      link = model$family$link
    )
  })


saveRDS(list_model_city, "models/list_model_city.rds")


# Fit interaction model (1 factor of 4 levels) ----------------------------

# Fit interaction models
list_model_city_int <-
  pmap(
    list(df_city = list_df_city, model_city = list_model_city),
    \(df_city, model_city) {
      # Fit initial model
      model <- gnm(
        death ~ model_city$cbt,
        family = quasipoisson,
        eliminate = strata,
        data = df_city
      )

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
        list(
          low_green_low_edu = "cb_lgle",
          low_green_high_edu = "cb_lghe",
          high_green_low_edu = "cb_hgle",
          high_green_high_edu = "cb_hghe"
        ) |>
        map(\(x) {
          # Extract all terms except the reference group
          int_terms <- str_subset(vec_int_terms, x, negate = TRUE) |>
            paste(collapse = " + ")

          # Create formula
          glue(". ~ . + {int_terms}") |> as.formula()
        })

      # Fit interaction models & extract coefs & vcovs
      list_model_int <-
        list_int_formula |>
        map(\(int_formula) {
          # Fit interaction model
          model_city_int <- update(model, int_formula)

          # Extract reference group's coefs & associated vcov
          list(
            coef = coef(model_city_int)[1:n_coef],
            vcov = vcov(model_city_int)[1:n_coef, 1:n_coef],
            link = model_city_int$family$link,
            formula = int_formula
          )
        })
    },
    .progress = TRUE
  )

saveRDS(list_model_city_int, "models/list_model_city_int.rds")


# Fit interaction model (2 factors of 2 levels) ---------------------------


# Fit interaction models
list_model_city_int <-
  pmap(
    list(df_city = list_df_city, model_city = list_model_city),
    \(df_city, model_city) {
      
      # Fit initial model
      model <- gnm(
        death ~ model_city$cbt,
        family = quasipoisson,
        eliminate = strata,
        data = df_city
      )
      
      # Number of coefficients to extract
      n_coef <- ncol(model_city$cbt)
      
      # Specify interaction cross-bases
      cb_lg <- model_city$cbt * (df_city$low_green_low_edu  + df_city$low_green_high_edu)
      cb_hg <- model_city$cbt * (df_city$high_green_low_edu + df_city$high_green_high_edu)
      cb_le <- model_city$cbt * (df_city$low_green_low_edu  + df_city$high_green_low_edu)
      cb_he <- model_city$cbt * (df_city$low_green_high_edu + df_city$high_green_high_edu)
      
      vec_int_terms <- c("cb_lg", "cb_hg", "cb_le", "cb_he")
      
      # Specify interaction model formulas
      list_int_formula <-
        list(
          low_green_low_edu = "cb_lg|cb_le",
          low_green_high_edu = "cb_lg|cb_he",
          high_green_low_edu = "cb_hg|cb_le",
          high_green_high_edu = "cb_hg|cb_he"
        ) |>
        map(\(x) {
          # Extract all terms except the reference group
          int_terms <- str_subset(vec_int_terms, x, negate = TRUE) |>
            paste(collapse = " + ")
          
          # Create formula
          glue(". ~ . + {int_terms}") |> as.formula()
        })
      
      # Fit interaction models & extract coefs & vcovs
      list_model_int <-
        list_int_formula |>
        map(\(int_formula) {
          # Fit interaction model
          model_city_int <- update(model, int_formula)
          
          # Extract reference group's coefs & associated vcov
          list(
            coef = coef(model_city_int)[1:n_coef],
            vcov = vcov(model_city_int)[1:n_coef, 1:n_coef],
            link = model_city_int$family$link,
            formula = int_formula
          )
        })
    },
    .progress = TRUE
  )

saveRDS(list_model_city_int, "models/list_model_city_int_2x2.rds")
