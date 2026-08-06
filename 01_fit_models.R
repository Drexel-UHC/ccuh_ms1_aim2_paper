library(tidyverse)
library(glue)
library(gnm)
library(dlnm)
library(splines)
library(SALURhelper)

source("helper_functions.R")
source("run_config.R")


# Load data
list_df_city <- readRDS(glue("data/list_df_city_{run_tag}.rds"))


# Fit base models ---------------------------------------------------------

# Specify model parameters
n_lag <- 21
knot_quantiles <- c(.1, .75, .9)
n_lag_knots <- 3

# Fit overall models
list_model_city <- list_df_city |>
  map(\(df_city) {
    # Temperatures on the days that enter the fit; gap-filled days carry no death (STAT-MAJ-03)
    temp_obs <- df_city$popwt_temp[!is.na(df_city$death)]

    # Specify knots placement
    var_knots <- quantile(temp_obs, knot_quantiles, na.rm = TRUE)
    lag_knots <- logknots(n_lag, nk = n_lag_knots)

    # Sequence of exposures for crosspred `at` parameter
    pred_seq <- seq(
      min(temp_obs),
      max(temp_obs),
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
      pred_seq = pred_seq,
      p99 = quantile(temp_obs, .99),
      p75 = quantile(temp_obs, .75),
      MMT = get_MMT(crossreduce(cbt, model, at = pred_seq)),
      # Subsetting drops gnm's eliminated strata effects, which nothing reads (REPRO-MAJ-02)
      coef = coef(model)[seq_len(ncol(cbt))],
      vcov = vcov(model),
      link = model$family$link
    )
  })


# The full cross-basis matrices stay in memory for the interaction models below, but
# only their attributes are needed downstream, so only those are saved (REPRO-MAJ-02)
list_model_city |>
  map(\(model_city) modifyList(model_city, list(cbt = slim_crossbasis(model_city$cbt)))) |>
  saveRDS(glue("models/list_model_city_{run_tag}.rds"))


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
          # (the formula is not stored: its environment drags the data and every
          # interaction cross-basis into the saved object, REPRO-MAJ-02)
          list(
            coef = coef(model_city_int)[1:n_coef],
            vcov = vcov(model_city_int)[1:n_coef, 1:n_coef],
            link = model_city_int$family$link
          )
        })
    },
    .progress = TRUE
  )

saveRDS(list_model_city_int, glue("models/list_model_city_int_{run_tag}.rds"))
