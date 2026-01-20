library(tidyverse)
library(glue)
library(gnm)
library(dlnm)
library(splines)
library(SALURhelper)


# Load data
list_df_city <- readRDS("data/list_df_city.rds")
list_model_city <- readRDS("models/list_model_city.rds")


# Create tertile nbhd_type factor -----------------------------------------
names_nbhd_type_tertile <-
  expand_grid(
    var1 = c("low_green", "med_geen", "high_green"),
    var2 = c("low_edu", "med_edu", "high_edu")
  ) |>
  apply(1, \(x) paste(x, collapse = "_"))


c(80, 80) < c(90, 70)
c(80, 80) %between% c(70, 90)
between(80, 70, 90)

# Fit interaction model (tertiles) ----------------------------------------

# Fit interaction models
list_model_city_int <-
  pmap(
    list(df_city = list_df_city, model_city = list_model_city),
    \(df_city, model_city) {
      # Create tertile nbhd_type factor
      cutoff_grn <- df_city$pct_green |> quantile(c(1 / 3, 2 / 3))
      cutoff_edu <- df_city$pct_educated |> quantile(c(1 / 3, 2 / 3))

      df_city <- df_city |>
        mutate(
          nbhd_type = case_when(
            pct_green < cutoff_grn &
              pct_educated < cutoff_edu ~ names_nbhd_type[1],
            pct_green < cutoff_grn &
              pct_educated >= cutoff_edu ~ names_nbhd_type[2],
            pct_green >= cutoff_grn &
              pct_educated < cutoff_edu ~ names_nbhd_type[3],
            pct_green >= cutoff_grn &
              pct_educated >= cutoff_edu ~ names_nbhd_type[4],
            pct_green < cutoff_grn &
              pct_educated < cutoff_edu ~ names_nbhd_type[5],
            pct_green < cutoff_grn &
              pct_educated >= cutoff_edu ~ names_nbhd_type[6],
            pct_green >= cutoff_grn &
              pct_educated < cutoff_edu ~ names_nbhd_type[7],
            pct_green >= cutoff_grn &
              pct_educated >= cutoff_edu ~ names_nbhd_type[8],
            pct_green >= cutoff_grn &
              pct_educated >= cutoff_edu ~ names_nbhd_type[9],
            .default = "None"
          ) |>
            factor(levels = names_nbhd_type)
        )

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
