library(tidyverse)
library(here)
library(glue)
library(gt)
library(SALURhelper)
library(dlnm); library(gnm); library(splines)


# Load data ---------------------------------------------------------------
list_df <- readRDS(here("1-data", "list_city_df.rds"))
city_names <- names(list_df)

list_df <- list_df |> 
  map(\(df0) {
    # Define city-specific median points
    med_pct_green    <- median(df0$pct_green)
    med_pct_educated <- median(df0$pct_educated)
    med_pop_density  <- median(df0$pop_density)
    
    # Add categorical dummy variables
    df0 <- df0 |> 
      mutate(pct_green_low    = ifelse(pct_green    <= med_pct_green,    1, 0),
             pct_green_high   = ifelse(pct_green    >  med_pct_green,    1, 0),
             pct_educated_low = ifelse(pct_educated <= med_pct_educated, 1, 0),
             pct_educated_high= ifelse(pct_educated >  med_pct_educated, 1, 0),
             pop_density_low  = ifelse(pop_density  <= med_pop_density,  1, 0),
             pop_density_high = ifelse(pop_density  >  med_pop_density,  1, 0))
  })


# Get predictions for overall curve ---------------------------------------

# Predictions centering at MMT or P75
df_pred_by_city <-
  map(list_df, \(df0) {
    
    # Specify knots placement
    var_knots <- quantile(df0$popwt_temp, c(.1, .75, .9), na.rm = TRUE)
    lag_knots <- logknots(21, nk = 3)
    
    # Specify cross-basis
    cbt <- crossbasis(df0$popwt_temp, lag = 21,
                      argvar = list(fun = "ns", knots = var_knots),
                      arglag = list(fun = "ns", knots = lag_knots),
                      group = df0$group)
    
    # Fit city-specific model
    model <- gnm(death ~ cbt, 
                 family = quasipoisson(),
                 eliminate = strata,
                 data = df0)
    
    # Fine predictions to get MMT
    pred_at <- seq(min(df0$popwt_temp), max(df0$popwt_temp), length.out = 100)
    p75 <- quantile(df0$popwt_temp, .75)
    p99 <- quantile(df0$popwt_temp, .99)
    
    # Get RR at P99 vs MMT or P75
    pred_for_MMT <- crossreduce(cbt,
                                model,
                                at = pred_at,
                                cen = p75)
    
    p99_v_MMT = crossreduce(cbt, model, at = p99, cen = get_MMT(pred_for_MMT))
    p99_v_p75 = crossreduce(cbt, model, at = p99, cen = p75)
    
    # Returned df
    tibble(prediction = c("center", "lower", "upper"),
           p99_v_MMT = c(p99_v_MMT$RRfit, p99_v_MMT$RRlow, p99_v_MMT$RRhigh),
           p99_v_p75 = c(p99_v_p75$RRfit, p99_v_p75$RRlow, p99_v_p75$RRhigh))
  }) |> list_rbind(names_to = "city_name")


# Create table
tbl_pred_by_city <- df_pred_by_city |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75),
              names_from = prediction,
              names_sep = "_") |> 
  mutate(across(contains("p99"), \(x) round(x, 3))) |> 
  mutate(p99_v_MMT = glue("{p99_v_MMT_center} ({p99_v_MMT_lower}, {p99_v_MMT_upper})"),
         p99_v_p75 = glue("{p99_v_p75_center} ({p99_v_p75_lower}, {p99_v_p75_upper})"),
         .keep = "unused") |> 
  gt(rowname_col = "city_name") |> 
  tab_header(title = "RR at the 99th percentile temperature of 5 cities",
             subtitle = "Centered at either the MMT or 75th percentile") |> 
  tab_footnote(footnote = "95% confidence interval",
               location = cells_column_labels(c(p99_v_MMT, p99_v_p75))) |> 
  tab_options(footnotes.marks = "letters") |> 
  tab_spanner(label = "Risk Ratio at P99", 
              columns = c(p99_v_MMT, p99_v_p75)) |> 
  cols_label(p99_v_MMT = "Centered at MMT",
             p99_v_p75 = "Centered at P75") |> 
  tab_style(style = cell_fill(color = "grey90"),
            locations = cells_body(rows = seq(1, 5, 2)))

# Show table
tbl_pred_by_city

# Save in word doc
gtsave(tbl_pred_by_city, "p99_RR_by_city.docx")


# Get predictions with rotating reference group method --------------------

# Name of each model
var_levels <- c(
  model_1 = "Low Green, Edu, Pop Dens",
  model_2 = "Low Green, Edu/High Pop Dens",
  model_3 = "Low Green, Pop Dens/High Edu",
  model_4 = "Low Green/High Edu, Pop Dens",
  model_5 = "High Green/Low Edu, Pop Dens",
  model_6 = "High Green, Pop Dens/Low Edu",
  model_7 = "High Green, Edu/Low Pop Dens",
  model_8 = "High Green, Edu, Pop Dens"
)

# Make predictions
df_multi_interaction_model_by_city <-  
  map(list_df, \(df0) {
    # # Define city-specific median points
    # med_pct_green    <- median(df0$pct_green)
    # med_pct_educated <- median(df0$pct_educated)
    # med_pop_density  <- median(df0$pop_density)
    # 
    # # Add categorical dummy variables
    # df0 <- df0 |> 
    #   mutate(pct_green_low    = ifelse(pct_green    <= med_pct_green,    1, 0),
    #          pct_green_high   = ifelse(pct_green    >  med_pct_green,    1, 0),
    #          pct_educated_low = ifelse(pct_educated <= med_pct_educated, 1, 0),
    #          pct_educated_high= ifelse(pct_educated >  med_pct_educated, 1, 0),
    #          pop_density_low  = ifelse(pop_density  <= med_pop_density,  1, 0),
    #          pop_density_high = ifelse(pop_density  >  med_pop_density,  1, 0))
    
    # Specify knots placement
    var_knots <- quantile(df0$popwt_temp, c(.1, .75, .9), na.rm = TRUE)
    lag_knots <- logknots(21, nk = 3)
    
    # Specify cross-basis
    cbt <- crossbasis(df0$popwt_temp, lag = 21,
                      argvar = list(fun = "ns", knots = var_knots),
                      arglag = list(fun = "ns", knots = lag_knots),
                      group = df0$group)
    
    # Fit Initial Model
    model <- gnm(death ~ cbt,
                 family = quasipoisson(),
                 eliminate = strata,
                 data = df0)
    
    # Interaction: greenspace
    cb_grn1 <- cbt * df0$pct_green_low
    cb_grn2 <- cbt * df0$pct_green_high
    
    # Interaction: education
    cb_edu1 <- cbt * df0$pct_educated_low
    cb_edu2 <- cbt * df0$pct_educated_high
    
    # Interaction: pop density
    cb_pdn1 <- cbt * df0$pop_density_low
    cb_pdn2 <- cbt * df0$pop_density_high
    
    # Model: all combinations of green, edu, and pop dens
    list_models <-
      list(
        model_1 = update(model, . ~ . + cb_grn2 + cb_edu2 + cb_pdn2), # Reference group: low green, edu, pop dens
        model_2 = update(model, . ~ . + cb_grn2 + cb_edu2 + cb_pdn1), # Reference group: low green, edu; high: pop dens
        model_3 = update(model, . ~ . + cb_grn2 + cb_edu1 + cb_pdn2), # Reference group: low green, pop dens; high: edu
        model_4 = update(model, . ~ . + cb_grn2 + cb_edu1 + cb_pdn1), # Reference group: low green; high: edu, pop dens
        model_5 = update(model, . ~ . + cb_grn1 + cb_edu2 + cb_pdn2), # Reference group: high green; low: edu, pop dens
        model_6 = update(model, . ~ . + cb_grn1 + cb_edu2 + cb_pdn1), # Reference group: high green, pop dens; low: edu
        model_7 = update(model, . ~ . + cb_grn1 + cb_edu1 + cb_pdn2), # Reference group: high green,edu; low: pop dens
        model_8 = update(model, . ~ . + cb_grn1 + cb_edu1 + cb_pdn1)  # Reference group: high green, edu, pop dens
      )
    
    # Fine predictions to get MMT
    pred_at <- seq(min(df0$popwt_temp), max(df0$popwt_temp), length.out = 100)
    p75 <- quantile(df0$popwt_temp, .75)
    p99 <- quantile(df0$popwt_temp, .99)
    
    list_pred_for_MMT <- list_models |> 
      map(\(model) {
        crossreduce(cbt, model, at = pred_at, cen = p75)
      })
    
    MMT <- map_dbl(list_pred_for_MMT, get_MMT)
    
    # Get RR at P99 vs MMT or P75
    df_RR_p99 <- 
      list(model = list_models,
           pred_for_MMT = list_pred_for_MMT) |> 
      pmap(\(model, pred_for_MMT) {
        p99_v_MMT <- crossreduce(cbt, model, at = p99, cen = get_MMT(pred_for_MMT))
        p99_v_p75 <- crossreduce(cbt, model, at = p99, cen = p75)
        
        # Returned df (RRs)
        # tibble(prediction = c("center", "lower", "upper"),
        #        p99_v_MMT = c(p99_v_MMT$RRfit, p99_v_MMT$RRlow, p99_v_MMT$RRhigh),
        #        p99_v_p75 = c(p99_v_p75$RRfit, p99_v_p75$RRlow, p99_v_p75$RRhigh))
        
        # Returned df 
        tibble(prediction = c("fit", "se"),
               p99_v_MMT = c(p99_v_MMT$fit, p99_v_MMT$se),
               p99_v_p75 = c(p99_v_p75$fit, p99_v_p75$se))
      }) |> list_rbind(names_to = "reference_group")
    
    df_RR_p99
    
  }, .progress = TRUE) |> list_rbind(names_to = "city_name")

# Save predictions
saveRDS(df_multi_interaction_model_by_city, "RR_p99_multi_interaction_fit.rds")

# Load predictions
df_multi_interaction_model_by_city <- readRDS("RR_p99_multi_interaction.rds")

# Create dataframe for table
df_tbl <- df_multi_interaction_model_by_city |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75), 
              names_from = prediction, names_sep = "_") |> 
  mutate(across(contains("p99"), \(x) round(x, 3))) |> 
  mutate(MMT = glue("{p99_v_MMT_center} ({p99_v_MMT_lower}, {p99_v_MMT_upper})"),
         P75 = glue("{p99_v_p75_center} ({p99_v_p75_lower}, {p99_v_p75_upper})"),
         .keep = "unused") |> 
  pivot_longer(c(MMT, P75), names_to = "center", values_to = "p99_RR") |> 
  pivot_wider(values_from = p99_RR,
              names_from = city_name, names_sep = "_") |> 
  mutate(reference_group = factor(reference_group, labels = var_levels)) |> 
  arrange(center) |> 
  select(c(center, reference_group, everything()))
  
# Create gt object
tbl_multi_interaction <- df_tbl |> 
    group_by(center) |> 
    gt() |> 
    tab_header(title = "Risk ratios at the 99th percentile temperature",
               subtitle = "Curves centered at either the MMT or 75th percentile") |> 
    gtExtras::gt_highlight_rows(rows = c(2, 7, 10, 15)) |> 
    tab_footnote(footnote = "Expected worst case scenario.",
                 locations = cells_body(columns = "reference_group",
                                        rows = c(2, 10))) |> 
    tab_footnote(footnote = "Expected best case scenario.",
                 locations = cells_body(columns = "reference_group",
                                        rows = c(7, 15))) |> 
    tab_options(footnotes.marks = "letters") |> 
    cols_label(reference_group = "") |> 
    tab_style(style = cell_borders(sides = "right"),
              locations = cells_body(columns = "reference_group"))

# Save table
saveRDS(tbl_multi_interaction, "2-code/p99_RR_by_city_multi_interaction.rds")
gtsave(tbl_multi_interaction, "2-code/p99_RR_by_city_multi_interaction.html")

# Find difference in RRs by change from "best to worst" by nieghborhood modifier
# Load predictions
df_mi_fit <- readRDS("RR_p99_multi_interaction_fit.rds")

# Helper function
RR_diff_est <- function(fit1, fit2, se1, se2) {
  glue("{RR} ({lower}, {upper})",
       RR = round(exp(fit1 - fit2), 3),
       lower = round(exp(fit1 - fit2 - qnorm(.975)*sqrt(se1^2 + se2^2)), 3),
       upper = round(exp(fit1 - fit2 + qnorm(.975)*sqrt(se1^2 + se2^2)), 3))
}

# Difference in RRs table
tbl_multi_interaction_RR_diffs <- df_mi_fit |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75),
              names_from = c(reference_group, prediction), names_sep = "_") |> 
  mutate(city_name = city_name,
         greenspace_p99_v_MMT = RR_diff_est(p99_v_MMT_model_2_fit, p99_v_MMT_model_6_fit, p99_v_MMT_model_2_se, p99_v_MMT_model_6_se),#"{exp(p99_v_MMT_model_2_fit - p99_v_MMT_model_6_fit)} {sqrt(p99_v_MMT_model_2_se^2 + p99_v_MMT_model_6_se^2)}"),
         education_p99_v_MMT  = RR_diff_est(p99_v_MMT_model_2_fit, p99_v_MMT_model_4_fit, p99_v_MMT_model_2_se, p99_v_MMT_model_4_se),
         popdens_p99_v_MMT    = RR_diff_est(p99_v_MMT_model_2_fit, p99_v_MMT_model_1_fit, p99_v_MMT_model_2_se, p99_v_MMT_model_1_se), 
         greenspace_p99_v_p75 = RR_diff_est(p99_v_p75_model_2_fit, p99_v_p75_model_6_fit, p99_v_p75_model_2_se, p99_v_p75_model_6_se),
         education_p99_v_p75  = RR_diff_est(p99_v_p75_model_2_fit, p99_v_p75_model_4_fit, p99_v_p75_model_2_se, p99_v_p75_model_4_se),
         popdens_p99_v_p75    = RR_diff_est(p99_v_p75_model_2_fit, p99_v_p75_model_1_fit, p99_v_p75_model_2_se, p99_v_p75_model_1_se), .keep = "none") |> 
  gt(rowname_col = "city_name") |> 
  tab_header(title = "Ratio of risk ratios at the 99th percentile temperature",
             subtitle = "Comparison of baseline scenario with individual variable changes") |> 
  tab_spanner(label = "Centered at MMT", columns = ends_with("MMT")) |> 
  tab_spanner(label = "Centered at P75", columns = ends_with("p75")) |> 
  cols_label(starts_with("greenspace") ~ "Greenspace",
             starts_with("education") ~ "Education",
             starts_with("popdens") ~ "Pop Density")

saveRDS(tbl_multi_interaction_RR_diffs, "2-code/p99_RR_by_city_multi_interaction_RR_diffs.rds")

# Predictions with rotating reference group (no pop dens) -----------------

var_levels2 <- c(
  model_1 = "Low Green and Edu",
  model_2 = "Low Green and High Edu",
  model_3 = "High Green and Low Edu",
  model_4 = "High Green and Edu"
)

df_multi_interaction_model_by_city2 <-  
  map(list_df, \(df0) {
    
    # Specify knots placement
    var_knots <- quantile(df0$popwt_temp, c(.1, .75, .9), na.rm = TRUE)
    lag_knots <- logknots(21, nk = 3)
    
    # Specify cross-basis
    cbt <- crossbasis(df0$popwt_temp, lag = 21,
                      argvar = list(fun = "ns", knots = var_knots),
                      arglag = list(fun = "ns", knots = lag_knots),
                      group = df0$group)
    
    # Fit Initial Model
    model <- gnm(death ~ cbt,
                 family = quasipoisson(),
                 eliminate = strata,
                 data = df0)
    
    # Interaction: greenspace
    cb_grn1 <- cbt * df0$pct_green_low
    cb_grn2 <- cbt * df0$pct_green_high
    
    # Interaction: education
    cb_edu1 <- cbt * df0$pct_educated_low
    cb_edu2 <- cbt * df0$pct_educated_high
    
    # Interaction: greenspace & education
    cb_int1 <- cbt * df0$pct_green_high * df0$pct_educated_high
    cb_int2 <- cbt * df0$pct_green_high * df0$pct_educated_low
    cb_int3 <- cbt * df0$pct_green_low * df0$pct_educated_high
    cb_int4 <- cbt * df0$pct_green_low * df0$pct_educated_low
    
    # Model: all combinations of green, edu
    list_models <-
      list(
        model_1 = update(model, . ~ . + cb_grn2 + cb_edu2 + cb_int1), # Reference group: low green, edu
        model_2 = update(model, . ~ . + cb_grn2 + cb_edu1 + cb_int2), # Reference group: low green, high edu
        model_3 = update(model, . ~ . + cb_grn1 + cb_edu2 + cb_int3), # Reference group: high green, low edu
        model_4 = update(model, . ~ . + cb_grn1 + cb_edu1 + cb_int4)  # Reference group: high green, edu
      )
    
    # Fine predictions to get MMT
    pred_at <- seq(min(df0$popwt_temp), max(df0$popwt_temp), length.out = 100)
    p75 <- quantile(df0$popwt_temp, .75)
    p99 <- quantile(df0$popwt_temp, .99)
    
    list_pred_for_MMT <- list_models |> 
      map(\(model) {
        crossreduce(cbt, model, at = pred_at, cen = p75)
      })
    
    MMT <- map_dbl(list_pred_for_MMT, get_MMT)
    
    # Get RR at P99 vs MMT or P75
    df_RR_p99 <- 
      list(model = list_models,
           pred_for_MMT = list_pred_for_MMT) |> 
      pmap(\(model, pred_for_MMT) {
        p99_v_MMT <- crossreduce(cbt, model, at = p99, cen = get_MMT(pred_for_MMT))
        p99_v_p75 <- crossreduce(cbt, model, at = p99, cen = p75)
        
        # Returned df
        # tibble(prediction = c("center", "lower", "upper"),
        #        p99_v_MMT = c(p99_v_MMT$RRfit, p99_v_MMT$RRlow, p99_v_MMT$RRhigh),
        #        p99_v_p75 = c(p99_v_p75$RRfit, p99_v_p75$RRlow, p99_v_p75$RRhigh))
        
        # Returned df 
        tibble(prediction = c("fit", "se"),
               p99_v_MMT = c(p99_v_MMT$fit, p99_v_MMT$se),
               p99_v_p75 = c(p99_v_p75$fit, p99_v_p75$se))
      }) |> list_rbind(names_to = "reference_group")
    
    df_RR_p99
    
  }, .progress = TRUE) |> list_rbind(names_to = "city_name")


saveRDS(df_multi_interaction_model_by_city2, "RR_p99_multi_interaction2_fit.rds")

df_multi_interaction_model_by_city2 <- readRDS("RR_p99_multi_interaction2.rds")

# Create table
df_tbl2 <- df_multi_interaction_model_by_city2 |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75), 
              names_from = prediction, names_sep = "_") |> 
  mutate(across(contains("p99"), \(x) round(x, 3))) |> 
  mutate(MMT = glue("{p99_v_MMT_center} ({p99_v_MMT_lower}, {p99_v_MMT_upper})"),
         P75 = glue("{p99_v_p75_center} ({p99_v_p75_lower}, {p99_v_p75_upper})"),
         .keep = "unused") |> 
  pivot_longer(c(MMT, P75), names_to = "center", values_to = "p99_RR") |> 
  pivot_wider(values_from = p99_RR,
              names_from = city_name, names_sep = "_") |> 
  mutate(reference_group = factor(reference_group, labels = var_levels2)) |> 
  arrange(center) |> 
  select(c(center, reference_group, everything()))

tbl_multi_interaction2 <- df_tbl2 |> 
  group_by(center) |> 
  gt() |> 
  tab_header(title = "Risk ratios at the 99th percentile temperature",
             subtitle = "Curves centered at either the MMT or 75th percentile") |> 
  gtExtras::gt_highlight_rows(rows = c(1, 4, 5, 8)) |> 
  tab_footnote(footnote = "Expected worst case scenario.",
               locations = cells_body(columns = "reference_group",
                                      rows = c(1, 4))) |> 
  tab_footnote(footnote = "Expected best case scenario.",
               locations = cells_body(columns = "reference_group",
                                      rows = c(5, 8))) |> 
  tab_options(footnotes.marks = "letters") |> 
  cols_label(reference_group = "") |> 
  tab_style(style = cell_borders(sides = "right"),
            locations = cells_body(columns = "reference_group"))

saveRDS(tbl_multi_interaction2, "p99_RR_by_city_multi_interaction2.rds")
gtsave(tbl_multi_interaction2, "p99_RR_by_city_multi_interaction2.html")

# Find difference in RRs by change from "best to worst" by nieghborhood modifier
# Load predictions
df_mi_fit2 <- readRDS("RR_p99_multi_interaction2_fit.rds")

# Helper function
RR_ratio_est <- function(fit1, fit2, se1, se2) {
  glue("{RR} ({lower}, {upper})",
       RR = round(exp(fit1 - fit2), 3),
       lower = round(exp(fit1 - fit2 - qnorm(.975)*sqrt(se1^2 + se2^2)), 3),
       upper = round(exp(fit1 - fit2 + qnorm(.975)*sqrt(se1^2 + se2^2)), 3))
}

RR_diff_est <- function(fit1, fit2) {
  glue("{RR}", RR = round(exp(fit1) - exp(fit2), 3))
}

# Difference in RRs table (baseline: worst case)
tbl_multi_interaction_RR_diffs2 <- df_mi_fit2 |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75),
              names_from = c(reference_group, prediction), names_sep = "_") |> 
  mutate(city_name = city_name,
         greenspace_p99_v_MMT = RR_ratio_est(p99_v_MMT_model_1_fit, p99_v_MMT_model_3_fit, p99_v_MMT_model_1_se, p99_v_MMT_model_3_se),#"{exp(p99_v_MMT_model_2_fit - p99_v_MMT_model_6_fit)} {sqrt(p99_v_MMT_model_2_se^2 + p99_v_MMT_model_6_se^2)}"),
         education_p99_v_MMT  = RR_ratio_est(p99_v_MMT_model_1_fit, p99_v_MMT_model_2_fit, p99_v_MMT_model_1_se, p99_v_MMT_model_2_se),
         greenspace_p99_v_p75 = RR_ratio_est(p99_v_p75_model_1_fit, p99_v_p75_model_3_fit, p99_v_p75_model_1_se, p99_v_p75_model_3_se),
         education_p99_v_p75  = RR_ratio_est(p99_v_p75_model_1_fit, p99_v_p75_model_2_fit, p99_v_p75_model_1_se, p99_v_p75_model_2_se), .keep = "none") |> 
  gt(rowname_col = "city_name") |> 
  tab_header(title = "Ratio of risk ratios at the 99th percentile temperature",
             subtitle = "Comparison of worst case scenario with individual variable changes") |> 
  tab_spanner(label = "Centered at MMT", columns = ends_with("MMT")) |> 
  tab_spanner(label = "Centered at P75", columns = ends_with("p75")) |> 
  cols_label(starts_with("greenspace") ~ "Greenspace",
             starts_with("education") ~ "Education")

saveRDS(tbl_multi_interaction_RR_diffs2, "2-code/p99_RR_by_city_multi_interaction_RR_diffs2.rds")

# Ratio in RRs table (baseline: best case)
tbl_multi_interaction_RR_diffs3 <- df_mi_fit2 |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75),
              names_from = c(reference_group, prediction), names_sep = "_") |> 
  mutate(city_name = city_name,
         low_green_edu_p99_v_MMT = RR_ratio_est(p99_v_MMT_model_1_fit, p99_v_MMT_model_4_fit, p99_v_MMT_model_1_se, p99_v_MMT_model_4_se),#"{exp(p99_v_MMT_model_2_fit - p99_v_MMT_model_6_fit)} {sqrt(p99_v_MMT_model_2_se^2 + p99_v_MMT_model_6_se^2)}"),
         low_green_p99_v_MMT     = RR_ratio_est(p99_v_MMT_model_2_fit, p99_v_MMT_model_4_fit, p99_v_MMT_model_2_se, p99_v_MMT_model_4_se),
         low_edu_p99_v_MMT       = RR_ratio_est(p99_v_MMT_model_3_fit, p99_v_MMT_model_4_fit, p99_v_MMT_model_3_se, p99_v_MMT_model_4_se),
         low_green_edu_p99_v_p75 = RR_ratio_est(p99_v_p75_model_1_fit, p99_v_p75_model_4_fit, p99_v_p75_model_1_se, p99_v_p75_model_4_se),
         low_green_p99_v_p75     = RR_ratio_est(p99_v_p75_model_2_fit, p99_v_p75_model_4_fit, p99_v_p75_model_2_se, p99_v_p75_model_4_se),
         low_edu_p99_v_p75       = RR_ratio_est(p99_v_p75_model_3_fit, p99_v_p75_model_4_fit, p99_v_p75_model_3_se, p99_v_p75_model_4_se), .keep = "none") |> 
  gt(rowname_col = "city_name") |> 
  tab_header(title = "Ratio of risk ratios at the 99th percentile temperature",
             subtitle = "Comparison of best case scenario to variable changes") |> 
  tab_spanner(label = "Centered at MMT", columns = ends_with("MMT")) |> 
  tab_spanner(label = "Centered at P75", columns = ends_with("p75")) |> 
  cols_label(starts_with("low_green_edu") ~ "Low Greenspace and Edu",
             starts_with("low_green_p") ~ "Low Greenspace",
             starts_with("low_edu") ~ "Low Education")

saveRDS(tbl_multi_interaction_RR_diffs3, "2-code/p99_RR_by_city_multi_interaction_RR_diffs3.rds")

# Difference in RRs table (baseline: best case)
tbl_multi_interaction_RR_diffs4 <- df_mi_fit2 |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75),
              names_from = c(reference_group, prediction), names_sep = "_") |> 
  mutate(city_name = city_name,
         low_green_edu_p99_v_MMT = RR_diff_est(p99_v_MMT_model_4_fit, p99_v_MMT_model_1_fit),#"{exp(p99_v_MMT_model_2_fit - p99_v_MMT_model_6_fit)} {sqrt(p99_v_MMT_model_2_se^2 + p99_v_MMT_model_6_se^2)}"),
         low_green_p99_v_MMT  = RR_diff_est(p99_v_MMT_model_4_fit, p99_v_MMT_model_2_fit),
         low_edu_p99_v_MMT  = RR_diff_est(p99_v_MMT_model_4_fit, p99_v_MMT_model_3_fit),
         low_green_edu_p99_v_p75 = RR_diff_est(p99_v_p75_model_4_fit, p99_v_p75_model_1_fit),
         low_green_p99_v_p75  = RR_diff_est(p99_v_p75_model_4_fit, p99_v_p75_model_2_fit),
         low_edu_p99_v_p75  = RR_diff_est(p99_v_p75_model_4_fit, p99_v_p75_model_3_fit), .keep = "none") |> 
  gt(rowname_col = "city_name") |> 
  tab_header(title = "Difference of risk ratios at the 99th percentile temperature",
             subtitle = "Comparison of best case scenario to variable changes") |> 
  tab_spanner(label = "Centered at MMT", columns = ends_with("MMT")) |> 
  tab_spanner(label = "Centered at P75", columns = ends_with("p75")) |> 
  cols_label(starts_with("low_green_edu") ~ "Low Greenspace and Edu",
             starts_with("low_green_p") ~ "Low Greenspace",
             starts_with("low_edu") ~ "Low Education")

saveRDS(tbl_multi_interaction_RR_diffs4, "2-code/p99_RR_by_city_multi_interaction_RR_diffs4.rds")

# Difference in RRs table (baseline: best case)
tbl_multi_interaction_RR_diffs4 <- df_mi_fit2 |> 
  pivot_wider(values_from = c(p99_v_MMT, p99_v_p75),
              names_from = c(reference_group, prediction), names_sep = "_") |> 
  mutate(city_name = city_name,
         low_green_edu_p99_v_MMT = RR_diff_est(p99_v_MMT_model_4_fit, p99_v_MMT_model_1_fit),#"{exp(p99_v_MMT_model_2_fit - p99_v_MMT_model_6_fit)} {sqrt(p99_v_MMT_model_2_se^2 + p99_v_MMT_model_6_se^2)}"),
         low_green_p99_v_MMT  = RR_diff_est(p99_v_MMT_model_4_fit, p99_v_MMT_model_2_fit),
         low_edu_p99_v_MMT  = RR_diff_est(p99_v_MMT_model_4_fit, p99_v_MMT_model_3_fit),
         low_green_edu_p99_v_p75 = RR_diff_est(p99_v_p75_model_4_fit, p99_v_p75_model_1_fit),
         low_green_p99_v_p75  = RR_diff_est(p99_v_p75_model_4_fit, p99_v_p75_model_2_fit),
         low_edu_p99_v_p75  = RR_diff_est(p99_v_p75_model_4_fit, p99_v_p75_model_3_fit), .keep = "none") |> 
  gt(rowname_col = "city_name") |> 
  tab_header(title = "Difference of risk ratios at the 99th percentile temperature",
             subtitle = "Comparison of best case scenario to variable changes") |> 
  tab_spanner(label = "Centered at MMT", columns = ends_with("MMT")) |> 
  tab_spanner(label = "Centered at P75", columns = ends_with("p75")) |> 
  cols_label(starts_with("low_green_edu") ~ "Low Greenspace and Edu",
             starts_with("low_green_p") ~ "Low Greenspace",
             starts_with("low_edu") ~ "Low Education")

saveRDS(tbl_multi_interaction_RR_diffs4, "2-code/p99_RR_by_city_multi_interaction_RR_diffs4.rds")

# Category count table ----------------------------------------------------

df_categories <- list_df |> 
  map(\(df_city) {
    df_city |> 
      summarize(across(everything(), first), .by = nbhd_geoid) |> 
      mutate(nbhd_type = case_when(
        pct_green_low == 1 & pct_educated_low == 1 & pop_density_low == 1 ~ var_levels[1],
        pct_green_low == 1 & pct_educated_low == 1 & pop_density_low == 0 ~ var_levels[2],
        pct_green_low == 1 & pct_educated_low == 0 & pop_density_low == 1 ~ var_levels[3],
        pct_green_low == 1 & pct_educated_low == 0 & pop_density_low == 0 ~ var_levels[4],
        pct_green_low == 0 & pct_educated_low == 1 & pop_density_low == 1 ~ var_levels[5],
        pct_green_low == 0 & pct_educated_low == 1 & pop_density_low == 0 ~ var_levels[6],
        pct_green_low == 0 & pct_educated_low == 0 & pop_density_low == 1 ~ var_levels[7],
        pct_green_low == 0 & pct_educated_low == 0 & pop_density_low == 0 ~ var_levels[8],
        .default = NA
      ) |> factor(levels = var_levels))
  }) |> list_rbind()
  

tbl_cat_count <- df_categories |> 
  group_by(city_name, nbhd_type, .drop = FALSE) |> 
  summarize(count = n()) |> 
  pivot_wider(values_from = count, names_from = city_name) |> 
  gt(rowname_col = "nbhd_type") |> 
  tab_header(title = "Count of observed combinations of categories by city") |> 
  gtExtras::gt_highlight_rows(rows = c(2, 7)) |> 
  tab_footnote(footnote = "Expected worst case scenario.",
               locations = cells_stub(rows = c(2))) |> 
  tab_footnote(footnote = "Expected best case scenario.",
               locations = cells_stub(rows = c(7))) |> 
  tab_options(footnotes.marks = "letters")

saveRDS(tbl_cat_count, "nbhd_category_count_by_country.rds")
gtsave(tbl_cat_count, "nbhd_category_count_by_country.html")


# Categories by count (no pop density) ------------------------------------

df_categories2 <- list_df |> 
  map(\(df_country) {
    df_country |> 
      summarize(across(everything(), first), .by = nbhd_geoid) |> 
      mutate(nbhd_type = case_when(
        pct_green_low == 1 & pct_educated_low == 1 ~ var_levels2[1],
        pct_green_low == 1 & pct_educated_low == 0 ~ var_levels2[2],
        pct_green_low == 0 & pct_educated_low == 1 ~ var_levels2[3],
        pct_green_low == 0 & pct_educated_low == 0 ~ var_levels2[4],
        .default = NA
      ) |> factor(levels = var_levels2))
  }) |> list_rbind()

tbl_cat_count2 <- df_categories2 |> 
  group_by(city_name, nbhd_type, .drop = FALSE) |> 
  summarize(count = n()) |> 
  pivot_wider(values_from = count, names_from = city_name) |> 
  gt(rowname_col = "nbhd_type") |> 
  tab_header(title = "Count of observed combinations of categories by city") |> 
  gtExtras::gt_highlight_rows(rows = c(1, 4)) |> 
  tab_footnote(footnote = "Expected worst case scenario.",
               locations = cells_stub(rows = c(1))) |> 
  tab_footnote(footnote = "Expected best case scenario.",
               locations = cells_stub(rows = c(4))) |> 
  tab_options(footnotes.marks = "letters")

saveRDS(tbl_cat_count2, "nbhd_category_count_by_country2.rds")
gtsave(tbl_cat_count2, "nbhd_category_count_by_country2.html")
