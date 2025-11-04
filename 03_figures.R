library(tidyverse)
library(patchwork)
library(glue)
library(gnm); library(dlnm); library(splines)
library(SALURhelper)

# ggplot2 theme
set_theme(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5))

# Load models
list_city_df <- readRDS("data/list_city_df.rds")
list_city_model <- readRDS("models/list_city_model.rds")
list_city_int_model <- readRDS("models/list_city_int_model.rds")

# Overall city curves -----------------------------------------------------

# Get overall RRs at values of pred_seq
list_city_pred <- list_city_model |> 
  map(\(city_model) {
    
    # Estimate RRs at 99th temperature percentile
    city_pred <- 
      crossreduce(city_model$cbt,
                  coef = city_model$coef,
                  vcov = city_model$vcov,
                  model.link = city_model$link,
                  at = city_model$pred_seq,
                  cen = city_model$MMT) # Can change between p75 and MMT
    
    names(city_pred)
    
    # Format df to return
    get_df_RRs(city_pred)
  })

# Get min & max RR for all cities for fixed y-axis
y_lims <- list_city_pred |> 
  map(\(x) range(c(x$RR_low, x$RR_high))) |> 
  unlist() |> 
  range()

# Temperature mortality curve plots
list_TMC_plot <- 
  pmap(list(list_city_pred, list_city_df, names(list_city_df)),
       \(city_pred, df_city, city_name) {
         
         x_lims <- range(df_city$popwt_temp)
       
         # Overall curve TMC
         p1 <- city_pred |> 
           ggplot(aes(exposure)) +
           geom_ribbon(aes(ymin = RR_low, ymax = RR_high), alpha = .2) +
           geom_line(aes(y = RR)) +
           geom_hline(yintercept = 1) +
           coord_transform(y = "log") +
           scale_x_continuous(limits = x_lims) +
           scale_y_continuous(limits = y_lims) +
           labs(title = glue("{city_name}"),
                x = NULL, y = "RR") 
         
         # Temperature histogram
         p2 <- df_city |> 
           summarise(across(everything(), first), .by = date) |> 
           ggplot(aes(popwt_temp)) +
           geom_histogram(alpha = .2, bins = 35) +
           scale_x_continuous(limits = x_lims) +
           labs(title = "",
                x = "Temperature (°C)",
                y = "Count")
         
         # Combine plots w/ patchwork
         p1 / p2
     })


wrap_plots(list_TMC_plot, nrow = 2)


# Neighborhood type city curves -------------------------------------------

# Get interaction RRs at values of pred_seq
list_city_int_pred <-
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
                           at = city_model$pred_seq,
                           cen = city_model$MMT) 
             
             # Format df to return
             get_df_RRs(city_pred)
           }) |> 
           list_rbind(names_to = "nbhd_type")
       })

# Temperature mortality curve plots
list_TMC_int_plot <- 
  pmap(list(list_city_int_pred, list_city_df, names(list_city_df)),
       \(city_int_pred, df_city, city_name) {
         
         x_lims <- range(df_city$popwt_temp)
         
         # Overall curve TMC
         p1 <- city_int_pred |> 
           ggplot(aes(exposure, color = nbhd_type, fill = nbhd_type)) +
           geom_ribbon(aes(ymin = RR_low, ymax = RR_high), alpha = .2) +
           geom_line(aes(y = RR)) +
           geom_hline(yintercept = 1) +
           coord_transform(y = "log") +
           scale_x_continuous(limits = x_lims) +
           #scale_y_continuous(limits = y_lims) +
           labs(title = glue("{city_name}"),
                x = NULL, y = "RR") 
         
         # Temperature histogram
         p2 <- df_city |>
           summarise(across(everything(), first), .by = date) |>
           ggplot(aes(popwt_temp)) +
           geom_histogram(alpha = .2, bins = 35) +
           scale_x_continuous(limits = x_lims) +
           labs(title = "",
                x = "Temperature (°C)",
                y = "Count")
         
         # Combine plots w/ patchwork
         p1 / p2
       })

wrap_plots(list_TMC_int_plot, nrow = 2, guides = "collect")