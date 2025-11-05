library(tidyverse)
library(patchwork)
library(glue)
library(gnm); library(dlnm); library(splines)
library(SALURhelper)

# ggplot2 theme
set_theme(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5))

# Load helper functions
source("helper_functions.R")

# Load models
list_city_df <- readRDS("data/list_city_df.rds")
list_city_model <- readRDS("models/list_city_model.rds")
list_city_int_model <- readRDS("models/list_city_int_model.rds")
df_RR_ratio <- read_csv("results/RR_p99_ratio.csv")


# Ratio of RRs ------------------------------------------------------------

df_RR_ratio |> 
  filter(!(city_name %in% c("Guatemala City", "Panama City"))) |> 
  pivot_longer(contains("low_")) |> 
  pivot_wider(values_from = value,
              names_from = estimate) |> 
  mutate(name = convert_to_label(name, label_nbhd_change)) |> 
  ggplot(aes(x = name, color = name)) +
  geom_hline(yintercept = 1) +
  geom_errorbar(aes(ymin = RR_ratio_low,
                    ymax = RR_ratio_high), width = .5) +
  geom_point(aes(y = RR_ratio), size = 3) +
  scale_color_brewer(type = "qual", direction = -1) +
  facet_wrap(~ city_name, ncol = 5) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(color = "", x = "", y = "Ratio RRs")

ggsave("results/figure_RR_ratios.pdf", units = "cm", width = 25, height = 10)

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
                  cen = city_model$p75) # Can change between p75 and MMT
    
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

ggsave("sup_results/figure_TMC_overall.pdf", units = "cm", width = 30, height = 25)


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
                           cen = city_model$p75) 
             
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
           mutate(nbhd_type = convert_to_label(nbhd_type, label_nbhd_type)) |> 
           ggplot(aes(exposure, color = nbhd_type, fill = nbhd_type)) +
           geom_ribbon(aes(ymin = RR_low, ymax = RR_high), alpha = .2) +
           geom_line(aes(y = RR)) +
           geom_hline(yintercept = 1) +
           coord_transform(y = "log") +
           scale_x_continuous(limits = x_lims) +
           scale_color_brewer(type = "qual") +
           scale_fill_brewer(type = "qual") +
           labs(title = glue("{city_name}"),
                color = "", fill = "",
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

ggsave("sup_results/figure_TMC_nbhd_type.pdf", units = "cm", width = 30, height = 25)