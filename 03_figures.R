library(tidyverse)
library(patchwork)
library(glue)
library(gnm)
library(dlnm)
library(splines)
library(SALURhelper)

# ggplot2 theme
set_theme(theme_bw())
theme_update(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)
)

# Load helper functions
source("helper_functions.R")

# Load models
list_df_city <- readRDS("data/list_df_city.rds")
list_model_city <- readRDS("models/list_model_city.rds")
list_model_city_int <- readRDS("models/list_model_city_int.rds")
df_RR_ratio <- read_csv("results/RR_ratio_p99.csv")


# Ratio of RRs ------------------------------------------------------------

df_RR_ratio |>
  filter(!(city_name %in% c("Guatemala City", "Panama City"))) |>
  pivot_longer(contains("low_")) |>
  pivot_wider(values_from = value, names_from = estimate) |>
  mutate(name = convert_to_label(name, label_nbhd_change)) |>
  ggplot(aes(x = name, color = name)) +
  geom_hline(yintercept = 1) +
  geom_errorbar(aes(ymin = RR_ratio_low, ymax = RR_ratio_high), width = .5) +
  geom_point(aes(y = RR_ratio), size = 3) +
  scale_y_continuous(trans = "log") +
  scale_color_brewer(type = "qual", direction = -1) +
  facet_wrap(~city_name, ncol = 5) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(color = "", x = "", y = "Ratio RRs")

ggsave("results/figure_RR_ratios.pdf", units = "cm", width = 25, height = 10)

# Overall city curves -----------------------------------------------------

# Get overall RRs at values of pred_seq
list_pred_city <- list_model_city |>
  map(\(model_city) {
    # Estimate RRs at 99th temperature percentile
    pred_city <-
      crossreduce(
        model_city$cbt,
        coef = model_city$coef,
        vcov = model_city$vcov,
        model.link = model_city$link,
        at = model_city$pred_seq,
        cen = model_city$p75
      ) # Can change between p75 and MMT

    # Format df to return
    get_df_RRs(pred_city)
  })

# Get fixed limits for x & y axis
y_lims <- list_pred_city |>
  map(\(x) range(c(x$RR_low, x$RR_high))) |>
  unlist() |>
  range()

# x_lims <- list_pred_city |> 
#   map(\(x) range(c(x$exposure, x$exposure))) |> 
#   unlist() |> 
#   range()

# Temperature mortality curve plots
list_plot_TMC <-
  pmap(
    list(list_pred_city, list_df_city, names(list_df_city)),
    \(pred_city, df_city, city_name) {
      # Get city specific limits for x & y axis
      #y_lims <- range(c(pred_city$RR_low, pred_city$RR_high))
      x_lims <- range(df_city$popwt_temp)

      # Overall curve TMC
      p1 <- pred_city |>
        ggplot(aes(exposure)) +
        geom_ribbon(aes(ymin = RR_low, ymax = RR_high), alpha = .2) +
        geom_line(aes(y = RR)) +
        geom_hline(yintercept = 1) +
        coord_transform(y = "log") +
        scale_x_continuous(limits = x_lims) +
        scale_y_continuous(limits = y_lims) +
        labs(title = glue("{city_name}"), x = NULL, y = "RR")

      # Temperature histogram
      p2 <- df_city |>
        summarise(across(everything(), first), .by = date) |>
        ggplot(aes(popwt_temp)) +
        geom_histogram(alpha = .2, bins = 35) +
        scale_x_continuous(limits = x_lims) +
        labs(title = "", x = "Temperature (°C)", y = "Count")

      # Combine plots w/ patchwork
      p1 / p2
    }
  )


wrap_plots(list_plot_TMC, nrow = 2)

ggsave(
  "sup_results/figure_TMC_overall.pdf",
  units = "cm",
  width = 30,
  height = 25
)


# Neighborhood type city curves -------------------------------------------

# Get interaction RRs at values of pred_seq
list_pred_city_int <-
  map2(list_model_city, list_model_city_int, \(model_city, model_city_int) {
    # Estimate RRs at 99th temperature percentile for each nbhd type
    df_pred_city_int <- model_city_int |>
      map(\(nbhd_type_model) {
        pred_city <-
          # Use coef & vcov from interaction model instead of overall model
          crossreduce(
            model_city$cbt,
            coef = nbhd_type_model$coef,
            vcov = nbhd_type_model$vcov,
            model.link = nbhd_type_model$link,
            at = model_city$pred_seq,
            cen = model_city$p75
          )

        # Format df to return
        get_df_RRs(pred_city)
      }) |>
      list_rbind(names_to = "nbhd_type")
  })

# Temperature mortality curve plots
list_plot_TMC_int <-
  pmap(
    list(list_pred_city_int, list_df_city, names(list_df_city)),
    \(pred_city_int, df_city, city_name) {
      x_lims <- range(df_city$popwt_temp)

      # Overall curve TMC
      p1 <- pred_city_int |>
        mutate(nbhd_type = convert_to_label(nbhd_type, label_nbhd_type)) |>
        ggplot(aes(exposure, color = nbhd_type, fill = nbhd_type)) +
        geom_ribbon(aes(ymin = RR_low, ymax = RR_high), alpha = .2) +
        geom_line(aes(y = RR)) +
        geom_hline(yintercept = 1) +
        coord_transform(y = "log") +
        scale_x_continuous(limits = x_lims) +
        scale_color_brewer(type = "qual") +
        scale_fill_brewer(type = "qual") +
        labs(
          title = glue("{city_name}"),
          color = "",
          fill = "",
          x = NULL,
          y = "RR"
        )

      # Temperature histogram
      p2 <- df_city |>
        summarise(across(everything(), first), .by = date) |>
        ggplot(aes(popwt_temp)) +
        geom_histogram(alpha = .2, bins = 35) +
        scale_x_continuous(limits = x_lims) +
        labs(title = "", x = "Temperature (°C)", y = "Count")

      # Combine plots w/ patchwork
      p1 / p2
    }
  )

wrap_plots(list_plot_TMC_int, nrow = 2, guides = "collect")

ggsave(
  "sup_results/figure_TMC_nbhd_type.pdf",
  units = "cm",
  width = 30,
  height = 25
)
