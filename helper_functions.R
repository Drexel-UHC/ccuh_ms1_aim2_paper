# Create ordered factor of city names
city_to_factor <- function(city_name) {
  factor(
    replace_values(city_name, "Sao Paulo" ~ "São Paulo"),
    levels = c(
      "Philadelphia PA Metro",
      "Rio de Janeiro",
      "São Paulo",
      "Panama City",
      "Guatemala City"
    )
  )
}

# Labeller for nbhd types or changes in nbhds
label_nbhd_change <- c("Low Edu", "Low Green", "Low Green Low Edu")
label_nbhd_type <- c(
  "Low Green Low Edu",
  "Low Green High Edu",
  "High Green Low Edu",
  "High Green High Edu"
)

convert_to_label <- function(string, label_order) {
  string |>
    str_replace_all("_", " ") |>
    str_to_title() |>
    factor(levels = label_order)
}

# Calculate estimate of ratio of RRs for two given nbhd types
RR_ratio_est <- function(fit1, fit2, se1, se2) {
  c(
    RR_ratio = exp(fit1 - fit2),
    # the second part here is the equivalent to the usual +-1.96 * SE, only the see here is
    # calculated using the delta method for the difference of two independent estimates sqrt(var1-var2)
    RR_ratio_low = exp(fit1 - fit2 - qnorm(.975) * sqrt(se1^2 + se2^2)),
    RR_ratio_high = exp(fit1 - fit2 + qnorm(.975) * sqrt(se1^2 + se2^2))
  )
}

# Helper function for labeling
format_number <- function(num, digits = 1) {
  format(round(num, digits), nsmall = digits)
}

# Extract median, 10th and 90th percentiles for table
get_percentiles <- function(x, digits = 1) {
  glue(
    "{med} ({low}, {high})",
    med = quantile(x, .5) |> format_number(digits),
    low = quantile(x, .1) |> format_number(digits),
    high = quantile(x, .9) |> format_number(digits)
  )
}
