# Labeller for nbhd types or changes in nbhds
label_nbhd_change <- c("Low Edu", "Low Green", "Low Green Low Edu")
label_nbhd_type   <- c("Low Green Low Edu", "Low Green High Edu", "High Green Low Edu", "High Green High Edu")

convert_to_label <- function(string, label_order) {
  string |> 
    str_replace_all("_", " ") |> 
    str_to_title() |> 
    factor(levels = label_order)
  
}

# Calculate estimate of ratio of RRs for two given nbhd types
RR_ratio_est <- function(fit1, fit2, se1, se2) {
  c(RR_ratio = exp(fit1 - fit2),
    RR_ratio_low = exp(fit1 - fit2 - qnorm(.975)*sqrt(se1^2 + se2^2)),
    RR_ratio_high = exp(fit1 - fit2 + qnorm(.975)*sqrt(se1^2 + se2^2)))
}