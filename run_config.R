# Configuration shared by 00-06. Every artifact these scripts write is tagged with
# `run_tag` so the staged runs of #18 Phase 3 coexist on disk and can be diffed.
# Switch runs without editing this file: Sys.setenv(CCUH_RUN_TAG = "E_v0_2_0_pre_2020")
run_tag <- Sys.getenv("CCUH_RUN_TAG", unset = "D_v0_2_0_all_years")

# Frozen data bundle read by 00_import_data.R
dir_bundle <- "//files.drexel.edu/colleges/SOPH/Shared/UHC/Projects/CCUH/ccuh-server/freeze/_datawarehouse/v_0_2_0"

# Years analysed per city; NULL keeps every year the bundle provides
list_run_years <- list(
  # Run B: v_0_2_0 held to the published (Run A) windows, so only the data revision changes
  B_v0_2_0_same_years = list(
    "Philadelphia PA Metro" = 2000:2022,
    "Rio de Janeiro" = 2003:2019,
    "São Paulo" = 2003:2019,
    # 2019 held out to match Run A; exercises the gap fill below deliberately
    "Panama City" = c(2012:2018, 2020),
    "Guatemala City" = 2009:2020
  ),
  # Run C: B plus the restored Panama City 2019, which closes the gap
  C_v0_2_0_pa_2019 = list(
    "Philadelphia PA Metro" = 2000:2022,
    "Rio de Janeiro" = 2003:2019,
    "São Paulo" = 2003:2019,
    "Panama City" = 2012:2020,
    "Guatemala City" = 2009:2020
  ),
  # Run D: every year the bundle provides (candidate new primary)
  D_v0_2_0_all_years = NULL,
  # Run E: Run D without the pandemic years
  E_v0_2_0_pre_2020 = list(
    "Philadelphia PA Metro" = 2000:2019,
    "Rio de Janeiro" = 2003:2019,
    "São Paulo" = 2003:2019,
    "Panama City" = 2007:2019,
    "Guatemala City" = 2009:2019
  )
)

if (!run_tag %in% names(list_run_years)) {
  stop(
    "Unknown CCUH_RUN_TAG '",
    run_tag,
    "'. Choose one of: ",
    paste(names(list_run_years), collapse = ", "),
    ". Run A is the preserved v_0_1_x published output and is not re-fit."
  )
}

run_years <- list_run_years[[run_tag]]
