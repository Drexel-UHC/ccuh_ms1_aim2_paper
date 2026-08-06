# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Epidemiological analysis of temperature-mortality associations across five cities (Philadelphia PA Metro, Rio de Janeiro, São Paulo, Panama City, Guatemala City), examining how neighborhood-level greenspace and education modify heat-related mortality risk. Uses distributed lag non-linear models (DLNM) within conditional quasi-Poisson frameworks (gnm package).

## Environment Setup

```r
renv::activate()
renv::restore()
```

R version: 4.5.2. All dependencies managed via renv.

## Analysis Pipeline

Scripts run sequentially (each depends on outputs of previous steps):

1. `00_import_data.R` - Loads parquet data from CCUH API (requires network access to UNC paths), creates neighborhood types via median splits of greenspace/education, saves RDS files to `data/`
2. `01_fit_models.R` - Fits conditional quasi-Poisson DLNM models (overall + interaction with 4-level neighborhood type), saves model components to `models/`
3. `02_RRs.R` - Extracts relative risks at 99th percentile temperature, calculates RR ratios, saves CSVs to `results/`
4. `03_figures.R` - Generates RR plots and temperature-mortality curves (PDFs to `results/` and `sup_results/`)
5. `04_spatial_figures.R` - Creates neighborhood maps using sf boundaries (PDFs to `results/`)
6. `05_tables.R` - Generates Table 1 and Supplemental Tables S1-S3 with gt (HTML to `results/`)
7. `06_compare_runs.R` - Assembles one table of p99 estimates across every staged run present in `results/`

`run_config.R` holds the run tag and per-city year windows (runs A-E of issue #18). Every artifact written by `00`-`05` is suffixed with the run tag; select a run with `CCUH_RUN_TAG`. Run A is the preserved `v_0_1_x` published output and cannot be rebuilt from the current bundle.

`exploratory/` holds `S1_fit_model_tertiles.R` and `E1_exploratory_data_analysis.qmd`. Neither is part of the reported pipeline and both are known broken - see their header comments.

## Architecture

**Data flow:** Parquet (CCUH API) → RDS (data/) → model components (models/) → CSVs + PDFs + HTML (results/, sup_results/)

**Data structures:** City-stratified lists throughout. `list_df_city` holds daily data per city; `list_model_city` and `list_model_city_int` hold model coefficients, vcov matrices, cross-basis objects, and MMT per city.

**Model specification:**
- Cross-basis: natural splines for temperature (knots at 10th, 75th, 90th percentiles) and lag (21-day lag, 3 log-spaced knots)
- Strata elimination via `gnm()` for conditional likelihood (strata = neighborhood × age × year × month × DOW)
- Interaction models test 4-level neighborhood type (low/high green × low/high edu) as effect modifier

**helper_functions.R** contains shared utilities:
- `city_to_factor()` - standardized city name ordering
- `convert_to_label()` - snake_case to Title Case factor conversion
- `RR_ratio_est()` - ratio of two RRs with delta method CIs
- `slim_crossbasis()` - strips a cross-basis down to the attributes `crossreduce()` reads
- `format_number()`, `get_percentiles()` - formatting helpers

## Key Packages

- **gnm** - conditional quasi-Poisson models
- **dlnm** - distributed lag non-linear models (crossbasis, crossreduce)
- **sf** - spatial data and maps
- **gt** - HTML table formatting
- **patchwork** - multi-panel figure composition
- **arrow** - parquet data import
- **SALURhelper** - project-specific helpers (get_MMT, get_df_RRs)

## Folder Structure

- `data/` - processed RDS data files
- `models/` - fitted model components (coefficients, vcov, cross-basis attributes)
- `results/` - main figures (PDF), tables (HTML), and summary CSVs
- `sup_results/` - supplementary figures
- `exploratory/` - analyses outside the reported pipeline (not maintained)
- `2-legacy-code/` - archived earlier analysis scripts (not in active use)

## Conventions

- Scripts prefixed with numbers (00-06) for pipeline ordering; S-prefix for supplementary analyses; E-prefix for exploratory
- Models stored as component lists (coef, vcov, basis attributes), not full gnm objects, to reduce file size
- Gap-filled days carry `death = NA` and are excluded from fits and descriptives, but keep their temperature so lags never span a gap
- Neighborhood types defined by within-city median splits of `pct_green` and `pct_educated`
- Color palette for neighborhood types: `rev(RColorBrewer::brewer.pal(4, "BrBG"))`
- Cities filtered to exclude Belo Horizonte and Campinas
- Age collapsed to two categories: <65 and 65+
