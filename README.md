# CCUH Aim 2 Paper

## Scripts

### Main Analysis:

- `00_import_data.R`: Pull data from CCUH API and apply basic formatting. Save formatted data to `data` folder.
- `01_fit_models.R`: Fit conditional quasi-Poisson models for overall & interaction analyses.
- `02_RRs.R`: Estimate the RRs at the 99th temperature percentile for cities and their neighborhood types.
- `03_figures.R`: Create figures from the analysis.
- `04_spatial_figures.R`: Create plot of neighborhood types for each city with spatial boundaries.
- `05_tables.R`: Creates all tables.
- `helper_functions.R`: Hosts additional functions used to make code cleaner.

### Extra Analysis

- `E1_exploratory_data_analysis.qmd`: A notebook that generates exploratory plots of our response, modifiers, and Country level statistics.

## Folders

- `data`: A folder to store formatted data originally pulled from the CCUH API.
- `models`: A folder to store fitted models and intermediary results.
- `results`: A folder for final figures and tables of the analysis.
- `sup_results`: A folder for supplemental figures and tables of the analysis.
- `renv`: A folder that activates `renv`, ensuring compatible R and package versions are used.
- `2-legacy-code`: A folder of legacy code that is being adapted in scripts.
