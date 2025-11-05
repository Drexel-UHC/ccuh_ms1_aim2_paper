# CCUH Aim 2 Paper

## Scripts

### Main Analysis:

- `import_data.R`: Pull data from CCUH API and apply basic formatting. Save formatted data to `data` folder.
- `fit_models.R`: Fit conditional quasi-Poisson models for overall & interaction analyses.
- `RRs.R`: Estimate the RRs at the 99th temperature percentile for cities and their neighborhood types.
- `figures.R`: Create figures from the analysis.
- `spatial_figures.R`: Create plot of neighborhood types for each city with spatial boundaries.
- `helper_functions.R`: Hosts additional functions used to make code cleaner.

### Extra Analysis

- `exploratory_data_analysis.R`: A notebook that generate exploratory plots of our response, modifiers, and Country level statistics. 

## Folders

- `data`: A folder to store formatted data originally pulled from the CCUH API.
- `models`: A folder to store fitted models and intermediary results.
- `results`: A folder for final figures and tables of the analysis.
- `sup_results`: A folder for supplemental figures and table of the analysis.
- `2-legacy-code`: A folder of legacy code that is being adapted in scripts.