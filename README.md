# CCUH Aim 2 Paper

Neighborhood greenspace and education as modifiers of heat-related mortality in five
cities of the Americas (Philadelphia PA Metro, Rio de Janeiro, São Paulo, Panama City,
Guatemala City), using distributed lag non-linear models within a conditional
quasi-Poisson framework.

## Setup

R 4.5.2. All package versions are pinned with `renv`:

```r
renv::restore()
```

`00_import_data.R` and `04_spatial_figures.R` read parquet files from the Drexel
network share (`//files.drexel.edu/colleges/SOPH/...`) and will fail without access to
it. Every other script runs from the RDS/CSV files those two produce.

## Runs

`run_config.R` defines a **run tag** and the per-city year windows for that run. Every
artifact written by `00`–`05` is suffixed with the tag, so runs coexist on disk and can
be diffed (`data/`, `models/`, `results/` and `sup_results/` are gitignored, so an
overwrite is not recoverable from git).

| Run tag | What it is |
|---|---|
| `A_v0_1_x_published` | Data bundle `v_0_1_0`/`v_0_1_1`, as published. Preserved artifacts only — not reproducible from the current scripts. |
| `B_v0_2_0_same_years` | Bundle `v_0_2_0` held to Run A's year windows (Panama City 2019 held out), isolating the data revision. |
| `C_v0_2_0_pa_2019` | B plus the restored Panama City 2019. |
| `D_v0_2_0_all_years` | Every year the bundle provides (default). |
| `E_v0_2_0_pre_2020` | D without 2020–2022. |

Select a run without editing the file:

```sh
CCUH_RUN_TAG=E_v0_2_0_pre_2020 Rscript 00_import_data.R
```

## Scripts

Run in order; each depends on the outputs of the previous ones.

1. `00_import_data.R` — pull the bundle, apply the run's year windows, build the
   neighborhood profiles, fill date gaps and assert every neighborhood × age series is
   contiguous. Writes `data/`.
2. `01_fit_models.R` — fit the overall and 4-profile interaction DLNMs. Writes `models/`.
3. `02_RRs.R` — relative risks at the 99th temperature percentile and their ratios.
   Writes `results/RR_p99_*.csv`.
4. `03_figures.R` — relative-risk figures and temperature–mortality curves.
5. `04_spatial_figures.R` — neighborhood maps.
6. `05_tables.R` — Table 1 and Supplemental Tables S1–S3.
7. `06_compare_runs.R` — one table of the p99 estimates across every run present in
   `results/`. Writes `results/comparison_runs.{csv,html}`.

`helper_functions.R` holds shared utilities; `run_config.R` holds the run configuration.

## Where each reported number comes from

`{tag}` is the run tag.

| Manuscript item | Script | Output |
|---|---|---|
| Table 1 | `05_tables.R` | `results/table_descriptive_{tag}.html` |
| Figure 1 (profile maps) | `04_spatial_figures.R` | `results/figures_spatial_{tag}.pdf` |
| Figure 2 (RRs by profile) | `03_figures.R` | `results/figure_RRs_{tag}.pdf` |
| Figure 3 (ratios of RRs) | `03_figures.R` | `results/figure_RR_ratios_{tag}.pdf` |
| Supplemental Table S1 | `05_tables.R` | `results/table_S1_count_nbhd_type_{tag}.html` |
| Supplemental Table S2 | `05_tables.R` | `results/table_S2_RR_nbhd_type_{tag}.html` |
| Supplemental Table S3 | `05_tables.R` | `results/table_S3_RR_ratio_{tag}.html` |
| Supplemental Figure S1 (education) | `04_spatial_figures.R` | `sup_results/figures_spatial_edu_{tag}.pdf` |
| Supplemental Figure S2 (greenspace) | `04_spatial_figures.R` | `sup_results/figures_spatial_green_{tag}.pdf` |
| Supplemental Figure S3 (temperature & TMC) | `03_figures.R` | `sup_results/figure_TMC_overall_{tag}.pdf` |
| Not reported: TMC by profile | `03_figures.R` | `sup_results/figure_TMC_nbhd_type_{tag}.pdf` |
| Not reported: temperature percentiles, death totals | `05_tables.R` | `results/p75_p99_by_city_{tag}.csv`, `results/total_deaths_{tag}.csv` |

## Folders

- `data`: formatted data pulled from the CCUH data warehouse.
- `models`: fitted model components (coefficients, vcov, cross-basis attributes).
- `results`: figures and tables reported in the manuscript.
- `sup_results`: supplemental figures.
- `exploratory`: analyses that are **not** part of the reported pipeline. Both scripts
  there are known to be broken; see their header comments.
- `renv`: activates `renv`, ensuring compatible R and package versions are used.
- `2-legacy-code`: earlier analysis code, not in active use.
