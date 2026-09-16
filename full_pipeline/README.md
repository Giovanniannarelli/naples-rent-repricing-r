# Original V10 thesis pipeline

This folder contains the complete R scripts used for the current empirical version of my MSc thesis. They are included in their original working form, including detailed checks, audit messages and some comments written in Italian.

The scripts are designed to be run sequentially from `00` to `07`. Raw data are intentionally not stored in this public repository, so a full rerun requires the official source files described in [`../data/README.md`](../data/README.md).

## Execution order

| Script | Role |
| --- | --- |
| `00_build_data.R` | Imports OMI and ISTAT data, performs cleaning and spatial matching, constructs the balanced 59-zone × 17-semester panel and writes the canonical master dataset. |
| `01_descriptive_audit.R` | Audits the empirical pattern before the main models: rent-gradient changes, dispersion, rank stability, initial-level diagnostics and pre-2019 dynamics. |
| `02_rdc_exposure_build.R` | Builds the pre-policy RdC exposure index from 2011 renter share, low employment and economic distress, plus alternative indices for robustness. |
| `03_main_models.R` | Estimates the main rent-gradient and dynamic RdC-exposure specifications, sale-price benchmarks, initial-rent adjustments and the 2023 analysis. |
| `04_omi_measurement_validation.R` | Tests whether the 2019/2023 patterns could reflect OMI measurement or discrete-update features by comparing endpoints, property types and markets. |
| `05_robustness_spatial.R` | Runs endpoint, distance-gradient, 59-vs-60-zone, leave-one-out, boundary, Moran and within-band permutation diagnostics. |
| `06_final_tables_figures.R` | Does not estimate new models; it freezes previously generated results and assembles thesis-ready tables, figures, claims and caveats. |
| `07_rdc_exposure_external_validation.R` | Externally validates the 2011-based exposure index against later Municipality-level administrative RdC incidence. It does not alter the frozen main estimates. |

## Important interpretation note

The code does **not** treat the RdC association as a separately identified causal effect on rents. The main identification problem is the strong relationship between predicted RdC exposure and initially low rental valuations. The public repository retains this limitation explicitly rather than selecting specifications according to statistical significance.

## Public vs. full code

The [`../R`](../R) folder contains shorter, recruiter-friendly extracts highlighting the main data-management and econometric tasks. This `full_pipeline` folder preserves the complete V10 research workflow so that the underlying programming work can be inspected in detail.
