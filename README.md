# Naples Rent Repricing in R

Public code portfolio based on my MSc thesis in Economics and Finance at the University of Naples Federico II.

## Research question

This project studies how the spatial structure of residential rental valuations in Naples changed between 2016 and 2024, with particular attention to the sharp flattening of the rent gradient in 2019H2 and its relationship with pre-policy exposure to the Italian *Reddito di Cittadinanza* (RdC).

The RdC analysis is not presented as a causal estimate of programme receipt on rents. A central identification issue is that areas with higher predicted RdC exposure also had substantially lower initial rental valuations.

## Data

The analysis combines several public statistical and administrative sources:

- **Agenzia delle Entrate – OMI**: semiannual residential rent and sale-price quotation ranges, 2016H1–2024H1.
- **ISTAT 2011 Population and Housing Census**: census-section socioeconomic and housing characteristics.
- **ISTAT 8milaCensus**: local indicators used to construct a pre-policy socioeconomic exposure measure.
- **ISTAT 2021 Census** and **Comune di Napoli administrative RdC data**: used for an external validation exercise at Municipality level.

Raw source files are not redistributed in this repository. See [`data/README.md`](data/README.md) for data-handling notes.

## Empirical structure

The core balanced panel contains **59 OMI zones observed over 17 semesters (2016H1–2024H1)**. OMI residential quotation ranges are converted to midpoint measures and semiannual log changes. The 2018H1 rental difference is excluded because the OMI rental surface convention changes between the adjacent semesters.

The main RdC exposure measure is constructed entirely from 2011 characteristics:

```text
RdC exposure = standardised equal-weight index of:
  - renter share
  - low employment
  - potential economic distress
```

The main dynamic specification estimates a separate exposure slope in each semester while absorbing OMI-band-by-semester effects and clustering standard errors by zone. A central robustness specification additionally controls for average 2018 log rent to address the strong relationship between RdC exposure and initially low rental valuations.

## Selected findings

The analysis documents a marked compression of the Naples residential rent gradient in 2019H2, driven by stronger OMI rental-valuation growth in initially cheaper outer areas. A comparable spatial restructuring is not observed in OMI sale-price valuations.

Higher pre-policy RdC exposure is strongly associated with 2019H2 rental repricing within OMI bands. However, the estimated association attenuates substantially after controlling for pre-policy rent levels. The evidence therefore supports a spatial alignment between RdC exposure and the 2019 repricing episode, but does not separately identify a causal RdC effect.

The rent gradient subsequently re-steepens in 2023H2, again without an analogous spatial change in sale-price valuations.

## Repository structure

Two versions of the code are included:

```text
naples-rent-repricing-r/
├── README.md
├── .gitignore
├── data/
│   └── README.md
├── R/                         # concise portfolio version
│   ├── 01_build_panel.R
│   ├── 02_build_rdc_exposure.R
│   ├── 03_main_models.R
│   ├── 04_external_validation.R
│   └── 05_visualisation.R
└── full_pipeline/             # original V10 thesis scripts
    ├── 00_build_data.R
    ├── 01_descriptive_audit.R
    ├── 02_rdc_exposure_build.R
    ├── 03_main_models.R
    ├── 04_omi_measurement_validation.R
    ├── 05_robustness_spatial.R
    ├── 06_final_tables_figures.R
    └── 07_rdc_exposure_external_validation.R
```

The [`R/`](R/) directory is a shorter public-facing version highlighting the main programming and analytical workflow. The [`full_pipeline/`](full_pipeline/) directory contains the original V10 scripts used for the thesis analysis.

## Full V10 pipeline

The original scripts are sequential:

1. **`00_build_data.R`** – imports and validates OMI files, constructs the 59-zone × 17-semester panel, spatially aggregates ISTAT 2011 census sections to OMI zones, and creates geography/adjacency outputs.
2. **`01_descriptive_audit.R`** – documents the 2019H2 flattening and 2023H2 re-steepening, cross-zone dispersion, rank stability, distance gradients and pre-break diagnostics.
3. **`02_rdc_exposure_build.R`** – constructs the pre-policy RdC exposure index from renter share, low employment and economic distress, plus alternative indices for robustness.
4. **`03_main_models.R`** – estimates the main spatial-gradient and dynamic-exposure specifications, HC3 and zone-clustered inference, initial-rent adjustments, sale-price benchmarks and alternative exposure models.
5. **`04_omi_measurement_validation.R`** – audits OMI updating patterns across endpoints, property types and residential/non-residential segments.
6. **`05_robustness_spatial.R`** – implements lower/mid/upper-bound checks, 59-vs-60-zone comparisons, leave-one-out tests, boundary sensitivity, Moran diagnostics and within-band permutation inference.
7. **`06_final_tables_figures.R`** – freezes the empirical results and assembles the final thesis tables, figures, claims and caveats without estimating new models.
8. **`07_rdc_exposure_external_validation.R`** – validates the 2011-based exposure measure against later administrative RdC incidence across Naples' ten Municipalities.

## Technical features demonstrated

- R data wrangling with `dplyr`, `tidyr` and `stringr`
- spatial processing and census-to-market-zone aggregation with `sf`
- heterogeneous CSV/ZIP/Excel input handling and encoding cleanup
- data-quality assertions, duplicate checks and coverage audits
- construction and standardisation of composite indicators
- balanced panel construction and first differences
- OLS and HC3 robust inference
- fixed effects and zone-clustered inference with `fixest`
- spatial residual diagnostics and permutation inference
- leave-one-out and sample-sensitivity analysis
- publication-oriented visualisation with `ggplot2`

## Reproducibility note

The code expects the official source files in the local input structure documented in the scripts. Raw OMI, ISTAT and administrative source files are deliberately not committed to this repository. Consequently, the full pipeline is provided for transparency and code review, while complete execution requires downloading the corresponding official source data.

OMI observations are semiannual appraisal-based quotation intervals rather than individual observed rental contracts. The code and interpretation preserve this distinction throughout.

## Author

**Giovanni Annarelli**  
MSc Economics and Finance, University of Naples Federico II
