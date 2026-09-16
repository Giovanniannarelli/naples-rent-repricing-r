# Naples Rent Repricing in R

Public code portfolio based on my MSc thesis in Economics and Finance at the University of Naples Federico II.

## Research question

The project studies how the spatial structure of residential rental valuations in Naples changed between 2016 and 2024, with particular attention to the sharp flattening of the rent gradient in 2019H2 and its relationship with pre-policy exposure to the Italian *Reddito di Cittadinanza* (RdC).

The empirical analysis is descriptive and econometric rather than a causal estimate of the effect of the RdC on rents. A central identification issue is that areas with higher predicted RdC exposure also had lower initial rental valuations.

## Data

The analysis combines several public statistical and administrative sources:

- **Agenzia delle Entrate – OMI**: semiannual residential rent and sale-price quotation ranges, 2016H1–2024H1.
- **ISTAT 2011 Population and Housing Census**: census-section socioeconomic and housing characteristics.
- **ISTAT 8milaCensus**: local indicators used to construct a pre-policy socioeconomic exposure measure.
- **ISTAT 2021 Census** and **Comune di Napoli administrative RdC data**: used for an external validation exercise at Municipality level.

Raw source files are not redistributed in this repository. See [`data/README.md`](data/README.md) for the expected inputs and data-handling notes.

## What the code does

The original thesis codebase is organised as a sequential R pipeline. The public version highlights the parts most relevant to data management, statistical analysis and reproducibility:

1. **Build and validate the panel** – import heterogeneous OMI/ISTAT files, clean encodings and numeric formats, construct a balanced zone-semester panel, perform spatial joins and coverage checks.
2. **Construct RdC exposure** – aggregate pre-policy census characteristics to OMI zones and build standardised exposure indices.
3. **Estimate the main models** – analyse the evolution of the spatial rent gradient and estimate semester-specific within-band associations between RdC exposure and rental-valuation growth.
4. **Robustness and validation** – compare rent and sale valuations, test alternative exposure definitions, perform spatial and permutation diagnostics, and validate the exposure index against later administrative data.
5. **Visualise results** – produce publication-oriented plots for gradient changes, dynamic coefficients and sensitivity to initial-rent controls.

## Main empirical structure

The core balanced panel contains **59 OMI zones observed over 17 semesters (2016H1–2024H1)**. OMI residential quotations are converted to midpoint measures and semiannual log changes. The 2018H1 rental difference is excluded because of a change in the OMI surface convention.

The main RdC exposure measure is built entirely from 2011 characteristics:

```text
RdC exposure = z-score average of:
  - renter share
  - low employment
  - potential economic distress
```

The main dynamic specification estimates a separate exposure slope in each semester while absorbing OMI-band-by-semester effects and clustering standard errors by zone.

A key robustness step adds average 2018 log rent as a fully pre-policy control for the strong correlation between RdC exposure and initially low rental valuations.

## Selected findings

The code documents a marked compression of the Naples residential rent gradient in 2019H2, driven by stronger OMI rental-valuation growth in initially cheaper outer areas. A comparable spatial restructuring is not observed in OMI sale-price valuations.

Higher pre-policy RdC exposure is strongly associated with 2019H2 rental repricing within OMI bands. However, the estimated association attenuates substantially after controlling for pre-policy rent levels, so the analysis does **not** interpret the result as a separately identified causal effect of the RdC.

## Technical features demonstrated

- R data wrangling with `dplyr`, `tidyr` and `stringr`
- spatial processing and census-to-market-zone aggregation with `sf`
- data-quality assertions, duplicate checks and coverage audits
- construction and standardisation of composite indicators
- balanced panel construction and first differences
- OLS, HC3 robust inference and zone-clustered inference
- fixed effects with `fixest`
- permutation tests and spatial residual diagnostics
- publication-oriented visualisation with `ggplot2`

## Repository structure

```text
naples-rent-repricing-r/
├── README.md
├── .gitignore
├── data/
│   └── README.md
└── R/
    ├── 01_build_panel.R
    ├── 02_build_rdc_exposure.R
    ├── 03_main_models.R
    ├── 04_external_validation.R
    └── 05_visualisation.R
```

## Reproducibility note

This repository is a cleaned public portfolio version of a larger thesis codebase. It preserves the actual data-management and econometric workflow while omitting raw data files and thesis-specific output folders. The original analysis also contains additional measurement audits, leave-one-out checks, endpoint robustness, Moran diagnostics and within-band permutation inference.

## Author

**Giovanni Annarelli**  
MSc Economics and Finance, University of Naples Federico II
