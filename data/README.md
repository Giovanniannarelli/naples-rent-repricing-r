# Data inputs

The raw datasets used in the thesis are **not redistributed** in this repository. The code was written against public statistical and administrative sources and expects the user to obtain the source files independently.

## Main sources

- **Agenzia delle Entrate – OMI Real Estate Market Observatory**
  - semiannual quotation archives for Naples, 2016H1–2024H1
  - residential property type 20 (*Abitazioni civili*), normal condition
  - both rental and sale-price quotation ranges
- **ISTAT 2011 Population and Housing Census**
  - census-section indicators and geometries for Naples
- **ISTAT 8milaCensus 2011**
  - ACE-level indicators used for the economic-distress component of the RdC exposure index
- **ISTAT 2021 Census**
  - resident-family counts and Municipality identifiers for external validation
- **Comune di Napoli – Piano di Attuazione Locale (PAL)**
  - Municipality-level administrative RdC caseloads used only for external validation

## Expected local structure

The original scripts use a local `input/` directory and write processed objects to `data_processed_v10/` and `output_v10/`.

Typical inputs include:

```text
input/
├── omi_raw.zip
├── omi_ntn.zip
├── dati-cpa_2011.zip
├── R15_11_WGS84.zip
├── subcomunali_063_063049.xlsx
├── istat_recent/
│   ├── Comuni_2021.zip
│   └── R15_21.zip
└── validation/
    └── PAL_2020.pdf
```

Exact archive names can vary depending on how the source websites package downloads.

## Why the raw data are excluded

The purpose of this repository is to demonstrate the R workflow: ingestion, validation, spatial aggregation, panel construction, econometric analysis and visualisation. Raw source archives are excluded to avoid redistributing third-party data and to keep the repository lightweight.

## Important measurement note

OMI observations are semiannual **quotation ranges / appraisal-based valuations**, not individual rental contracts or transaction-level observations. The code therefore refers to *OMI rental valuations* or *rent quotations*, rather than observed contract rents.
