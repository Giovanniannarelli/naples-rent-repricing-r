# ==============================================================================
# 06_final_tables_figures.R
# FIX v2: freeze audit uses 6-decimal anchors / 1e-6 tolerance for model statistics.
# V10 - FINAL THESIS PACKAGE / RESULTS FREEZE
# ==============================================================================
#
# SCOPO
# -----
# Questo script NON stima nuovi modelli.
# Legge esclusivamente gli output congelati dei blocchi 01-05 e costruisce:
#
#   - 4 tabelle main-text thesis-ready;
#   - 3 tabelle di appendice;
#   - cartelle con copie delle figure main / appendix;
#   - manifest delle figure con caption suggerite;
#   - checklist delle claim empiriche e dei caveat;
#   - audit finale che verifica i coefficienti frozen.
#
# Nessuna specificazione viene scelta in base al p-value.
#
# OUTPUT:
# output_v10/final_thesis/
#   tables_main/
#   tables_appendix/
#   figures_main/
#   figures_appendix/
#   00_FINAL_FREEZE_AUDIT.csv
#   00_CLAIMS_AND_CAVEATS.csv
#   00_FIGURE_MANIFEST.csv
#   00_TABLE_MANIFEST.csv
#   README_06_FINAL_PACKAGE.txt
#
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Packages / paths
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "stringr")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Pacchetti mancanti: ",
    paste(missing_packages, collapse = ", "),
    "\nInstallare con install.packages().",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

get_script_file <- function() {
  frames <- sys.frames()
  if (length(frames)) {
    for (i in rev(seq_along(frames))) {
      candidate <- frames[[i]]$ofile
      if (!is.null(candidate) &&
          length(candidate) == 1L &&
          nzchar(candidate)) {
        return(candidate)
      }
    }
  }
  NA_character_
}

script_file <- get_script_file()

PROJECT_DIR <- if (!is.na(script_file)) {
  dirname(
    normalizePath(
      script_file,
      winslash = "/",
      mustWork = TRUE
    )
  )
} else {
  normalizePath(
    getwd(),
    winslash = "/",
    mustWork = TRUE
  )
}

OUTPUT_DIR <- file.path(PROJECT_DIR, "output_v10")
FINAL_DIR <- file.path(OUTPUT_DIR, "final_thesis")
TABLE_MAIN_DIR <- file.path(FINAL_DIR, "tables_main")
TABLE_APP_DIR <- file.path(FINAL_DIR, "tables_appendix")
FIG_MAIN_DIR <- file.path(FINAL_DIR, "figures_main")
FIG_APP_DIR <- file.path(FINAL_DIR, "figures_appendix")

for (d in c(
  FINAL_DIR,
  TABLE_MAIN_DIR,
  TABLE_APP_DIR,
  FIG_MAIN_DIR,
  FIG_APP_DIR
)) {
  dir.create(
    d,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

assert_06 <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

read_csv_06 <- function(filename) {
  p <- file.path(OUTPUT_DIR, filename)

  assert_06(
    file.exists(p),
    paste0("File richiesto mancante: ", p)
  )

  utils::read.csv(
    p,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

write_csv_06 <- function(x, path) {
  utils::write.csv(
    x,
    path,
    row.names = FALSE,
    na = ""
  )
}

approx_equal_06 <- function(
  observed,
  expected,
  tol = 1e-10
) {
  length(observed) == 1L &&
    is.finite(observed) &&
    abs(observed - expected) <= tol
}

sig_stars_06 <- function(p) {
  ifelse(
    is.na(p), "",
    ifelse(
      p < 0.01, "***",
      ifelse(
        p < 0.05, "**",
        ifelse(p < 0.10, "*", "")
      )
    )
  )
}

fmt_num_06 <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    "",
    formatC(
      x,
      format = "f",
      digits = digits
    )
  )
}

fmt_p_06 <- function(p) {
  ifelse(
    is.na(p),
    "",
    ifelse(
      p < 0.001,
      "<0.001",
      formatC(
        p,
        format = "f",
        digits = 3
      )
    )
  )
}

# ------------------------------------------------------------------------------
# 1. Canonical inputs
# ------------------------------------------------------------------------------
baseline <- read_csv_06(
  "baseline_profile_by_band.csv"
)
band_trends <- read_csv_06(
  "01_band_trends.csv"
)
break2019 <- read_csv_06(
  "02b_break_2019_2_by_band.csv"
)
catchup2023 <- read_csv_06(
  "03b_catchup_2023_2_by_band.csv"
)
exposure_band <- read_csv_06(
  "12f_rdc_exposure_by_band.csv"
)

gradient_changes <- read_csv_06(
  "22_spatial_gradient_changes.csv"
)
rdc_dynamic <- read_csv_06(
  "23_rdc_dynamic_main.csv"
)
rdc_prebreak <- read_csv_06(
  "23b_rdc_dynamic_pretrend_wald.csv"
)
rdc_initial <- read_csv_06(
  "24_rdc_2019_initial_rent_models.csv"
)
rdc_placebo <- read_csv_06(
  "25_rdc_2019_placebo_models.csv"
)
rdc_absolute <- read_csv_06(
  "26_rdc_2019_absolute_change_models.csv"
)
rdc_2023 <- read_csv_06(
  "27_rdc_2023_reversal_models.csv"
)
rdc_alt <- read_csv_06(
  "28_rdc_alternative_indices_2019.csv"
)
rdc_corr <- read_csv_06(
  "29_rdc_initial_rent_correlations.csv"
)
main_summary <- read_csv_06(
  "30_main_model_summary.csv"
)

omi_key <- read_csv_06(
  "50_omi_measurement_key_findings.csv"
)

endpoint <- read_csv_06(
  "61_endpoint_robustness_2019_2023.csv"
)
distance <- read_csv_06(
  "62_distance_gradient_robustness.csv"
)
sample5960 <- read_csv_06(
  "63_sample_59_vs_60_2019.csv"
)
loo2019 <- read_csv_06(
  "64_leave_one_out_2019.csv"
)
loo2023 <- read_csv_06(
  "64b_leave_one_out_2023_gradient.csv"
)
boundary <- read_csv_06(
  "65_boundary_sensitivity.csv"
)
moran <- read_csv_06(
  "66_moran_residual_spatial_tests.csv"
)
perm <- read_csv_06(
  "67_within_band_permutation_rdc.csv"
)

# ------------------------------------------------------------------------------
# 2. FINAL FREEZE AUDIT
#    Expected values come from the frozen 03/05 results.
# ------------------------------------------------------------------------------
extract_one <- function(
  data,
  condition,
  column,
  label
) {
  hit <- data[condition, , drop = FALSE]

  assert_06(
    nrow(hit) == 1L,
    paste0(
      "Riga non univoca nel freeze audit: ",
      label,
      " | n = ", nrow(hit)
    )
  )

  hit[[column]][1]
}

v_2019_D <- extract_one(
  gradient_changes,
  gradient_changes$semestre == "2019-2" &
    gradient_changes$outcome == "d_log_rent" &
    gradient_changes$fascia == "D",
  "differenziale_vs_B",
  "2019 D-B"
)

v_2019_E <- extract_one(
  gradient_changes,
  gradient_changes$semestre == "2019-2" &
    gradient_changes$outcome == "d_log_rent" &
    gradient_changes$fascia == "E",
  "differenziale_vs_B",
  "2019 E-B"
)

v_2023_D <- extract_one(
  gradient_changes,
  gradient_changes$semestre == "2023-2" &
    gradient_changes$outcome == "d_log_rent" &
    gradient_changes$fascia == "D",
  "differenziale_vs_B",
  "2023 D-B"
)

v_2023_E <- extract_one(
  gradient_changes,
  gradient_changes$semestre == "2023-2" &
    gradient_changes$outcome == "d_log_rent" &
    gradient_changes$fascia == "E",
  "differenziale_vs_B",
  "2023 E-B"
)

v_rdc_m1 <- extract_one(
  rdc_initial,
  rdc_initial$modello == "M1" &
    rdc_initial$termine ==
      "rdc_exposure_main",
  "stima",
  "RdC M1"
)

v_rdc_m2 <- extract_one(
  rdc_initial,
  rdc_initial$modello == "M2" &
    rdc_initial$termine ==
      "rdc_exposure_main",
  "stima",
  "RdC M2"
)

v_dist19 <- extract_one(
  distance,
  distance$evento == "2019 flattening" &
    distance$outcome == "log_rent" &
    distance$result_type == "Slope change",
  "stima",
  "2019 distance change"
)

v_dist23 <- extract_one(
  distance,
  distance$evento == "2023 re-steepening" &
    distance$outcome == "log_rent" &
    distance$result_type == "Slope change",
  "stima",
  "2023 distance change"
)

v_moran_base <- extract_one(
  moran,
  moran$diagnostic ==
    paste0(
      "2019H2 rent residual - fascia + ",
      "average 2018 rent"
    ),
  "moran_I",
  "Moran baseline-adjusted"
)

v_perm_m1 <- extract_one(
  perm,
  perm$adjusted_initial_rent == FALSE,
  "permutation_p_two_sided",
  "Permutation M1"
)

v_perm_m2 <- extract_one(
  perm,
  perm$adjusted_initial_rent == TRUE,
  "permutation_p_two_sided",
  "Permutation M2"
)

freeze_audit <- data.frame(
  controllo = c(
    "2019H2 rent D-B",
    "2019H2 rent E-B",
    "2023H2 rent D-B",
    "2023H2 rent E-B",
    "2019H2 RdC M1",
    "2019H2 RdC M2 initial-rent adjusted",
    "2019 distance-gradient change rent",
    "2023 distance-gradient change rent",
    "2019 Moran after fascia + initial rent",
    "2019 within-band permutation M1 p",
    "2019 within-band permutation M2 p"
  ),
  observed = c(
    v_2019_D,
    v_2019_E,
    v_2023_D,
    v_2023_E,
    v_rdc_m1,
    v_rdc_m2,
    v_dist19,
    v_dist23,
    v_moran_base,
    v_perm_m1,
    v_perm_m2
  ),
  # Freeze anchors are intentionally stored at 6-decimal precision.
  # The audit is meant to detect material changes in frozen results, not
  # irrelevant CSV / floating-point differences at 1e-7 or smaller.
  expected = c(
    0.065127,
    0.064992,
    -0.029267,
    -0.028617,
    0.022972,
    0.006644,
    0.008138,
    -0.003337,
    -0.016016,
    0.0003,
    0.1846
  ),
  tolerance = c(
    rep(1e-6, 9),
    1e-12,
    1e-12
  ),
  stringsAsFactors = FALSE
) |>
  mutate(
    absolute_difference =
      abs(observed - expected),
    esito = ifelse(
      absolute_difference <= tolerance,
      "PASS",
      "FAIL"
    )
  )

write_csv_06(
  freeze_audit,
  file.path(
    FINAL_DIR,
    "00_FINAL_FREEZE_AUDIT.csv"
  )
)

if (any(freeze_audit$esito != "PASS")) {
  bad <- freeze_audit |>
    filter(esito != "PASS")

  stop(
    paste0(
      "FINAL FREEZE AUDIT FALLITO: ",
      paste(
        bad$controllo,
        collapse = "; "
      ),
      ". Nessuna tabella finale e' stata congelata."
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 3. MAIN TABLE 1
#    Spatial / socioeconomic profile before the 2019 episode
# ------------------------------------------------------------------------------
rent2019h1 <- band_trends |>
  filter(
    semestre == "2019-1"
  ) |>
  select(
    fascia,
    mean_rent_2019H1 = mean_rent
  )

table1 <- baseline |>
  select(
    fascia,
    n_zone,
    quota_affitto_2011,
    spazio_mq_procapite_2011,
    quota_terziaria_2011,
    quota_occupati_15plus_2011
  ) |>
  left_join(
    exposure_band |>
      select(
        fascia,
        economic_distress_mean,
        rdc_exposure_mean,
        rdc_exposure_sd
      ),
    by = "fascia"
  ) |>
  left_join(
    rent2019h1,
    by = "fascia"
  ) |>
  transmute(
    Band = fascia,
    Zones = n_zone,
    `Mean rent 2019H1 (EUR/m2/month)` =
      round(mean_rent_2019H1, 2),
    `Renter share 2011 (%)` =
      round(100 * quota_affitto_2011, 1),
    `Employment rate 15+ 2011 (%)` =
      round(
        100 * quota_occupati_15plus_2011,
        1
      ),
    `Economic distress 2011 (%)` =
      round(
        100 * economic_distress_mean,
        1
      ),
    `Space per capita 2011 (m2)` =
      round(
        spazio_mq_procapite_2011,
        1
      ),
    `Mean RdC exposure (SD units)` =
      round(rdc_exposure_mean, 2),
    `Within-band SD RdC exposure` =
      round(rdc_exposure_sd, 2)
  )

write_csv_06(
  table1,
  file.path(
    TABLE_MAIN_DIR,
    "TABLE_1_PRE_POLICY_SPATIAL_PROFILE.csv"
  )
)

# ------------------------------------------------------------------------------
# 4. MAIN TABLE 2
#    Main urban result: 2019 flattening and 2023 re-steepening
# ------------------------------------------------------------------------------
desc_events <- bind_rows(
  break2019 |>
    mutate(
      event = "2019H2 flattening"
    ),
  catchup2023 |>
    mutate(
      event = "2023H2 re-steepening"
    )
) |>
  select(
    event,
    fascia,
    n_zone,
    mean_pct_rent,
    mean_pct_sale
  )

formal_events <- gradient_changes |>
  filter(
    semestre %in% c(
      "2019-2",
      "2023-2"
    ),
    outcome %in% c(
      "d_log_rent",
      "d_log_sale"
    )
  ) |>
  mutate(
    event = ifelse(
      semestre == "2019-2",
      "2019H2 flattening",
      "2023H2 re-steepening"
    ),
    market = ifelse(
      outcome == "d_log_rent",
      "rent",
      "sale"
    )
  ) |>
  select(
    event,
    fascia,
    market,
    differenziale_vs_B,
    errore_standard,
    p_value
  ) |>
  pivot_wider(
    names_from = market,
    values_from = c(
      differenziale_vs_B,
      errore_standard,
      p_value
    )
  )

table2 <- desc_events |>
  left_join(
    formal_events,
    by = c(
      "event",
      "fascia"
    )
  ) |>
  mutate(
    rent_diff_logpp =
      100 * differenziale_vs_B_rent,
    rent_se_logpp =
      100 * errore_standard_rent,
    sale_diff_logpp =
      100 * differenziale_vs_B_sale,
    sale_se_logpp =
      100 * errore_standard_sale,

    rent_diff_formatted = ifelse(
      fascia == "B",
      "Reference",
      paste0(
        fmt_num_06(
          rent_diff_logpp,
          2
        ),
        sig_stars_06(p_value_rent),
        " (",
        fmt_num_06(
          rent_se_logpp,
          2
        ),
        ")"
      )
    ),

    sale_diff_formatted = ifelse(
      fascia == "B",
      "Reference",
      paste0(
        fmt_num_06(
          sale_diff_logpp,
          2
        ),
        sig_stars_06(p_value_sale),
        " (",
        fmt_num_06(
          sale_se_logpp,
          2
        ),
        ")"
      )
    )
  ) |>
  transmute(
    Event = event,
    Band = fascia,
    Zones = n_zone,
    `Mean rent change (%)` =
      round(100 * mean_pct_rent, 2),
    `Rent differential vs B (log pp; HC3 SE)` =
      rent_diff_formatted,
    `Rent p-value` =
      ifelse(
        fascia == "B",
        "",
        fmt_p_06(p_value_rent)
      ),
    `Mean sale change (%)` =
      round(100 * mean_pct_sale, 2),
    `Sale differential vs B (log pp; HC3 SE)` =
      sale_diff_formatted,
    `Sale p-value` =
      ifelse(
        fascia == "B",
        "",
        fmt_p_06(p_value_sale)
      )
  )

write_csv_06(
  table2,
  file.path(
    TABLE_MAIN_DIR,
    "TABLE_2_MAIN_SPATIAL_REPRICING.csv"
  )
)

# ------------------------------------------------------------------------------
# 5. MAIN TABLE 3
#    RdC mechanism / initial-rent confounding / placebo
# ------------------------------------------------------------------------------
get_summary_row <- function(label) {
  hit <- main_summary |>
    filter(risultato == label)

  assert_06(
    nrow(hit) == 1L,
    paste0(
      "Riga mancante in 30_main_model_summary: ",
      label
    )
  )

  hit
}

s_dyn_rent <- get_summary_row(
  "RdC exposure dynamic slope - rent 2019H2"
)
s_pre <- get_summary_row(
  "Joint pre-2019H2 RdC exposure slopes - rent"
)
s_m1 <- get_summary_row(
  "2019H2 rent exposure - M1"
)
s_m2 <- get_summary_row(
  "2019H2 rent exposure - M2 initial-rent adjusted"
)
s_m3 <- get_summary_row(
  paste0(
    "2019H2 rent exposure - ",
    "M3 nonlinear initial-rent adjusted"
  )
)
s_m4 <- get_summary_row(
  "2019H2 rent exposure - M4 initial-rent quintiles"
)
s_m5 <- get_summary_row(
  "2019H2 rent exposure - M5 2019H1 rent adjusted"
)
s_sale <- get_summary_row(
  "RdC exposure dynamic slope - sale 2019H2"
)
s_gap <- get_summary_row(
  "RdC exposure dynamic slope - rent-sale gap 2019H2"
)
s_2023 <- get_summary_row(
  "RdC exposure dynamic slope - rent 2023H2"
)

table3 <- data.frame(
  Result = c(
    "Dynamic within-band rent association, 2019H2",
    "Joint pre-2019H2 / pre-break slopes",
    "M1: band + RdC exposure",
    "M2: M1 + average log rent 2018",
    "M3: M2 + nonlinear initial-rent term",
    "M4: M1 + 2018 initial-rent quintiles",
    "M5: M1 + log rent 2019H1",
    "Sale-price placebo, 2019H2",
    "Rent-sale valuation gap, 2019H2",
    "Contextual RdC-exposure slope, 2023H2"
  ),
  Estimate = c(
    100 * s_dyn_rent$stima,
    s_pre$stima,
    100 * s_m1$stima,
    100 * s_m2$stima,
    100 * s_m3$stima,
    100 * s_m4$stima,
    100 * s_m5$stima,
    100 * s_sale$stima,
    100 * s_gap$stima,
    100 * s_2023$stima
  ),
  SE = c(
    100 * s_dyn_rent$errore_standard,
    NA_real_,
    100 * s_m1$errore_standard,
    100 * s_m2$errore_standard,
    100 * s_m3$errore_standard,
    100 * s_m4$errore_standard,
    100 * s_m5$errore_standard,
    100 * s_sale$errore_standard,
    100 * s_gap$errore_standard,
    100 * s_2023$errore_standard
  ),
  p_value = c(
    s_dyn_rent$p_value,
    s_pre$p_value,
    s_m1$p_value,
    s_m2$p_value,
    s_m3$p_value,
    s_m4$p_value,
    s_m5$p_value,
    s_sale$p_value,
    s_gap$p_value,
    s_2023$p_value
  ),
  Inference = c(
    "Zone-clustered SE; 59 clusters",
    "F-test; 5 pre-2019H2 slopes; cluster df=58",
    "HC3",
    "HC3",
    "HC3",
    "HC3",
    "HC3",
    "Zone-clustered SE; 59 clusters",
    "Zone-clustered SE; 59 clusters",
    "Zone-clustered SE; 59 clusters"
  ),
  Interpretation = c(
    "Log-rent growth per +1 SD RdC exposure",
    "F statistic, not a coefficient",
    "Unadjusted within-band association",
    "Preferred initial-rent confound adjustment",
    "Nonlinear initial-rent robustness",
    "Flexible initial-rent robustness",
    "Alternative initial-level robustness",
    "No analogous dynamic sale-price effect",
    "Rental-specific relative revaluation",
    "Not interpreted as a causal phase-out effect"
  ),
  stringsAsFactors = FALSE
) |>
  mutate(
    `Estimate / F statistic` =
      round(Estimate, 3),
    `SE` =
      round(SE, 3),
    `p-value` =
      fmt_p_06(p_value),
    Significance =
      sig_stars_06(p_value)
  ) |>
  select(
    Result,
    `Estimate / F statistic`,
    SE,
    `p-value`,
    Significance,
    Inference,
    Interpretation
  )

write_csv_06(
  table3,
  file.path(
    TABLE_MAIN_DIR,
    "TABLE_3_RDC_MECHANISM_AND_CONFOUNDING.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. MAIN TABLE 4
#    Compact robustness summary
# ------------------------------------------------------------------------------
ep19 <- endpoint |>
  filter(
    evento == "2019-2",
    market == "Rent",
    termine == "rdc_exposure_main",
    modello_tipo %in% c(
      "RdC M1",
      "RdC M2 initial-level adjusted"
    )
  )

get_ep_value <- function(endpoint_name, model_name, col) {
  hit <- ep19 |>
    filter(
      endpoint == endpoint_name,
      modello_tipo == model_name
    )
  assert_06(
    nrow(hit) == 1L,
    paste0(
      "Endpoint robustness mancante: ",
      endpoint_name, " / ", model_name
    )
  )
  hit[[col]][1]
}

get_distance_change <- function(
  event_name,
  outcome_name,
  col
) {
  hit <- distance |>
    filter(
      evento == event_name,
      outcome == outcome_name,
      result_type == "Slope change"
    )
  assert_06(
    nrow(hit) == 1L,
    paste0(
      "Distance result mancante: ",
      event_name, " / ", outcome_name
    )
  )
  hit[[col]][1]
}

get_5960 <- function(sample_name, col) {
  hit <- sample5960 |>
    filter(
      sample == sample_name,
      str_detect(modello, "RDC_M1$"),
      termine == "rdc_exposure_main"
    )
  assert_06(
    nrow(hit) == 1L,
    paste0("59/60 result mancante: ", sample_name)
  )
  hit[[col]][1]
}

boundary_m1_drop <- boundary |>
  filter(
    evento == "2019-2",
    scenario ==
      "Drop >10% boundary-shift zones",
    str_detect(modello, "RDC_M1$"),
    termine == "rdc_exposure_main"
  )

boundary_m2_drop <- boundary |>
  filter(
    evento == "2019-2",
    scenario ==
      "Drop >10% boundary-shift zones",
    str_detect(modello, "RDC_M2$"),
    termine == "rdc_exposure_main"
  )

assert_06(
  nrow(boundary_m1_drop) == 1L &&
    nrow(boundary_m2_drop) == 1L,
  "Boundary robustness rows non univoche."
)

moran_fascia <- moran |>
  filter(
    diagnostic ==
      "2019H2 rent residual - fascia"
  )

moran_baseline <- moran |>
  filter(
    diagnostic ==
      paste0(
        "2019H2 rent residual - fascia + ",
        "average 2018 rent"
      )
  )

assert_06(
  nrow(moran_fascia) == 1L &&
    nrow(moran_baseline) == 1L,
  "Moran rows non univoche."
)

table4 <- data.frame(
  Robustness = c(
    "RdC M1 - OMI lower bound",
    "RdC M1 - OMI midpoint",
    "RdC M1 - OMI upper bound",
    "RdC M2 adjusted - OMI lower bound",
    "RdC M2 adjusted - OMI midpoint",
    "RdC M2 adjusted - OMI upper bound",
    "2019 rent distance-gradient change",
    "2019 sale distance-gradient change",
    "2023 rent distance-gradient change",
    "2023 sale distance-gradient change",
    "RdC M1 - 59 balanced zones",
    "RdC M1 - 60 zones incl. D32",
    "RdC M1 - drop boundary-sensitive zones",
    "RdC M2 - drop boundary-sensitive zones",
    "RdC M1 leave-one-out range: minimum",
    "RdC M1 leave-one-out range: maximum",
    "RdC M1 leave-one-out worst p-value",
    "RdC M2 leave-one-out range: minimum",
    "RdC M2 leave-one-out range: maximum",
    "Moran I after fascia",
    "Moran I after fascia + initial rent",
    "Within-band permutation M1",
    "Within-band permutation M2"
  ),
  Estimate = c(
    get_ep_value(
      "Rent lower bound",
      "RdC M1",
      "stima"
    ),
    get_ep_value(
      "Rent midpoint",
      "RdC M1",
      "stima"
    ),
    get_ep_value(
      "Rent upper bound",
      "RdC M1",
      "stima"
    ),
    get_ep_value(
      "Rent lower bound",
      "RdC M2 initial-level adjusted",
      "stima"
    ),
    get_ep_value(
      "Rent midpoint",
      "RdC M2 initial-level adjusted",
      "stima"
    ),
    get_ep_value(
      "Rent upper bound",
      "RdC M2 initial-level adjusted",
      "stima"
    ),
    get_distance_change(
      "2019 flattening",
      "log_rent",
      "stima"
    ),
    get_distance_change(
      "2019 flattening",
      "log_sale",
      "stima"
    ),
    get_distance_change(
      "2023 re-steepening",
      "log_rent",
      "stima"
    ),
    get_distance_change(
      "2023 re-steepening",
      "log_sale",
      "stima"
    ),
    get_5960(
      "59 balanced zones",
      "stima"
    ),
    get_5960(
      "60 zones incl. D32",
      "stima"
    ),
    boundary_m1_drop$stima,
    boundary_m2_drop$stima,
    min(loo2019$rdc_M1_est),
    max(loo2019$rdc_M1_est),
    max(loo2019$rdc_M1_p),
    min(loo2019$rdc_M2_est),
    max(loo2019$rdc_M2_est),
    moran_fascia$moran_I,
    moran_baseline$moran_I,
    perm$beta_observed[
      perm$adjusted_initial_rent == FALSE
    ],
    perm$beta_observed[
      perm$adjusted_initial_rent == TRUE
    ]
  ),
  p_value = c(
    get_ep_value(
      "Rent lower bound",
      "RdC M1",
      "p_value"
    ),
    get_ep_value(
      "Rent midpoint",
      "RdC M1",
      "p_value"
    ),
    get_ep_value(
      "Rent upper bound",
      "RdC M1",
      "p_value"
    ),
    get_ep_value(
      "Rent lower bound",
      "RdC M2 initial-level adjusted",
      "p_value"
    ),
    get_ep_value(
      "Rent midpoint",
      "RdC M2 initial-level adjusted",
      "p_value"
    ),
    get_ep_value(
      "Rent upper bound",
      "RdC M2 initial-level adjusted",
      "p_value"
    ),
    get_distance_change(
      "2019 flattening",
      "log_rent",
      "p_value"
    ),
    get_distance_change(
      "2019 flattening",
      "log_sale",
      "p_value"
    ),
    get_distance_change(
      "2023 re-steepening",
      "log_rent",
      "p_value"
    ),
    get_distance_change(
      "2023 re-steepening",
      "log_sale",
      "p_value"
    ),
    get_5960(
      "59 balanced zones",
      "p_value"
    ),
    get_5960(
      "60 zones incl. D32",
      "p_value"
    ),
    boundary_m1_drop$p_value,
    boundary_m2_drop$p_value,
    NA_real_,
    NA_real_,
    max(loo2019$rdc_M1_p),
    NA_real_,
    NA_real_,
    moran_fascia$p_two_sided,
    moran_baseline$p_two_sided,
    perm$permutation_p_two_sided[
      perm$adjusted_initial_rent == FALSE
    ],
    perm$permutation_p_two_sided[
      perm$adjusted_initial_rent == TRUE
    ]
  ),
  Note = c(
    rep(
      "Coefficient is log rent change per +1 SD RdC exposure",
      6
    ),
    "Change in log-rent slope per km; positive = flattening",
    "Sale-price benchmark",
    "Change in log-rent slope per km; negative = re-steepening",
    "Sale-price benchmark",
    "Main 59-zone sample",
    "Adds D32",
    "Drops B13, B16, C31, E45",
    "Drops B13, B16, C31, E45",
    "Coefficient range only",
    "Coefficient range only",
    "Maximum p across 59 exclusions; value shown under Estimate",
    "Coefficient range only",
    "Coefficient range only",
    "Residual spatial diagnostic after broad OMI geography",
    "Residual spatial diagnostic after initial-rent adjustment",
    "Permutation p-value within OMI bands",
    "Permutation p-value within OMI bands"
  ),
  stringsAsFactors = FALSE
) |>
  mutate(
    Estimate = round(Estimate, 5),
    `p-value` = fmt_p_06(p_value),
    Significance = sig_stars_06(p_value)
  ) |>
  select(
    Robustness,
    Estimate,
    `p-value`,
    Significance,
    Note
  )

write_csv_06(
  table4,
  file.path(
    TABLE_MAIN_DIR,
    "TABLE_4_FINAL_ROBUSTNESS_SUMMARY.csv"
  )
)

# ------------------------------------------------------------------------------
# 7. APPENDIX TABLE A1 - measurement validation
# ------------------------------------------------------------------------------
table_a1 <- omi_key |>
  mutate(
    value = round(value, 4)
  ) |>
  rename(
    Diagnostic = finding,
    Value = value
  )

write_csv_06(
  table_a1,
  file.path(
    TABLE_APP_DIR,
    "TABLE_A1_OMI_MEASUREMENT_VALIDATION.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. APPENDIX TABLE A2 - alternative exposure indices
# ------------------------------------------------------------------------------
table_a2 <- rdc_alt |>
  mutate(
    stima = round(stima, 6),
    errore_standard = round(
      errore_standard,
      6
    ),
    p_value = round(p_value, 6)
  )

write_csv_06(
  table_a2,
  file.path(
    TABLE_APP_DIR,
    "TABLE_A2_RDC_ALTERNATIVE_INDICES.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. APPENDIX TABLE A3 - spatial diagnostics
# ------------------------------------------------------------------------------
table_a3 <- bind_rows(
  moran |>
    transmute(
      Test = diagnostic,
      Statistic = round(moran_I, 5),
      `p-value` = round(
        p_two_sided,
        5
      ),
      Detail = paste0(
        "Moran I; ",
        permutations,
        " permutations"
      )
    ),
  perm |>
    transmute(
      Test = modello,
      Statistic = round(
        beta_observed,
        5
      ),
      `p-value` = round(
        permutation_p_two_sided,
        5
      ),
      Detail = paste0(
        "Within-band exposure permutation; ",
        permutations,
        " permutations"
      )
    )
)

write_csv_06(
  table_a3,
  file.path(
    TABLE_APP_DIR,
    "TABLE_A3_SPATIAL_AND_PERMUTATION_DIAGNOSTICS.csv"
  )
)

# ------------------------------------------------------------------------------
# 10. Claim / caveat freeze
# ------------------------------------------------------------------------------
claims <- data.frame(
  id = c(
    "C1", "C2", "C3", "C4",
    "C5", "C6", "C7", "C8"
  ),
  status = c(
    "MAIN CLAIM",
    "MAIN CLAIM",
    "MECHANISM EVIDENCE",
    "IDENTIFICATION LIMIT",
    "MAIN CLAIM",
    "NEGATIVE RESULT",
    "MEASUREMENT CAVEAT",
    "SPATIAL DIAGNOSTIC"
  ),
  thesis_wording = c(
    paste0(
      "Naples experienced a marked flattening of the ",
      "OMI residential rent gradient in 2019H2, driven ",
      "by stronger repricing in initially cheaper outer bands."
    ),
    paste0(
      "The 2019 spatial restructuring is rental-specific: ",
      "sale-price valuations do not exhibit an analogous ",
      "change in the spatial gradient."
    ),
    paste0(
      "Pre-policy RdC exposure strongly predicts the spatial ",
      "incidence of the 2019H2 rental repricing within OMI bands."
    ),
    paste0(
      "The independent RdC contribution is not cleanly ",
      "identified because exposure is strongly correlated ",
      "with initially low rents and the coefficient attenuates ",
      "substantially after initial-rent adjustment."
    ),
    paste0(
      "In 2023H2 the residential rent gradient re-steepens, ",
      "especially for the outer D/E bands, while sale valuations ",
      "again do not show the same spatial restructuring."
    ),
    paste0(
      "The data do not support interpreting the 2023 ",
      "re-steepening as a causal reverse experiment of ",
      "the RdC phase-out."
    ),
    paste0(
      "OMI observations are semiannual appraisal-based ",
      "quotation intervals rather than observed contract rents; ",
      "external market sources are therefore used only as ",
      "city-level validation of upward rental pressure."
    ),
    paste0(
      "Raw changes are spatially autocorrelated, but residual ",
      "Moran diagnostics show no detectable remaining spatial ",
      "dependence after accounting for OMI geography and initial rent."
    )
  ),
  evidence = c(
    "Table 2; Figures 1 and 5; endpoint/LOO/boundary robustness",
    "Table 2; Figure 5; sale endpoint robustness",
    "Table 3; Figure 3; permutation and leave-one-out robustness",
    "Table 3; Figure 4; endpoint and permutation robustness",
    "Table 2; Figures 1 and 5; leave-one-out robustness",
    "Table 3; 2023 RdC coefficient not significant",
    "OMI audit 04 + external validation memo",
    "Table 4 / Appendix Table A3"
  ),
  avoid_wording = c(
    "Do not claim observed contract rents rose by exactly the OMI percentage.",
    "Do not claim the whole Naples real-estate market repriced.",
    "Do not write 'RdC caused the flattening'.",
    "Do not hide the initial-rent attenuation.",
    "Do not claim every band exactly returned to its pre-2019 level; D/E do so most clearly.",
    "Do not call 2023 an RdC phase-out treatment effect.",
    "Do not describe OMI quotations as individual leases or transactions.",
    "Do not claim Moran non-significance proves absence of every possible form of spatial dependence."
  ),
  stringsAsFactors = FALSE
)

write_csv_06(
  claims,
  file.path(
    FINAL_DIR,
    "00_CLAIMS_AND_CAVEATS.csv"
  )
)

# ------------------------------------------------------------------------------
# 11. Figure selection / copying
# ------------------------------------------------------------------------------
main_figures <- data.frame(
  final_number = 1:5,
  source_file = c(
    "fig14_rent_gradient_levels_main.png",
    "fig09_rdc_exposure_map.png",
    "fig12_rdc_dynamic_rent_sale.png",
    "fig13_rdc_2019_initial_rent_sensitivity.png",
    "fig21_distance_gradient_event_changes.png"
  ),
  destination_file = c(
    "FIGURE_1_RENT_GRADIENT_LEVELS.png",
    "FIGURE_2_RDC_EXPOSURE_MAP.png",
    "FIGURE_3_DYNAMIC_RDC_RENT_SALE.png",
    "FIGURE_4_INITIAL_RENT_SENSITIVITY.png",
    "FIGURE_5_DISTANCE_GRADIENT_ROBUSTNESS.png"
  ),
  suggested_caption = c(
    paste0(
      "Residential rent gradient across OMI bands, ",
      "showing the 2019 compression and subsequent ",
      "re-steepening."
    ),
    paste0(
      "Pre-policy spatial exposure to the Citizenship ",
      "Income across Naples OMI zones."
    ),
    paste0(
      "Semester-specific within-band association between ",
      "pre-policy RdC exposure and changes in residential ",
      "rent and sale valuations."
    ),
    paste0(
      "Sensitivity of the 2019H2 RdC-exposure coefficient ",
      "to alternative controls for initial rent levels."
    ),
    paste0(
      "Formal changes in the continuous distance gradient ",
      "for residential rent and sale valuations in 2019H2 ",
      "and 2023H2."
    )
  ),
  role = c(
    "Main urban result",
    "Mechanism / exposure design",
    "Dynamic mechanism + placebo",
    "Identification limitation",
    "Final spatial robustness"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(main_figures))) {
  src <- file.path(
    OUTPUT_DIR,
    main_figures$source_file[i]
  )
  dst <- file.path(
    FIG_MAIN_DIR,
    main_figures$destination_file[i]
  )

  assert_06(
    file.exists(src),
    paste0("Figura main mancante: ", src)
  )

  ok <- file.copy(
    src,
    dst,
    overwrite = TRUE
  )

  assert_06(
    isTRUE(ok),
    paste0(
      "Impossibile copiare figura: ",
      main_figures$source_file[i]
    )
  )
}

appendix_sources <- c(
  "fig01_rent_index_by_band.png",
  "fig02_sale_index_by_band.png",
  "fig03_rent_dispersion.png",
  "fig04_sale_dispersion.png",
  "fig05_initial_rent_vs_2019_growth.png",
  "fig06_2019_absolute_rent_change_by_band.png",
  "fig07_rent_distance_gradient.png",
  "fig08_2023_reversal.png",
  "fig10_rdc_components_by_band.png",
  "fig11_rdc_dynamic_rent.png",
  "fig15_omi_type20_rent_sale_changes.png",
  "fig16_omi_type20_share_changed.png",
  "fig17_omi_residential_nonresidential.png",
  "fig18_omi_2019_2023_property_types.png",
  "fig19_endpoint_rdc_2019.png",
  "fig20_leave_one_out_rdc_2019.png"
)

appendix_manifest <- data.frame(
  appendix_number = seq_along(
    appendix_sources
  ),
  source_file = appendix_sources,
  destination_file = paste0(
    "FIGURE_A",
    seq_along(appendix_sources),
    "_",
    toupper(
      str_replace(
        str_replace(
          appendix_sources,
          "^fig[0-9]+_",
          ""
        ),
        "\\.png$",
        ""
      )
    ),
    ".png"
  ),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(appendix_manifest))) {
  src <- file.path(
    OUTPUT_DIR,
    appendix_manifest$source_file[i]
  )
  dst <- file.path(
    FIG_APP_DIR,
    appendix_manifest$destination_file[i]
  )

  assert_06(
    file.exists(src),
    paste0("Figura appendix mancante: ", src)
  )

  ok <- file.copy(
    src,
    dst,
    overwrite = TRUE
  )

  assert_06(
    isTRUE(ok),
    paste0(
      "Impossibile copiare figura appendix: ",
      appendix_manifest$source_file[i]
    )
  )
}

figure_manifest <- bind_rows(
  main_figures |>
    transmute(
      placement = "MAIN TEXT",
      number = paste0(
        "Figure ",
        final_number
      ),
      source_file,
      destination_file,
      caption = suggested_caption,
      role
    ),
  appendix_manifest |>
    transmute(
      placement = "APPENDIX",
      number = paste0(
        "Figure A",
        appendix_number
      ),
      source_file,
      destination_file,
      caption = "",
      role = "Supplementary diagnostic / robustness"
    )
)

write_csv_06(
  figure_manifest,
  file.path(
    FINAL_DIR,
    "00_FIGURE_MANIFEST.csv"
  )
)

# ------------------------------------------------------------------------------
# 12. Table manifest
# ------------------------------------------------------------------------------
table_manifest <- data.frame(
  placement = c(
    rep("MAIN TEXT", 4),
    rep("APPENDIX", 3)
  ),
  number = c(
    "Table 1",
    "Table 2",
    "Table 3",
    "Table 4",
    "Table A1",
    "Table A2",
    "Table A3"
  ),
  file = c(
    "tables_main/TABLE_1_PRE_POLICY_SPATIAL_PROFILE.csv",
    "tables_main/TABLE_2_MAIN_SPATIAL_REPRICING.csv",
    "tables_main/TABLE_3_RDC_MECHANISM_AND_CONFOUNDING.csv",
    "tables_main/TABLE_4_FINAL_ROBUSTNESS_SUMMARY.csv",
    "tables_appendix/TABLE_A1_OMI_MEASUREMENT_VALIDATION.csv",
    "tables_appendix/TABLE_A2_RDC_ALTERNATIVE_INDICES.csv",
    "tables_appendix/TABLE_A3_SPATIAL_AND_PERMUTATION_DIAGNOSTICS.csv"
  ),
  purpose = c(
    "Describe baseline spatial sorting and pre-policy RdC exposure.",
    "Establish 2019 flattening, rent specificity, and 2023 re-steepening.",
    "Present RdC association, pre-break dynamics, placebo, and initial-rent confounding.",
    "Summarize endpoint, sample, boundary, leave-one-out, distance, Moran and permutation robustness.",
    "Document OMI measurement diagnostics.",
    "Show that conclusions do not depend on the exposure weighting scheme.",
    "Document spatial residual and within-band permutation diagnostics."
  ),
  stringsAsFactors = FALSE
)

write_csv_06(
  table_manifest,
  file.path(
    FINAL_DIR,
    "00_TABLE_MANIFEST.csv"
  )
)

# ------------------------------------------------------------------------------
# 13. README final package
# ------------------------------------------------------------------------------
readme <- c(
  "V10 - 06 FINAL THESIS PACKAGE",
  "=============================",
  "",
  "STATUS",
  "No new econometric model is estimated in 06.",
  "All tables and figures are assembled from outputs frozen in 01-05.",
  "",
  "MAIN TEXT TABLES",
  "Table 1 - Pre-policy spatial profile.",
  "Table 2 - Main spatial repricing: 2019 flattening and 2023 re-steepening.",
  "Table 3 - RdC mechanism evidence and initial-rent confounding.",
  "Table 4 - Compact final robustness summary.",
  "",
  "MAIN TEXT FIGURES",
  "Figure 1 - Rent gradient in levels.",
  "Figure 2 - RdC exposure map.",
  "Figure 3 - Dynamic RdC rent vs sale.",
  "Figure 4 - Initial-rent sensitivity.",
  "Figure 5 - Distance-gradient robustness.",
  "",
  "CORE EMPIRICAL HIERARCHY",
  "1. Main finding: temporary compression of the Naples OMI residential rent gradient in 2019.",
  "2. Market segmentation: no analogous spatial restructuring in OMI sale valuations.",
  "3. Mechanism evidence: 2019 repricing is strongly associated with pre-policy RdC exposure within OMI bands.",
  "4. Identification limit: RdC exposure and initially low rents are tightly intertwined; the adjusted RdC coefficient is attenuated and not statistically distinguishable from zero.",
  "5. Later episode: a robust residential rent-gradient re-steepening occurs in 2023.",
  "6. Negative result: 2023 is not identified as a causal reverse RdC phase-out experiment.",
  "",
  "IMPORTANT TIMING LANGUAGE",
  "The formal joint test in 23b covers the five semesters before 2019H2 and should be described as a pre-2019H2 / pre-break test.",
  "Do NOT call all five semesters strictly pre-policy because RdC payments began during 2019H1.",
  "2019H2 may be described as the first full OMI semester after the RdC rollout.",
  "",
  "OMI LANGUAGE",
  "OMI data are semiannual appraisal-based quotation intervals.",
  "Prefer 'OMI rental valuations', 'OMI rent quotations', or 'OMI rental repricing'.",
  "Do not equate OMI changes mechanically with observed individual contract-rent changes.",
  "",
  "EXTERNAL VALIDATION",
  "Use external sources only for city-level validation that rental pressure was positive in Naples in the relevant periods.",
  "Do not claim external sources replicate the B/C/D/E spatial flattening unless a directly comparable source is found.",
  "",
  "NEXT STEP",
  "After 06, the empirical analysis is frozen unless the supervisor requests a specific additional test.",
  "The remaining work is synthesis, supervisor discussion, chapter outline, and thesis writing."
)

writeLines(
  readme,
  file.path(
    FINAL_DIR,
    "README_06_FINAL_PACKAGE.txt"
  ),
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 14. Console summary
# ------------------------------------------------------------------------------
message("")
message("==============================================================")
message("06_final_tables_figures.R COMPLETATO")
message("==============================================================")
message(
  "Freeze audit: ",
  sum(
    freeze_audit$esito == "PASS"
  ),
  " / ",
  nrow(freeze_audit),
  " PASS"
)
message(
  "Main tables: 4"
)
message(
  "Appendix tables: 3"
)
message(
  "Main figures: ",
  nrow(main_figures)
)
message(
  "Appendix figures: ",
  nrow(appendix_manifest)
)
message("")
message(
  "Final package: ",
  normalizePath(
    FINAL_DIR,
    winslash = "/",
    mustWork = TRUE
  )
)
message("")
message(
  "Nessun nuovo modello e' stato stimato."
)
message(
  "La parte empirica e' ora congelata, salvo richieste specifiche della relatrice."
)
