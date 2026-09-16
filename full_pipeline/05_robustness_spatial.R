# ==============================================================================
# 05_robustness_spatial.R
# FIX v2: corrected summary-filter name collisions in get_ep/get_dist.
# V10 - FINAL ROBUSTNESS AND SPATIAL DIAGNOSTICS
# Naples OMI rent-gradient / RdC exposure thesis
# ==============================================================================
#
# SCOPO
# -----
# Stress-test dei risultati gia' congelati nei blocchi 01-04.
# Questo script NON ridefinisce:
# - il campione principale;
# - la RdC exposure;
# - il timing 2019H2 / 2023H2;
# - la specificazione principale del 03.
#
# ROBUSTEZZE PRE-SPECIFICATE
# --------------------------
# 1) OMI lower / midpoint / upper bounds:
#    il flattening 2019 e il re-steepening 2023 devono essere visibili anche
#    usando separatamente Loc_min e Loc_max; le vendite sono benchmark/placebo.
#
# 2) Distance gradient:
#    test formale del cambiamento di slope rispetto alla distanza dal B-core:
#    2019H1 -> 2019H2 e 2023H1 -> 2023H2, rent vs sale.
#
# 3) 59 vs 60 zones, 2019H2:
#    D32 e' disponibile nel cross-section 2019 ma non nel lungo balanced panel.
#    Confrontiamo i risultati usando esattamente la stessa specifica su 59 e 60.
#
# 4) Leave-one-out:
#    nessuna singola zona deve guidare il flattening o l'associazione RdC 2019.
#
# 5) Boundary-sensitive zones:
#    B13, B16, C31, E45 erano le zone con revisioni geometriche >10% nel
#    precedente audit di stabilita' geografica. Le escludiamo congiuntamente
#    come robustness pre-specificata, non in base ai risultati del 2019.
#
# 6) Moran's I dei residui:
#    W binaria e simmetrica costruita da zone_neighbors.csv (131 coppie).
#    Permutation inference two-sided e greater-tail, 9,999 permutazioni.
#    Se il clustering grezzo scompare dopo fascia / initial rent, non imponiamo
#    arbitrariamente una spatial-HAC/Conley specification.
#
# 7) Within-band permutation inference:
#    la RdC exposure viene permutata SOLO all'interno della fascia OMI.
#    E' una robustness inferenziale, NON un randomization test causale.
#
# INPUT CANONICI
# ---------------
# output_v10/master_naples_housing_panel_rdc.csv
# output_v10/zone_neighbors.csv
# output_v10/zone_geography.csv
# output_v10/10_all_types_2019_2_detail.csv
# output_v10/12b_rdc_exposure_zone_60.csv
#
# OUTPUT
# ------
# 60_spatial_robustness_audit.csv
# 61_endpoint_robustness_2019_2023.csv
# 62_distance_gradient_robustness.csv
# 63_sample_59_vs_60_2019.csv
# 64_leave_one_out_2019.csv
# 64b_leave_one_out_2023_gradient.csv
# 65_boundary_sensitivity.csv
# 66_moran_residual_spatial_tests.csv
# 67_within_band_permutation_rdc.csv
# 68_robustness_summary.csv
# fig19_endpoint_rdc_2019.png
# fig20_leave_one_out_rdc_2019.png
# fig21_distance_gradient_event_changes.png
# README_05_ROBUSTNESS_SPATIAL.txt
#
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Packages / paths
# ------------------------------------------------------------------------------
required_packages <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "sandwich", "fixest"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Pacchetti R mancanti: ", paste(missing_packages, collapse = ", "),
    "\nInstallare con:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(sandwich)
  library(fixest)
})

get_script_file <- function() {
  frames <- sys.frames()
  if (length(frames)) {
    for (i in rev(seq_along(frames))) {
      candidate <- frames[[i]]$ofile
      if (!is.null(candidate) && length(candidate) == 1L && nzchar(candidate)) {
        return(candidate)
      }
    }
  }
  NA_character_
}

script_file <- get_script_file()

PROJECT_DIR <- if (!is.na(script_file)) {
  dirname(normalizePath(script_file, winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

OUTPUT_DIR <- file.path(PROJECT_DIR, "output_v10")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

MASTER_PATH <- file.path(
  OUTPUT_DIR, "master_naples_housing_panel_rdc.csv"
)
NEIGHBOR_PATH <- file.path(
  OUTPUT_DIR, "zone_neighbors.csv"
)
GEOGRAPHY_PATH <- file.path(
  OUTPUT_DIR, "zone_geography.csv"
)
DETAIL60_PATH <- file.path(
  OUTPUT_DIR, "10_all_types_2019_2_detail.csv"
)
EXPOSURE60_PATH <- file.path(
  OUTPUT_DIR, "12b_rdc_exposure_zone_60.csv"
)

assert_05 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

for (p in c(
  MASTER_PATH,
  NEIGHBOR_PATH,
  GEOGRAPHY_PATH,
  DETAIL60_PATH,
  EXPOSURE60_PATH
)) {
  assert_05(
    file.exists(p),
    paste0("File richiesto non trovato: ", p)
  )
}

write_csv_05 <- function(x, filename) {
  utils::write.csv(
    x,
    file.path(OUTPUT_DIR, filename),
    row.names = FALSE,
    na = ""
  )
}

# ------------------------------------------------------------------------------
# 1. Statistical helpers
# ------------------------------------------------------------------------------
tidy_lm_hc3 <- function(
  fit,
  model_name,
  outcome_name,
  specification
) {
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- stats::coef(fit)
  se <- sqrt(diag(V))

  keep <- is.finite(b) & is.finite(se)
  b <- b[keep]
  se <- se[keep]

  df_r <- stats::df.residual(fit)
  tval <- b / se
  pval <- 2 * stats::pt(
    abs(tval),
    df = df_r,
    lower.tail = FALSE
  )
  crit <- stats::qt(0.975, df = df_r)

  data.frame(
    modello = model_name,
    outcome = outcome_name,
    specifica = specification,
    termine = names(b),
    stima = as.numeric(b),
    errore_standard = as.numeric(se),
    statistica_t = as.numeric(tval),
    p_value = as.numeric(pval),
    ci95_low = as.numeric(b - crit * se),
    ci95_high = as.numeric(b + crit * se),
    n = stats::nobs(fit),
    df_residui = df_r,
    r2 = summary(fit)$r.squared,
    r2_adj = summary(fit)$adj.r.squared,
    stringsAsFactors = FALSE
  )
}

extract_hc3 <- function(
  fit,
  term,
  model_name,
  outcome_name,
  specification
) {
  tab <- tidy_lm_hc3(
    fit,
    model_name,
    outcome_name,
    specification
  )
  hit <- tab |>
    filter(termine == term)

  assert_05(
    nrow(hit) == 1L,
    paste0(
      "Termine non trovato univocamente: ",
      term, " in ", model_name
    )
  )
  hit
}

fit_fixest_cluster_term <- function(
  formula,
  data,
  term_pattern_1,
  term_pattern_2 = NULL,
  model_name,
  outcome_name,
  specification
) {
  fit <- fixest::feols(
    formula,
    data = data,
    cluster = ~zona,
    notes = FALSE,
    warn = TRUE
  )

  b <- stats::coef(fit)
  V <- stats::vcov(fit)
  nm <- names(b)

  hit <- grepl(
    term_pattern_1,
    nm,
    fixed = TRUE
  )

  if (!is.null(term_pattern_2)) {
    hit <- hit & grepl(
      term_pattern_2,
      nm,
      fixed = TRUE
    )
  }

  terms <- nm[hit]

  assert_05(
    length(terms) == 1L,
    paste0(
      "Interazione non trovata univocamente in ",
      model_name,
      ": ", paste(nm, collapse = ", ")
    )
  )

  term <- terms[1]
  est <- unname(b[term])
  se <- sqrt(V[term, term])
  G <- n_distinct(data$zona)
  df_c <- G - 1L
  tval <- est / se
  pval <- 2 * stats::pt(
    abs(tval),
    df = df_c,
    lower.tail = FALSE
  )
  crit <- stats::qt(0.975, df = df_c)

  data.frame(
    modello = model_name,
    outcome = outcome_name,
    specifica = specification,
    termine = term,
    stima = est,
    errore_standard = se,
    statistica_t = tval,
    p_value = pval,
    ci95_low = est - crit * se,
    ci95_high = est + crit * se,
    n = stats::nobs(fit),
    n_cluster = G,
    df_cluster = df_c,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 2. Read canonical inputs / structural audit
# ------------------------------------------------------------------------------
master <- utils::read.csv(
  MASTER_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

master_input_nrow <- nrow(master)
master_input_ncol <- ncol(master)

required_master <- c(
  "zona", "fascia", "semestre", "period_index",
  "loc_min", "loc_max", "loc_medio",
  "compr_min", "compr_max", "compr_medio",
  "log_rent", "log_sale",
  "d_log_rent", "d_log_sale",
  "valid_rent_difference", "valid_sale_difference",
  "superficie_loc", "superficie_compr",
  "distance_to_B_core_km", "rep_lon", "rep_lat",
  "rdc_exposure_main"
)

assert_05(
  all(required_master %in% names(master)),
  paste0(
    "Colonne mancanti nel master: ",
    paste(
      setdiff(required_master, names(master)),
      collapse = ", "
    )
  )
)

assert_05(
  master_input_nrow == 1003L &&
    master_input_ncol == 78L,
  paste0(
    "Master canonico inatteso: ",
    master_input_nrow, " x ", master_input_ncol,
    "; atteso 1003 x 78."
  )
)

master <- master |>
  mutate(
    zona = as.character(zona),
    fascia = factor(
      as.character(fascia),
      levels = c("B", "C", "D", "E")
    ),
    semestre = as.character(semestre)
  ) |>
  arrange(zona, period_index)

assert_05(
  n_distinct(master$zona) == 59L,
  "Attese 59 zone bilanciate."
)
assert_05(
  all(table(master$zona) == 17L),
  "Il master non e' 59 x 17."
)

neighbors <- utils::read.csv(
  NEIGHBOR_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

assert_05(
  all(c("zona_1", "zona_2") %in% names(neighbors)),
  "zone_neighbors.csv non contiene zona_1/zona_2."
)
assert_05(
  nrow(neighbors) == 131L,
  paste0(
    "Attese 131 coppie di vicinato; trovate ",
    nrow(neighbors), "."
  )
)

geography <- utils::read.csv(
  GEOGRAPHY_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

assert_05(
  nrow(geography) == 60L &&
    n_distinct(geography$zona) == 60L,
  "zone_geography.csv deve avere 60 zone."
)

detail60 <- utils::read.csv(
  DETAIL60_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

exposure60 <- utils::read.csv(
  EXPOSURE60_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

assert_05(
  nrow(exposure60) == 60L &&
    n_distinct(exposure60$zona) == 60L,
  "12b_rdc_exposure_zone_60.csv deve avere 60 zone."
)

# ------------------------------------------------------------------------------
# 3. Derived endpoint differences and PRE-policy 2018 baselines
# ------------------------------------------------------------------------------
safe_log <- function(x) {
  ifelse(
    is.finite(x) & x > 0,
    log(x),
    NA_real_
  )
}

master <- master |>
  group_by(zona) |>
  arrange(period_index, .by_group = TRUE) |>
  mutate(
    prev_loc_min = lag(loc_min),
    prev_loc_max = lag(loc_max),
    prev_compr_min = lag(compr_min),
    prev_compr_max = lag(compr_max),
    prev_superficie_loc = lag(superficie_loc),
    prev_superficie_compr = lag(superficie_compr),

    d_log_loc_min = ifelse(
      superficie_loc == prev_superficie_loc &
        is.finite(loc_min) &
        is.finite(prev_loc_min) &
        loc_min > 0 &
        prev_loc_min > 0,
      log(loc_min) - log(prev_loc_min),
      NA_real_
    ),

    d_log_loc_max = ifelse(
      superficie_loc == prev_superficie_loc &
        is.finite(loc_max) &
        is.finite(prev_loc_max) &
        loc_max > 0 &
        prev_loc_max > 0,
      log(loc_max) - log(prev_loc_max),
      NA_real_
    ),

    d_log_compr_min = ifelse(
      superficie_compr == prev_superficie_compr &
        is.finite(compr_min) &
        is.finite(prev_compr_min) &
        compr_min > 0 &
        prev_compr_min > 0,
      log(compr_min) - log(prev_compr_min),
      NA_real_
    ),

    d_log_compr_max = ifelse(
      superficie_compr == prev_superficie_compr &
        is.finite(compr_max) &
        is.finite(prev_compr_max) &
        compr_max > 0 &
        prev_compr_max > 0,
      log(compr_max) - log(prev_compr_max),
      NA_real_
    )
  ) |>
  ungroup()

baseline2018 <- master |>
  filter(
    semestre %in% c("2018-1", "2018-2")
  ) |>
  group_by(zona) |>
  summarise(
    baseline_log_rent_2018 =
      mean(log_rent),
    baseline_log_sale_2018 =
      mean(log_sale),

    baseline_log_loc_min_2018 =
      mean(safe_log(loc_min)),
    baseline_log_loc_max_2018 =
      mean(safe_log(loc_max)),

    baseline_log_compr_min_2018 =
      mean(safe_log(compr_min)),
    baseline_log_compr_max_2018 =
      mean(safe_log(compr_max)),

    n_2018 = n(),
    .groups = "drop"
  )

assert_05(
  nrow(baseline2018) == 59L &&
    all(baseline2018$n_2018 == 2L),
  "Baseline 2018 deve essere 59 x 2."
)

make_event_cs <- function(semester) {
  d <- master |>
    filter(semestre == semester) |>
    left_join(
      baseline2018,
      by = "zona"
    ) |>
    mutate(
      baseline_rent_c =
        baseline_log_rent_2018 -
        mean(baseline_log_rent_2018),
      baseline_sale_c =
        baseline_log_sale_2018 -
        mean(baseline_log_sale_2018),

      baseline_loc_min_c =
        baseline_log_loc_min_2018 -
        mean(baseline_log_loc_min_2018),
      baseline_loc_max_c =
        baseline_log_loc_max_2018 -
        mean(baseline_log_loc_max_2018),

      baseline_compr_min_c =
        baseline_log_compr_min_2018 -
        mean(baseline_log_compr_min_2018),
      baseline_compr_max_c =
        baseline_log_compr_max_2018 -
        mean(baseline_log_compr_max_2018)
    )

  assert_05(
    nrow(d) == 59L,
    paste0(
      "Cross-section ", semester,
      " deve avere 59 zone."
    )
  )
  d
}

cs2019 <- make_event_cs("2019-2")
cs2023 <- make_event_cs("2023-2")

assert_05(
  all(
    cs2019$superficie_loc ==
      cs2019$prev_superficie_loc
  ) &&
    all(
      cs2023$superficie_loc ==
        cs2023$prev_superficie_loc
    ),
  "2019H2/2023H2 attraversano un cambio di superficie rent inatteso."
)

# ------------------------------------------------------------------------------
# 4. ROBUSTNESS 1: lower / midpoint / upper OMI endpoints
# ------------------------------------------------------------------------------
endpoint_specs <- data.frame(
  endpoint = c(
    "Rent lower bound",
    "Rent midpoint",
    "Rent upper bound",
    "Sale lower bound",
    "Sale midpoint",
    "Sale upper bound"
  ),
  outcome = c(
    "d_log_loc_min",
    "d_log_rent",
    "d_log_loc_max",
    "d_log_compr_min",
    "d_log_sale",
    "d_log_compr_max"
  ),
  baseline = c(
    "baseline_loc_min_c",
    "baseline_rent_c",
    "baseline_loc_max_c",
    "baseline_compr_min_c",
    "baseline_sale_c",
    "baseline_compr_max_c"
  ),
  market = c(
    "Rent", "Rent", "Rent",
    "Sale", "Sale", "Sale"
  ),
  stringsAsFactors = FALSE
)

endpoint_rows <- list()
k <- 1L

for (event_sem in c("2019-2", "2023-2")) {
  d <- if (event_sem == "2019-2") {
    cs2019
  } else {
    cs2023
  }

  for (i in seq_len(nrow(endpoint_specs))) {
    ep <- endpoint_specs[i, ]
    y <- ep$outcome
    bline <- ep$baseline

    assert_05(
      all(is.finite(d[[y]])),
      paste0(
        "Outcome endpoint mancante: ",
        event_sem, " / ", y
      )
    )

    # Urban gradient: C/D/E relative to B.
    f_band <- stats::lm(
      stats::as.formula(
        paste0(y, " ~ fascia")
      ),
      data = d
    )

    tab_band <- tidy_lm_hc3(
      f_band,
      model_name = paste0(
        event_sem, "_", y, "_BAND"
      ),
      outcome_name = y,
      specification = "fascia; B reference"
    ) |>
      filter(
        termine %in% c(
          "fasciaC", "fasciaD", "fasciaE"
        )
      ) |>
      mutate(
        evento = event_sem,
        endpoint = ep$endpoint,
        market = ep$market,
        modello_tipo = "Band differential"
      )

    endpoint_rows[[k]] <- tab_band
    k <- k + 1L

    # RdC exposure M1.
    f_m1 <- stats::lm(
      stats::as.formula(
        paste0(
          y,
          " ~ fascia + rdc_exposure_main"
        )
      ),
      data = d
    )

    r_m1 <- extract_hc3(
      f_m1,
      "rdc_exposure_main",
      model_name = paste0(
        event_sem, "_", y, "_RDC_M1"
      ),
      outcome_name = y,
      specification = "fascia + RdC exposure"
    ) |>
      mutate(
        evento = event_sem,
        endpoint = ep$endpoint,
        market = ep$market,
        modello_tipo = "RdC M1"
      )

    endpoint_rows[[k]] <- r_m1
    k <- k + 1L

    # RdC exposure + endpoint-specific pre-policy initial level.
    f_m2 <- stats::lm(
      stats::as.formula(
        paste0(
          y,
          " ~ fascia + rdc_exposure_main + ",
          bline
        )
      ),
      data = d
    )

    r_m2 <- extract_hc3(
      f_m2,
      "rdc_exposure_main",
      model_name = paste0(
        event_sem, "_", y, "_RDC_M2"
      ),
      outcome_name = y,
      specification = paste0(
        "fascia + RdC exposure + ",
        bline
      )
    ) |>
      mutate(
        evento = event_sem,
        endpoint = ep$endpoint,
        market = ep$market,
        modello_tipo = "RdC M2 initial-level adjusted"
      )

    endpoint_rows[[k]] <- r_m2
    k <- k + 1L
  }
}

endpoint_results <- bind_rows(
  endpoint_rows
) |>
  select(
    evento,
    market,
    endpoint,
    modello_tipo,
    everything()
  ) |>
  arrange(
    evento,
    market,
    endpoint,
    modello_tipo,
    termine
  )

write_csv_05(
  endpoint_results,
  "61_endpoint_robustness_2019_2023.csv"
)

# ------------------------------------------------------------------------------
# 5. ROBUSTNESS 2: distance gradient formal event interaction
# ------------------------------------------------------------------------------
fit_distance_event <- function(
  pre_semester,
  post_semester,
  event_label,
  outcome
) {
  d <- master |>
    filter(
      semestre %in% c(
        pre_semester,
        post_semester
      )
    ) |>
    mutate(
      post_event = as.integer(
        semestre == post_semester
      )
    )

  assert_05(
    nrow(d) == 118L,
    paste0(
      "Distance event ", event_label,
      " deve avere 118 righe."
    )
  )

  # Individual-period slopes, HC3.
  slopes <- lapply(
    c(pre_semester, post_semester),
    function(s) {
      ds <- d |>
        filter(semestre == s)

      fit <- stats::lm(
        stats::as.formula(
          paste0(
            outcome,
            " ~ distance_to_B_core_km"
          )
        ),
        data = ds
      )

      extract_hc3(
        fit,
        "distance_to_B_core_km",
        model_name = paste0(
          event_label, "_", outcome, "_", s
        ),
        outcome_name = outcome,
        specification = "cross-sectional distance gradient"
      ) |>
        mutate(
          evento = event_label,
          periodo = s,
          result_type = "Slope"
        )
    }
  ) |>
    bind_rows()

  # Change in slope across the two periods, clustered by zone.
  interaction <- fit_fixest_cluster_term(
    stats::as.formula(
      paste0(
        outcome,
        " ~ distance_to_B_core_km * post_event"
      )
    ),
    data = d,
    term_pattern_1 = "distance_to_B_core_km",
    term_pattern_2 = "post_event",
    model_name = paste0(
      event_label, "_", outcome, "_interaction"
    ),
    outcome_name = outcome,
    specification = paste0(
      pre_semester, " -> ", post_semester,
      "; distance x post; SE cluster zone"
    )
  ) |>
    mutate(
      evento = event_label,
      periodo = paste0(
        pre_semester, " -> ", post_semester
      ),
      result_type = "Slope change"
    )

  bind_rows(
    slopes,
    interaction
  )
}

distance_results <- bind_rows(
  fit_distance_event(
    "2019-1", "2019-2",
    "2019 flattening",
    "log_rent"
  ),
  fit_distance_event(
    "2019-1", "2019-2",
    "2019 flattening",
    "log_sale"
  ),
  fit_distance_event(
    "2023-1", "2023-2",
    "2023 re-steepening",
    "log_rent"
  ),
  fit_distance_event(
    "2023-1", "2023-2",
    "2023 re-steepening",
    "log_sale"
  )
) |>
  arrange(
    evento,
    outcome,
    result_type,
    periodo
  )

write_csv_05(
  distance_results,
  "62_distance_gradient_robustness.csv"
)

# ------------------------------------------------------------------------------
# 6. ROBUSTNESS 3: 59 vs 60 zones, 2019H2
# ------------------------------------------------------------------------------
detail60_t20 <- detail60 |>
  filter(cod_tip == 20L) |>
  transmute(
    zona = as.character(zona),
    fascia = factor(
      as.character(fascia),
      levels = c("B", "C", "D", "E")
    ),
    d_log_rent = as.numeric(d_log_rent),
    d_log_sale = as.numeric(d_log_sale),
    rent_initial = as.numeric(rent_initial),
    sale_initial = as.numeric(sale_initial),
    same_surface_rent =
      as.logical(same_surface_rent),
    same_surface_sale =
      as.logical(same_surface_sale)
  )

assert_05(
  nrow(detail60_t20) == 60L &&
    n_distinct(detail60_t20$zona) == 60L,
  "Il cross-section Type20 2019 deve avere 60 zone."
)
assert_05(
  all(detail60_t20$same_surface_rent) &&
    all(detail60_t20$same_surface_sale),
  "Il cross-section 2019 60-zone ha surface mismatch."
)

detail60_t20 <- detail60_t20 |>
  left_join(
    exposure60 |>
      transmute(
        zona = as.character(zona),
        balanced_panel = as.logical(
          balanced_panel
        ),
        rdc_exposure_main =
          as.numeric(rdc_exposure_main)
      ),
    by = "zona"
  ) |>
  mutate(
    log_rent_initial =
      log(rent_initial),
    log_rent_initial_c =
      log_rent_initial -
      mean(log_rent_initial)
  )

assert_05(
  sum(detail60_t20$balanced_panel) == 59L,
  "Il flag balanced_panel non identifica 59/60 zone."
)

extra_60 <- setdiff(
  detail60_t20$zona[
    !detail60_t20$balanced_panel
  ],
  character(0)
)

assert_05(
  identical(sort(extra_60), "D32"),
  paste0(
    "La zona extra attesa nel 2019 e' D32; trovata: ",
    paste(extra_60, collapse = ", ")
  )
)

fit_59_60 <- function(d, sample_name) {
  # Re-centering is sample-specific.
  d <- d |>
    mutate(
      log_rent_initial_c =
        log_rent_initial -
        mean(log_rent_initial)
    )

  models <- list(
    BAND = stats::lm(
      d_log_rent ~ fascia,
      data = d
    ),
    RDC_M1 = stats::lm(
      d_log_rent ~
        fascia +
        rdc_exposure_main,
      data = d
    ),
    RDC_M5 = stats::lm(
      d_log_rent ~
        fascia +
        rdc_exposure_main +
        log_rent_initial_c,
      data = d
    )
  )

  out <- list()
  kk <- 1L

  for (nm in names(models)) {
    tab <- tidy_lm_hc3(
      models[[nm]],
      model_name = paste0(
        sample_name, "_", nm
      ),
      outcome_name = "d_log_rent_2019H2",
      specification = switch(
        nm,
        BAND = "fascia; B reference",
        RDC_M1 = "fascia + RdC exposure",
        RDC_M5 = paste0(
          "fascia + RdC exposure + ",
          "log rent 2019H1"
        )
      )
    ) |>
      filter(
        termine %in% c(
          "fasciaC",
          "fasciaD",
          "fasciaE",
          "rdc_exposure_main"
        )
      ) |>
      mutate(
        sample = sample_name
      )

    out[[kk]] <- tab
    kk <- kk + 1L
  }

  bind_rows(out)
}

sample_59_60 <- bind_rows(
  fit_59_60(
    detail60_t20 |>
      filter(balanced_panel),
    "59 balanced zones"
  ),
  fit_59_60(
    detail60_t20,
    "60 zones incl. D32"
  )
) |>
  select(
    sample,
    everything()
  ) |>
  arrange(
    sample,
    modello,
    termine
  )

write_csv_05(
  sample_59_60,
  "63_sample_59_vs_60_2019.csv"
)

# ------------------------------------------------------------------------------
# 7. ROBUSTNESS 4: leave-one-out, 2019
# ------------------------------------------------------------------------------
fit_loo_2019 <- function(d) {
  d <- d |>
    mutate(
      baseline_rent_c =
        baseline_log_rent_2018 -
        mean(baseline_log_rent_2018)
    )

  band_fit <- stats::lm(
    d_log_rent ~ fascia,
    data = d
  )
  m1_fit <- stats::lm(
    d_log_rent ~
      fascia +
      rdc_exposure_main,
    data = d
  )
  m2_fit <- stats::lm(
    d_log_rent ~
      fascia +
      rdc_exposure_main +
      baseline_rent_c,
    data = d
  )

  list(
    D = extract_hc3(
      band_fit,
      "fasciaD",
      "LOO_band",
      "d_log_rent_2019H2",
      "fascia"
    ),
    E = extract_hc3(
      band_fit,
      "fasciaE",
      "LOO_band",
      "d_log_rent_2019H2",
      "fascia"
    ),
    M1 = extract_hc3(
      m1_fit,
      "rdc_exposure_main",
      "LOO_M1",
      "d_log_rent_2019H2",
      "fascia + RdC exposure"
    ),
    M2 = extract_hc3(
      m2_fit,
      "rdc_exposure_main",
      "LOO_M2",
      "d_log_rent_2019H2",
      paste0(
        "fascia + RdC exposure + ",
        "average log rent 2018"
      )
    )
  )
}

loo_2019_rows <- lapply(
  as.character(cs2019$zona),
  function(z_drop) {
    d <- cs2019 |>
      filter(zona != z_drop)

    res <- fit_loo_2019(d)

    data.frame(
      dropped_zone = z_drop,
      dropped_band = as.character(
        cs2019$fascia[
          cs2019$zona == z_drop
        ][1]
      ),

      band_D_est = res$D$stima,
      band_D_p = res$D$p_value,
      band_E_est = res$E$stima,
      band_E_p = res$E$p_value,

      rdc_M1_est = res$M1$stima,
      rdc_M1_p = res$M1$p_value,
      rdc_M1_ci_low = res$M1$ci95_low,
      rdc_M1_ci_high = res$M1$ci95_high,

      rdc_M2_est = res$M2$stima,
      rdc_M2_p = res$M2$p_value,
      rdc_M2_ci_low = res$M2$ci95_low,
      rdc_M2_ci_high = res$M2$ci95_high,

      stringsAsFactors = FALSE
    )
  }
) |>
  bind_rows()

assert_05(
  nrow(loo_2019_rows) == 59L,
  "Leave-one-out 2019 deve avere 59 righe."
)

write_csv_05(
  loo_2019_rows,
  "64_leave_one_out_2019.csv"
)

# 2023: leave-one-out only for the URBAN re-steepening result.
loo_2023_rows <- lapply(
  as.character(cs2023$zona),
  function(z_drop) {
    d <- cs2023 |>
      filter(zona != z_drop)

    fit <- stats::lm(
      d_log_rent ~ fascia,
      data = d
    )

    d_row <- extract_hc3(
      fit,
      "fasciaD",
      "LOO_2023_band",
      "d_log_rent_2023H2",
      "fascia"
    )
    e_row <- extract_hc3(
      fit,
      "fasciaE",
      "LOO_2023_band",
      "d_log_rent_2023H2",
      "fascia"
    )

    data.frame(
      dropped_zone = z_drop,
      dropped_band = as.character(
        cs2023$fascia[
          cs2023$zona == z_drop
        ][1]
      ),
      band_D_est = d_row$stima,
      band_D_p = d_row$p_value,
      band_E_est = e_row$stima,
      band_E_p = e_row$p_value,
      stringsAsFactors = FALSE
    )
  }
) |>
  bind_rows()

assert_05(
  nrow(loo_2023_rows) == 59L,
  "Leave-one-out 2023 deve avere 59 righe."
)

write_csv_05(
  loo_2023_rows,
  "64b_leave_one_out_2023_gradient.csv"
)

# ------------------------------------------------------------------------------
# 8. ROBUSTNESS 5: boundary-sensitive zones
# ------------------------------------------------------------------------------
# Pre-specificato dal precedente audit geografico:
# >10% polygon symmetric difference across vintages.
BOUNDARY_SENSITIVE <- c(
  "B13", "B16", "C31", "E45"
)

assert_05(
  all(
    BOUNDARY_SENSITIVE %in%
      as.character(unique(master$zona))
  ),
  "Una boundary-sensitive zone non e' nel balanced panel."
)

fit_boundary_scenario <- function(
  d,
  event_semester,
  scenario,
  dropped
) {
  d2 <- d |>
    filter(
      !zona %in% dropped
    ) |>
    mutate(
      baseline_rent_c =
        baseline_log_rent_2018 -
        mean(baseline_log_rent_2018)
    )

  band <- stats::lm(
    d_log_rent ~ fascia,
    data = d2
  )

  out <- bind_rows(
    extract_hc3(
      band,
      "fasciaC",
      paste0(scenario, "_band"),
      paste0(
        "d_log_rent_", event_semester
      ),
      "fascia"
    ),
    extract_hc3(
      band,
      "fasciaD",
      paste0(scenario, "_band"),
      paste0(
        "d_log_rent_", event_semester
      ),
      "fascia"
    ),
    extract_hc3(
      band,
      "fasciaE",
      paste0(scenario, "_band"),
      paste0(
        "d_log_rent_", event_semester
      ),
      "fascia"
    )
  )

  if (event_semester == "2019-2") {
    m1 <- stats::lm(
      d_log_rent ~
        fascia +
        rdc_exposure_main,
      data = d2
    )
    m2 <- stats::lm(
      d_log_rent ~
        fascia +
        rdc_exposure_main +
        baseline_rent_c,
      data = d2
    )

    out <- bind_rows(
      out,
      extract_hc3(
        m1,
        "rdc_exposure_main",
        paste0(scenario, "_RDC_M1"),
        "d_log_rent_2019H2",
        "fascia + RdC exposure"
      ),
      extract_hc3(
        m2,
        "rdc_exposure_main",
        paste0(scenario, "_RDC_M2"),
        "d_log_rent_2019H2",
        paste0(
          "fascia + RdC exposure + ",
          "average log rent 2018"
        )
      )
    )
  }

  out |>
    mutate(
      evento = event_semester,
      scenario = scenario,
      dropped_zones = ifelse(
        length(dropped) == 0,
        "None",
        paste(dropped, collapse = "|")
      ),
      n_remaining = nrow(d2)
    )
}

boundary_results <- bind_rows(
  fit_boundary_scenario(
    cs2019,
    "2019-2",
    "All 59",
    character(0)
  ),
  fit_boundary_scenario(
    cs2019,
    "2019-2",
    "Drop >10% boundary-shift zones",
    BOUNDARY_SENSITIVE
  ),
  fit_boundary_scenario(
    cs2023,
    "2023-2",
    "All 59",
    character(0)
  ),
  fit_boundary_scenario(
    cs2023,
    "2023-2",
    "Drop >10% boundary-shift zones",
    BOUNDARY_SENSITIVE
  )
) |>
  select(
    evento,
    scenario,
    dropped_zones,
    n_remaining,
    everything()
  )

write_csv_05(
  boundary_results,
  "65_boundary_sensitivity.csv"
)

# ------------------------------------------------------------------------------
# 9. ROBUSTNESS 6: Moran's I residual diagnostics
# ------------------------------------------------------------------------------
# Build symmetric binary adjacency using the 131 unordered pairs.
zones59 <- sort(
  unique(as.character(master$zona))
)

zone_index <- setNames(
  seq_along(zones59),
  zones59
)

neighbor_zones <- sort(
  unique(
    c(
      as.character(neighbors$zona_1),
      as.character(neighbors$zona_2)
    )
  )
)

assert_05(
  identical(neighbor_zones, zones59),
  "Il neighbor graph non usa esattamente le 59 zone bilanciate."
)

edge_i <- unname(
  zone_index[
    as.character(neighbors$zona_1)
  ]
)
edge_j <- unname(
  zone_index[
    as.character(neighbors$zona_2)
  ]
)

assert_05(
  all(is.finite(edge_i)) &&
    all(is.finite(edge_j)),
  "Coppia di vicinato non mappata."
)

degree <- tabulate(
  c(edge_i, edge_j),
  nbins = length(zones59)
)

assert_05(
  all(degree > 0),
  "Esiste almeno una zona isolata nel grafo."
)

# Simple connectivity check.
adj_list <- vector(
  "list",
  length(zones59)
)
for (e in seq_along(edge_i)) {
  adj_list[[edge_i[e]]] <- c(
    adj_list[[edge_i[e]]],
    edge_j[e]
  )
  adj_list[[edge_j[e]]] <- c(
    adj_list[[edge_j[e]]],
    edge_i[e]
  )
}

visited <- rep(FALSE, length(zones59))
queue <- 1L
visited[1] <- TRUE

while (length(queue) > 0L) {
  current <- queue[1]
  queue <- queue[-1]

  nb <- adj_list[[current]]
  new_nb <- nb[!visited[nb]]

  if (length(new_nb)) {
    visited[new_nb] <- TRUE
    queue <- c(queue, new_nb)
  }
}

assert_05(
  all(visited),
  "Il neighbor graph delle 59 zone non e' connesso."
)

moran_I_edges <- function(x) {
  x <- as.numeric(x)
  assert_05(
    length(x) == length(zones59),
    "Lunghezza vettore errata per Moran I."
  )
  assert_05(
    all(is.finite(x)),
    "Valori non finiti nel Moran I."
  )

  z <- x - mean(x)
  denom <- sum(z^2)

  assert_05(
    denom > 0,
    "Varianza nulla nel Moran I."
  )

  # W e' simmetrica: ogni edge unordered vale due volte nella doppia somma.
  numerator <- 2 * sum(
    z[edge_i] * z[edge_j]
  )
  S0 <- 2 * length(edge_i)
  n <- length(z)

  (n / S0) * numerator / denom
}

moran_perm_test <- function(
  x,
  label,
  B = 9999L,
  seed = 20260829L
) {
  x <- as.numeric(x)
  obs <- moran_I_edges(x)
  expected <- -1 / (length(x) - 1)

  set.seed(seed)
  perm_I <- numeric(B)

  for (b in seq_len(B)) {
    perm_I[b] <- moran_I_edges(
      sample(x, replace = FALSE)
    )
  }

  p_greater <- (
    1 + sum(perm_I >= obs)
  ) / (B + 1)

  p_two <- (
    1 + sum(
      abs(perm_I - expected) >=
        abs(obs - expected)
    )
  ) / (B + 1)

  data.frame(
    diagnostic = label,
    moran_I = obs,
    expected_I = expected,
    p_greater = p_greater,
    p_two_sided = p_two,
    permutations = B,
    n = length(x),
    n_edges = length(edge_i),
    stringsAsFactors = FALSE
  )
}

prepare_spatial_cs <- function(d) {
  d |>
    arrange(match(zona, zones59)) |>
    mutate(
      baseline_rent_c =
        baseline_log_rent_2018 -
        mean(baseline_log_rent_2018)
    )
}

sp19 <- prepare_spatial_cs(cs2019)
sp23 <- prepare_spatial_cs(cs2023)

assert_05(
  identical(
    as.character(sp19$zona),
    zones59
  ) &&
    identical(
      as.character(sp23$zona),
      zones59
    ),
  "Ordine zone errato per Moran."
)

fit19_band <- lm(
  d_log_rent ~ fascia,
  data = sp19
)
fit19_base <- lm(
  d_log_rent ~
    fascia +
    baseline_rent_c,
  data = sp19
)
fit19_base_rdc <- lm(
  d_log_rent ~
    fascia +
    baseline_rent_c +
    rdc_exposure_main,
  data = sp19
)
fit19_sale_band <- lm(
  d_log_sale ~ fascia,
  data = sp19
)

fit23_band <- lm(
  d_log_rent ~ fascia,
  data = sp23
)

moran_results <- bind_rows(
  moran_perm_test(
    sp19$d_log_rent,
    "2019H2 rent change - raw"
  ),
  moran_perm_test(
    residuals(fit19_band),
    "2019H2 rent residual - fascia"
  ),
  moran_perm_test(
    residuals(fit19_base),
    paste0(
      "2019H2 rent residual - fascia + ",
      "average 2018 rent"
    )
  ),
  moran_perm_test(
    residuals(fit19_base_rdc),
    paste0(
      "2019H2 rent residual - fascia + ",
      "average 2018 rent + RdC"
    )
  ),
  moran_perm_test(
    residuals(fit19_sale_band),
    "2019H2 sale residual - fascia"
  ),
  moran_perm_test(
    sp23$d_log_rent,
    "2023H2 rent change - raw"
  ),
  moran_perm_test(
    residuals(fit23_band),
    "2023H2 rent residual - fascia"
  )
)

write_csv_05(
  moran_results,
  "66_moran_residual_spatial_tests.csv"
)

# ------------------------------------------------------------------------------
# 10. ROBUSTNESS 7: within-band permutation inference for RdC
# ------------------------------------------------------------------------------
residualize_qr <- function(v, X) {
  qr.resid(
    qr(X),
    as.numeric(v)
  )
}

permute_within_group <- function(
  x,
  group
) {
  out <- x
  idx_split <- split(
    seq_along(x),
    group
  )
  for (idx in idx_split) {
    out[idx] <- sample(
      x[idx],
      replace = FALSE
    )
  }
  out
}

within_band_perm_rdc <- function(
  data,
  adjusted = FALSE,
  B = 9999L,
  seed = 20260829L
) {
  d <- data |>
    mutate(
      baseline_rent_c =
        baseline_log_rent_2018 -
        mean(baseline_log_rent_2018)
    )

  if (!adjusted) {
    Xc <- model.matrix(
      ~ fascia,
      data = d
    )
    label <- "M1: fascia + RdC exposure"
  } else {
    Xc <- model.matrix(
      ~ fascia + baseline_rent_c,
      data = d
    )
    label <- paste0(
      "M2: fascia + RdC exposure + ",
      "average log rent 2018"
    )
  }

  y_res <- residualize_qr(
    d$d_log_rent,
    Xc
  )

  x_obs <- d$rdc_exposure_main
  x_res <- residualize_qr(
    x_obs,
    Xc
  )

  beta_obs <- sum(
    x_res * y_res
  ) / sum(
    x_res^2
  )

  set.seed(seed)
  beta_perm <- numeric(B)

  for (b in seq_len(B)) {
    x_p <- permute_within_group(
      x_obs,
      d$fascia
    )
    x_pr <- residualize_qr(
      x_p,
      Xc
    )

    beta_perm[b] <- sum(
      x_pr * y_res
    ) / sum(
      x_pr^2
    )
  }

  p_two <- (
    1 + sum(
      abs(beta_perm) >= abs(beta_obs)
    )
  ) / (B + 1)

  data.frame(
    modello = label,
    adjusted_initial_rent = adjusted,
    beta_observed = beta_obs,
    permutation_p_two_sided = p_two,
    permutations = B,
    permutation_mean = mean(beta_perm),
    permutation_sd = sd(beta_perm),
    permutation_q025 =
      unname(
        quantile(
          beta_perm,
          0.025
        )
      ),
    permutation_q975 =
      unname(
        quantile(
          beta_perm,
          0.975
        )
      ),
    n = nrow(d),
    strata = "OMI fascia B/C/D/E",
    note = paste0(
      "Permutation robustness only; ",
      "not a causal randomization test."
    ),
    stringsAsFactors = FALSE
  )
}

permutation_results <- bind_rows(
  within_band_perm_rdc(
    cs2019,
    adjusted = FALSE
  ),
  within_band_perm_rdc(
    cs2019,
    adjusted = TRUE
  )
)

write_csv_05(
  permutation_results,
  "67_within_band_permutation_rdc.csv"
)

# ------------------------------------------------------------------------------
# 11. Summary table
# ------------------------------------------------------------------------------
get_ep <- function(
  event_value,
  endpoint_value,
  model_type_value,
  term_value
) {
  hit <- endpoint_results |>
    filter(
      .data$evento == .env$event_value,
      .data$endpoint == .env$endpoint_value,
      .data$modello_tipo == .env$model_type_value,
      .data$termine == .env$term_value
    )

  assert_05(
    nrow(hit) == 1L,
    paste0(
      "Endpoint summary row non univoca: ",
      event_value, " / ", endpoint_value,
      " / ", model_type_value, " / ", term_value,
      " | righe trovate = ", nrow(hit)
    )
  )

  hit
}

get_dist <- function(
  event_value,
  outcome_value
) {
  hit <- distance_results |>
    filter(
      .data$evento == .env$event_value,
      .data$outcome == .env$outcome_value,
      .data$result_type == "Slope change"
    )

  assert_05(
    nrow(hit) == 1L,
    paste0(
      "Distance summary row non univoca: ",
      event_value, " / ", outcome_value,
      " | righe trovate = ", nrow(hit)
    )
  )

  hit
}

ep19_min_rdc <- get_ep(
  "2019-2",
  "Rent lower bound",
  "RdC M1",
  "rdc_exposure_main"
)
ep19_mid_rdc <- get_ep(
  "2019-2",
  "Rent midpoint",
  "RdC M1",
  "rdc_exposure_main"
)
ep19_max_rdc <- get_ep(
  "2019-2",
  "Rent upper bound",
  "RdC M1",
  "rdc_exposure_main"
)

ep19_min_adj <- get_ep(
  "2019-2",
  "Rent lower bound",
  "RdC M2 initial-level adjusted",
  "rdc_exposure_main"
)
ep19_mid_adj <- get_ep(
  "2019-2",
  "Rent midpoint",
  "RdC M2 initial-level adjusted",
  "rdc_exposure_main"
)
ep19_max_adj <- get_ep(
  "2019-2",
  "Rent upper bound",
  "RdC M2 initial-level adjusted",
  "rdc_exposure_main"
)

dist19_r <- get_dist(
  "2019 flattening",
  "log_rent"
)
dist19_s <- get_dist(
  "2019 flattening",
  "log_sale"
)
dist23_r <- get_dist(
  "2023 re-steepening",
  "log_rent"
)
dist23_s <- get_dist(
  "2023 re-steepening",
  "log_sale"
)

# Sample robustness: extract M1 exposure 59/60.
s59 <- sample_59_60 |>
  filter(
    sample == "59 balanced zones",
    grepl("RDC_M1$", modello),
    termine == "rdc_exposure_main"
  )
s60 <- sample_59_60 |>
  filter(
    sample == "60 zones incl. D32",
    grepl("RDC_M1$", modello),
    termine == "rdc_exposure_main"
  )

assert_05(
  nrow(s59) == 1L &&
    nrow(s60) == 1L,
  "59/60 exposure row mancante."
)

loo_summary <- data.frame(
  metric = c(
    "2019 RdC M1 leave-one-out minimum",
    "2019 RdC M1 leave-one-out maximum",
    "2019 RdC M1 maximum leave-one-out p",
    "2019 RdC M2 leave-one-out minimum",
    "2019 RdC M2 leave-one-out maximum",
    "2019 band D leave-one-out minimum",
    "2019 band D leave-one-out maximum",
    "2019 band E leave-one-out minimum",
    "2019 band E leave-one-out maximum",
    "2023 band D leave-one-out minimum",
    "2023 band D leave-one-out maximum",
    "2023 band E leave-one-out minimum",
    "2023 band E leave-one-out maximum"
  ),
  value = c(
    min(loo_2019_rows$rdc_M1_est),
    max(loo_2019_rows$rdc_M1_est),
    max(loo_2019_rows$rdc_M1_p),
    min(loo_2019_rows$rdc_M2_est),
    max(loo_2019_rows$rdc_M2_est),
    min(loo_2019_rows$band_D_est),
    max(loo_2019_rows$band_D_est),
    min(loo_2019_rows$band_E_est),
    max(loo_2019_rows$band_E_est),
    min(loo_2023_rows$band_D_est),
    max(loo_2023_rows$band_D_est),
    min(loo_2023_rows$band_E_est),
    max(loo_2023_rows$band_E_est)
  ),
  stringsAsFactors = FALSE
)

robustness_summary <- bind_rows(
  data.frame(
    result = c(
      "2019 RdC exposure - rent lower bound M1",
      "2019 RdC exposure - rent midpoint M1",
      "2019 RdC exposure - rent upper bound M1",
      "2019 RdC exposure - rent lower bound adjusted",
      "2019 RdC exposure - rent midpoint adjusted",
      "2019 RdC exposure - rent upper bound adjusted",
      "2019 distance-gradient slope change - rent",
      "2019 distance-gradient slope change - sale",
      "2023 distance-gradient slope change - rent",
      "2023 distance-gradient slope change - sale",
      "2019 RdC exposure - 59 zones",
      "2019 RdC exposure - 60 zones incl D32",
      "2019 Moran raw rent change",
      "2019 Moran residual fascia",
      "2019 Moran residual fascia + baseline",
      "2019 Moran residual fascia + baseline + RdC",
      "2019 within-band permutation M1",
      "2019 within-band permutation M2"
    ),
    estimate = c(
      ep19_min_rdc$stima,
      ep19_mid_rdc$stima,
      ep19_max_rdc$stima,
      ep19_min_adj$stima,
      ep19_mid_adj$stima,
      ep19_max_adj$stima,
      dist19_r$stima,
      dist19_s$stima,
      dist23_r$stima,
      dist23_s$stima,
      s59$stima,
      s60$stima,
      moran_results$moran_I[
        moran_results$diagnostic ==
          "2019H2 rent change - raw"
      ],
      moran_results$moran_I[
        moran_results$diagnostic ==
          "2019H2 rent residual - fascia"
      ],
      moran_results$moran_I[
        moran_results$diagnostic ==
          paste0(
            "2019H2 rent residual - fascia + ",
            "average 2018 rent"
          )
      ],
      moran_results$moran_I[
        moran_results$diagnostic ==
          paste0(
            "2019H2 rent residual - fascia + ",
            "average 2018 rent + RdC"
          )
      ],
      permutation_results$beta_observed[1],
      permutation_results$beta_observed[2]
    ),
    p_value = c(
      ep19_min_rdc$p_value,
      ep19_mid_rdc$p_value,
      ep19_max_rdc$p_value,
      ep19_min_adj$p_value,
      ep19_mid_adj$p_value,
      ep19_max_adj$p_value,
      dist19_r$p_value,
      dist19_s$p_value,
      dist23_r$p_value,
      dist23_s$p_value,
      s59$p_value,
      s60$p_value,
      moran_results$p_two_sided[
        moran_results$diagnostic ==
          "2019H2 rent change - raw"
      ],
      moran_results$p_two_sided[
        moran_results$diagnostic ==
          "2019H2 rent residual - fascia"
      ],
      moran_results$p_two_sided[
        moran_results$diagnostic ==
          paste0(
            "2019H2 rent residual - fascia + ",
            "average 2018 rent"
          )
      ],
      moran_results$p_two_sided[
        moran_results$diagnostic ==
          paste0(
            "2019H2 rent residual - fascia + ",
            "average 2018 rent + RdC"
          )
      ],
      permutation_results$permutation_p_two_sided[1],
      permutation_results$permutation_p_two_sided[2]
    ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    result = loo_summary$metric,
    estimate = loo_summary$value,
    p_value = NA_real_,
    stringsAsFactors = FALSE
  )
)

write_csv_05(
  robustness_summary,
  "68_robustness_summary.csv"
)

# ------------------------------------------------------------------------------
# 12. Audit
# ------------------------------------------------------------------------------
audit <- data.frame(
  controllo = c(
    "Master input rows",
    "Master input columns",
    "Balanced zones",
    "Rows per balanced zone",
    "Neighbor pairs",
    "Neighbor graph zones",
    "Neighbor graph no isolates",
    "Neighbor graph connected",
    "Geography zones",
    "2019 Type20 zones",
    "2019 Type20 same rent surface",
    "2019 Type20 same sale surface",
    "Exposure60 zones",
    "Balanced flag in Exposure60",
    "Only extra 2019 zone is D32",
    "2019 endpoint cross-section",
    "2023 endpoint cross-section",
    "Boundary-sensitive zones present",
    "Leave-one-out 2019 rows",
    "Leave-one-out 2023 rows",
    "Moran permutation count",
    "Within-band permutation count"
  ),
  valore = c(
    master_input_nrow,
    master_input_ncol,
    n_distinct(master$zona),
    paste(
      range(
        as.integer(
          table(master$zona)
        )
      ),
      collapse = "-"
    ),
    nrow(neighbors),
    length(neighbor_zones),
    sum(degree == 0),
    all(visited),
    nrow(geography),
    nrow(detail60_t20),
    all(detail60_t20$same_surface_rent),
    all(detail60_t20$same_surface_sale),
    nrow(exposure60),
    sum(detail60_t20$balanced_panel),
    paste(extra_60, collapse = "|"),
    nrow(cs2019),
    nrow(cs2023),
    paste(
      BOUNDARY_SENSITIVE,
      collapse = "|"
    ),
    nrow(loo_2019_rows),
    nrow(loo_2023_rows),
    unique(moran_results$permutations),
    unique(permutation_results$permutations)
  ),
  esito = c(
    ifelse(
      master_input_nrow == 1003L,
      "PASS", "FAIL"
    ),
    ifelse(
      master_input_ncol == 78L,
      "PASS", "FAIL"
    ),
    ifelse(
      n_distinct(master$zona) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      all(table(master$zona) == 17L),
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(neighbors) == 131L,
      "PASS", "FAIL"
    ),
    ifelse(
      length(neighbor_zones) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      all(degree > 0),
      "PASS", "FAIL"
    ),
    ifelse(
      all(visited),
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(geography) == 60L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(detail60_t20) == 60L,
      "PASS", "FAIL"
    ),
    ifelse(
      all(detail60_t20$same_surface_rent),
      "PASS", "FAIL"
    ),
    ifelse(
      all(detail60_t20$same_surface_sale),
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(exposure60) == 60L,
      "PASS", "FAIL"
    ),
    ifelse(
      sum(detail60_t20$balanced_panel) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      identical(sort(extra_60), "D32"),
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(cs2019) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(cs2023) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      all(
        BOUNDARY_SENSITIVE %in%
          as.character(unique(master$zona))
      ),
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(loo_2019_rows) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(loo_2023_rows) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      all(
        moran_results$permutations == 9999L
      ),
      "PASS", "FAIL"
    ),
    ifelse(
      all(
        permutation_results$permutations ==
          9999L
      ),
      "PASS", "FAIL"
    )
  ),
  stringsAsFactors = FALSE
)

write_csv_05(
  audit,
  "60_spatial_robustness_audit.csv"
)

failed <- audit |>
  filter(esito == "FAIL")

if (nrow(failed) > 0L) {
  message("")
  message("==============================================================")
  message("AUDIT 05 - CONTROLLI FALLITI")
  message("==============================================================")
  for (i in seq_len(nrow(failed))) {
    message(
      "- ", failed$controllo[i],
      " | valore = ", failed$valore[i]
    )
  }
  stop(
    paste0(
      "Audit 05 fallito in ",
      nrow(failed),
      " controllo/i."
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 13. Figures
# ------------------------------------------------------------------------------

# Fig 19 - endpoint robustness of 2019 RdC coefficient.
fig19_data <- endpoint_results |>
  filter(
    evento == "2019-2",
    market %in% c("Rent", "Sale"),
    modello_tipo %in% c(
      "RdC M1",
      "RdC M2 initial-level adjusted"
    ),
    termine == "rdc_exposure_main"
  ) |>
  mutate(
    endpoint = factor(
      endpoint,
      levels = c(
        "Rent lower bound",
        "Rent midpoint",
        "Rent upper bound",
        "Sale lower bound",
        "Sale midpoint",
        "Sale upper bound"
      )
    ),
    modello_tipo = factor(
      modello_tipo,
      levels = c(
        "RdC M1",
        "RdC M2 initial-level adjusted"
      ),
      labels = c(
        "Band-adjusted",
        "+ pre-policy initial level"
      )
    )
  )

fig19 <- ggplot(
  fig19_data,
  aes(
    x = endpoint,
    y = stima,
    shape = modello_tipo
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  geom_errorbar(
    aes(
      ymin = ci95_low,
      ymax = ci95_high
    ),
    width = 0.12,
    position = position_dodge(
      width = 0.35
    )
  ) +
  geom_point(
    size = 2.3,
    position = position_dodge(
      width = 0.35
    )
  ) +
  labs(
    title = "2019H2 RdC-exposure robustness across OMI quotation bounds",
    subtitle = "Lower bound, midpoint and upper bound; HC3 confidence intervals",
    x = NULL,
    y = "Coefficient per +1 SD RdC exposure",
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1
    )
  )

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig19_endpoint_rdc_2019.png"
  ),
  fig19,
  width = 10,
  height = 6,
  dpi = 300
)

# Fig 20 - leave-one-out M1/M2.
fig20_data <- loo_2019_rows |>
  select(
    dropped_zone,
    rdc_M1_est,
    rdc_M2_est
  ) |>
  pivot_longer(
    cols = c(
      rdc_M1_est,
      rdc_M2_est
    ),
    names_to = "model",
    values_to = "estimate"
  ) |>
  mutate(
    model = recode(
      model,
      rdc_M1_est = "M1: band + RdC",
      rdc_M2_est = "M2: + initial rent"
    )
  )

full_m1 <- extract_hc3(
  lm(
    d_log_rent ~
      fascia +
      rdc_exposure_main,
    data = cs2019
  ),
  "rdc_exposure_main",
  "Full_M1",
  "d_log_rent_2019H2",
  "fascia + RdC exposure"
)$stima

full_m2 <- extract_hc3(
  lm(
    d_log_rent ~
      fascia +
      rdc_exposure_main +
      baseline_rent_c,
    data = cs2019
  ),
  "rdc_exposure_main",
  "Full_M2",
  "d_log_rent_2019H2",
  "fascia + RdC exposure + average 2018 rent"
)$stima

fig20 <- ggplot(
  fig20_data,
  aes(
    x = dropped_zone,
    y = estimate,
    shape = model
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = full_m1,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = full_m2,
    linetype = "dotted",
    linewidth = 0.4
  ) +
  geom_point(
    size = 1.8,
    position = position_dodge(
      width = 0.35
    )
  ) +
  labs(
    title = "2019H2 RdC exposure: leave-one-zone-out robustness",
    subtitle = "Dashed/dotted horizontal lines are the corresponding full-sample estimates",
    x = "Excluded OMI zone",
    y = "RdC exposure coefficient",
    shape = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      hjust = 1,
      vjust = 0.5
    )
  )

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig20_leave_one_out_rdc_2019.png"
  ),
  fig20,
  width = 12,
  height = 6,
  dpi = 300
)

# Fig 21 - distance slope changes.
fig21_data <- distance_results |>
  filter(
    result_type == "Slope change"
  ) |>
  mutate(
    outcome_label = recode(
      outcome,
      log_rent = "Rent",
      log_sale = "Sale price"
    )
  )

fig21 <- ggplot(
  fig21_data,
  aes(
    x = evento,
    y = stima,
    shape = outcome_label
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  geom_errorbar(
    aes(
      ymin = ci95_low,
      ymax = ci95_high
    ),
    width = 0.12,
    position = position_dodge(
      width = 0.35
    )
  ) +
  geom_point(
    size = 2.3,
    position = position_dodge(
      width = 0.35
    )
  ) +
  labs(
    title = "Change in the distance gradient at the two main rental episodes",
    subtitle = "Positive = flatter distance slope; negative = steeper distance slope",
    x = NULL,
    y = "Change in log-value slope per km",
    shape = NULL
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig21_distance_gradient_event_changes.png"
  ),
  fig21,
  width = 8,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 14. README
# ------------------------------------------------------------------------------
readme <- c(
  "V10 - 05 FINAL ROBUSTNESS AND SPATIAL DIAGNOSTICS",
  "==================================================",
  "",
  "PURPOSE",
  "This script stress-tests results already fixed in 01-04.",
  "It does not redefine the RdC exposure or select specifications by significance.",
  "",
  "1. OMI ENDPOINTS",
  "2019H2 and 2023H2 are re-estimated using lower bound, midpoint and upper",
  "bound of the OMI rent/sale quotation interval. Initial-level-adjusted RdC",
  "models use the corresponding endpoint's average log level in 2018.",
  "",
  "2. DISTANCE ROBUSTNESS",
  "Formal slope-change tests compare 2019H1->2019H2 and 2023H1->2023H2.",
  "The interaction standard errors are clustered by OMI zone.",
  "",
  "3. 59 VS 60 ZONES",
  "The 2019 cross-section includes D32, which is unavailable in the long",
  "balanced Type20 panel after 2021. The same 2019H1-to-2019H2 specification",
  "is compared on 59 versus all 60 zones.",
  "",
  "4. LEAVE-ONE-OUT",
  "Every balanced zone is excluded once. No zone is selected for exclusion",
  "based on the observed 2019 outcome.",
  "",
  "5. BOUNDARY SENSITIVITY",
  "B13, B16, C31 and E45 are removed jointly because the earlier geography",
  "audit identified polygon symmetric differences above 10% across vintages.",
  "This set is pre-specified independently of the 2019 rent changes.",
  "",
  "6. SPATIAL RESIDUAL DIAGNOSTICS",
  "Moran's I uses the pre-built symmetric binary neighbor graph (131 unordered",
  "edges across all 59 balanced zones) and 9,999 permutations.",
  "Both greater-tail and two-sided pseudo-p values are reported.",
  "",
  "A significant Moran's I in raw changes does not by itself invalidate the",
  "model. The relevant diagnostic is whether spatial dependence remains in",
  "the residuals after the spatial structure already modeled by OMI bands",
  "and, where relevant, pre-policy initial rent.",
  "",
  "This script does not impose a Conley cutoff ex post. If residual Moran",
  "remains materially significant, spatial-HAC inference should be considered",
  "as an additional explicitly motivated extension.",
  "",
  "7. WITHIN-BAND PERMUTATION",
  "RdC exposure labels are permuted within B/C/D/E only. This checks whether",
  "the observed within-band coefficient is unusual relative to reassignment",
  "within the same broad OMI geography. It is NOT a causal randomization test.",
  "",
  "INTERPRETATION",
  "Robustness should be judged by stability of signs/magnitudes and by whether",
  "the already identified main limitation (initial-rent confounding) persists.",
  "A robustness result must not be used to replace the frozen main model solely",
  "because it gives a smaller p-value."
)

writeLines(
  readme,
  file.path(
    OUTPUT_DIR,
    "README_05_ROBUSTNESS_SPATIAL.txt"
  ),
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 15. Console summary
# ------------------------------------------------------------------------------
moran_raw <- moran_results |>
  filter(
    diagnostic ==
      "2019H2 rent change - raw"
  )

moran_band <- moran_results |>
  filter(
    diagnostic ==
      "2019H2 rent residual - fascia"
  )

moran_base <- moran_results |>
  filter(
    diagnostic ==
      paste0(
        "2019H2 rent residual - fascia + ",
        "average 2018 rent"
      )
  )

message("")
message("==============================================================")
message("05_robustness_spatial.R COMPLETATO")
message("==============================================================")
message(
  "Master canonico: ",
  master_input_nrow, " x ", master_input_ncol
)
message(
  "Neighbor graph: ",
  length(zones59), " zone | ",
  nrow(neighbors), " unordered edges | connected = ",
  all(visited)
)
message("")
message(
  "2019 RdC M1 endpoints (lower/mid/upper): ",
  sprintf("%.5f", ep19_min_rdc$stima), " / ",
  sprintf("%.5f", ep19_mid_rdc$stima), " / ",
  sprintf("%.5f", ep19_max_rdc$stima)
)
message(
  "2019 RdC adjusted endpoints (lower/mid/upper): ",
  sprintf("%.5f", ep19_min_adj$stima), " / ",
  sprintf("%.5f", ep19_mid_adj$stima), " / ",
  sprintf("%.5f", ep19_max_adj$stima)
)
message("")
message(
  "2019 distance-slope change rent: ",
  sprintf("%.6f", dist19_r$stima),
  " | p = ", sprintf("%.6f", dist19_r$p_value)
)
message(
  "2019 distance-slope change sale: ",
  sprintf("%.6f", dist19_s$stima),
  " | p = ", sprintf("%.6f", dist19_s$p_value)
)
message(
  "2023 distance-slope change rent: ",
  sprintf("%.6f", dist23_r$stima),
  " | p = ", sprintf("%.6f", dist23_r$p_value)
)
message("")
message(
  "2019 RdC M1 59 zones: ",
  sprintf("%.6f", s59$stima),
  " | 60 zones: ",
  sprintf("%.6f", s60$stima)
)
message("")
message(
  "2019 Moran raw: I = ",
  sprintf("%.4f", moran_raw$moran_I),
  " | p(two-sided) = ",
  sprintf("%.4f", moran_raw$p_two_sided)
)
message(
  "2019 Moran after fascia: I = ",
  sprintf("%.4f", moran_band$moran_I),
  " | p(two-sided) = ",
  sprintf("%.4f", moran_band$p_two_sided)
)
message(
  "2019 Moran after fascia + initial rent: I = ",
  sprintf("%.4f", moran_base$moran_I),
  " | p(two-sided) = ",
  sprintf("%.4f", moran_base$p_two_sided)
)
message("")
message(
  "Within-band permutation M1 p = ",
  sprintf(
    "%.4f",
    permutation_results$permutation_p_two_sided[1]
  ),
  " | M2 p = ",
  sprintf(
    "%.4f",
    permutation_results$permutation_p_two_sided[2]
  )
)
message("")
message(
  "Output: ",
  normalizePath(
    OUTPUT_DIR,
    winslash = "/",
    mustWork = TRUE
  )
)
message(
  "05 chiude le robustness pre-specificate; ",
  "non selezionare specifiche alternative in base al p-value."
)
