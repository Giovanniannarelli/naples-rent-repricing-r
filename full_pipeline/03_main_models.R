# ==============================================================================
# 03_main_models.R
# V10 - MAIN ECONOMETRIC MODELS
# Naples residential rent gradient + pre-policy RdC exposure, 2016-2024
# ==============================================================================
#
# STRATEGIA EMPIRICA PRE-SPECIFICATA
# ----------------------------------
# A) Main urban result:
#    documentare formalmente flattening 2019H2 e re-steepening 2023H2 del
#    gradiente locativo, con benchmark sui prezzi di vendita.
#
# B) Main RdC specification:
#    outcome = first difference of log OMI residential rent quotation.
#    Per ogni semestre stimiamo il coefficiente della RdC exposure (1 SD)
#    all'interno delle fasce OMI, assorbendo fascia x semestre.
#
#    Δlog(Rent_z,t) = FE_(fascia x semestre)
#                     + beta_t * RdCExposure_z
#                     + error_z,t
#
#    La forma pooled viene stimata con un'interazione distinta exposure x
#    semestre e SE cluster per zona.
#
#    NOTA: non è un "event study causale" già identificato. È una dynamic
#    exposure specification. Il test pre-2019 verifica se le zone con diversa
#    exposure presentavano slopes differenziali prima dell'introduzione RdC.
#
# C) Confondimento chiave:
#    RdCExposure è fortemente correlata al livello iniziale dei canoni.
#    Per il 2019H2 stimiamo quindi, ex ante:
#       M1 fascia + RdC exposure
#       M2 + average log rent 2018
#       M3 + average log rent 2018 + quadratico
#       M4 + quintili average log rent 2018
#       M5 + log rent 2019H1 (robustezza più prossima allo shock)
#
#    L'average 2018 è il controllo principale per initial-rent confounding:
#    è interamente pre-policy e non condivide il termine log(Rent_2019H1) che
#    entra meccanicamente in Δlog(Rent_2019H2).
#
# D) Placebo / benchmark:
#    - Δlog sale prices
#    - Δlog rent-sale valuation gap
#    - variazione assoluta del canone OMI (euro/mq/mese)
#
# E) 2023H2:
#    si documenta il re-steepening e si verifica l'associazione con la stessa
#    RdC exposure. NON viene definito causalmente come phase-out experiment.
#
# F) Robustezza definizione exposure:
#    2-component, Anderson, PCA1, NEET; sono pre-costruite nel 02 e NON
#    sostituiscono il main in base alla significatività.
#
# INPUT CANONICO
# ---------------
# output_v10/master_naples_housing_panel_rdc.csv   (1003 x 78)
#
# OUTPUT (output_v10/)
# --------------------
# 20_main_model_audit.csv
# 21_spatial_gradient_levels.csv
# 22_spatial_gradient_changes.csv
# 23_rdc_dynamic_main.csv
# 23b_rdc_dynamic_pretrend_wald.csv
# 24_rdc_2019_initial_rent_models.csv
# 24b_rdc_2019_initial_rent_full_coefficients.csv
# 25_rdc_2019_placebo_models.csv
# 26_rdc_2019_absolute_change_models.csv
# 27_rdc_2023_reversal_models.csv
# 28_rdc_alternative_indices_2019.csv
# 29_rdc_initial_rent_correlations.csv
# 30_main_model_summary.csv
# fig11_rdc_dynamic_rent.png
# fig12_rdc_dynamic_rent_sale.png
# fig13_rdc_2019_initial_rent_sensitivity.png
# fig14_rent_gradient_levels_main.png
# README_03_MAIN_MODELS.txt
#
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Pacchetti
# ------------------------------------------------------------------------------
required_packages <- c(
  "dplyr", "tidyr", "stringr", "ggplot2", "fixest", "sandwich"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Pacchetti R mancanti: ", paste(missing_packages, collapse = ", "),
    "\nInstallarli una sola volta con:\ninstall.packages(c(",
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
  library(fixest)
  library(sandwich)
})

# ------------------------------------------------------------------------------
# 1. Percorsi robusti
# ------------------------------------------------------------------------------
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

MASTER_PATH <- file.path(OUTPUT_DIR, "master_naples_housing_panel_rdc.csv")

assert_03 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

assert_03(
  file.exists(MASTER_PATH),
  paste0(
    "Manca il master canonico: ", MASTER_PATH,
    "\nEseguire prima 00_build_data.R e 02_rdc_exposure_build.R."
  )
)

write_csv_03 <- function(x, filename) {
  utils::write.csv(
    x,
    file.path(OUTPUT_DIR, filename),
    row.names = FALSE,
    na = ""
  )
}

# ------------------------------------------------------------------------------
# 2. Helper statistici
# ------------------------------------------------------------------------------

# HC3 cross-section: usa df residui t, non normal approximation.
tidy_lm_hc3 <- function(fit, model_name, outcome_name, specification) {
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- stats::coef(fit)
  se <- sqrt(diag(V))

  keep <- is.finite(b) & is.finite(se)
  b <- b[keep]
  se <- se[keep]

  df_r <- stats::df.residual(fit)
  tval <- b / se
  pval <- 2 * stats::pt(abs(tval), df = df_r, lower.tail = FALSE)
  crit <- stats::qt(0.975, df = df_r)

  out <- data.frame(
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
  rownames(out) <- NULL
  out
}

extract_term_hc3 <- function(
  fit, term, model_name, outcome_name, specification
) {
  tab <- tidy_lm_hc3(fit, model_name, outcome_name, specification)
  hit <- tab |> filter(termine == term)
  assert_03(
    nrow(hit) == 1L,
    paste0("Termine ", term, " non trovato univocamente in ", model_name)
  )
  hit
}

# Fit cross-section per semestre per ottenere gradienti B/C/D/E con HC3.
fit_band_by_semester <- function(data, outcome, sample_label) {
  sem_map <- data |>
    distinct(semestre, period_index) |>
    arrange(period_index)

  pieces <- lapply(seq_len(nrow(sem_map)), function(i) {
    s <- sem_map$semestre[i]
    pi <- sem_map$period_index[i]
    d <- data |> filter(semestre == s)

    assert_03(
      n_distinct(d$zona) == nrow(d),
      paste0("Duplicati zona nel cross-section ", s, " per ", outcome)
    )

    d$fascia <- factor(d$fascia, levels = c("B", "C", "D", "E"))
    fit <- stats::lm(
      stats::as.formula(paste0(outcome, " ~ fascia")),
      data = d
    )
    tab <- tidy_lm_hc3(
      fit,
      model_name = paste0(sample_label, "_", s),
      outcome_name = outcome,
      specification = "fascia; B reference"
    )

    band_rows <- bind_rows(
      data.frame(
        semestre = s,
        period_index = pi,
        outcome = outcome,
        fascia = "B",
        differenziale_vs_B = 0,
        errore_standard = NA_real_,
        p_value = NA_real_,
        ci95_low = 0,
        ci95_high = 0,
        n = nrow(d),
        stringsAsFactors = FALSE
      ),
      tab |>
        filter(termine %in% c("fasciaC", "fasciaD", "fasciaE")) |>
        transmute(
          semestre = s,
          period_index = pi,
          outcome = outcome,
          fascia = sub("fascia", "", termine),
          differenziale_vs_B = stima,
          errore_standard = errore_standard,
          p_value = p_value,
          ci95_low = ci95_low,
          ci95_high = ci95_high,
          n = n
        )
    )
    band_rows
  })

  bind_rows(pieces) |>
    arrange(period_index, fascia)
}

# Dynamic exposure model:
# una colonna Exposure x Semester per ogni semestre, con FE fascia-semestre.
# Il coefficiente beta_t è quindi la slope within-fascia in QUEL semestre,
# non una differenza rispetto a un semestre base.
fit_dynamic_exposure <- function(
  data,
  outcome,
  exposure_var = "rdc_exposure_main",
  aligned_semesters = NULL
) {
  d <- data

  if (!is.null(aligned_semesters)) {
    d <- d |> filter(semestre %in% aligned_semesters)
  }

  sem_map <- d |>
    distinct(semestre, period_index) |>
    arrange(period_index)

  assert_03(
    nrow(sem_map) >= 2L,
    paste0("Troppi pochi semestri nel dynamic model di ", outcome)
  )

  # FE fascia x semestre: cella comune per tutte le zone della stessa fascia
  # nello stesso semestre.
  d <- d |>
    mutate(
      fascia = factor(fascia, levels = c("B", "C", "D", "E")),
      fascia_semestre = interaction(
        fascia, semestre, drop = TRUE, lex.order = TRUE
      )
    )

  term_map <- data.frame(
    semestre = sem_map$semestre,
    period_index = sem_map$period_index,
    term = paste0("rdc__", gsub("-", "_", sem_map$semestre)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(term_map))) {
    s <- term_map$semestre[i]
    nm <- term_map$term[i]
    d[[nm]] <- ifelse(
      d$semestre == s,
      d[[exposure_var]],
      0
    )
  }

  rhs <- paste(term_map$term, collapse = " + ")
  fml <- stats::as.formula(
    paste0(outcome, " ~ ", rhs, " | fascia_semestre")
  )

  fit <- fixest::feols(
    fml,
    data = d,
    cluster = ~zona,
    notes = FALSE,
    warn = TRUE
  )

  b <- stats::coef(fit)
  V <- stats::vcov(fit)
  G <- dplyr::n_distinct(d$zona)
  df_cluster <- G - 1L
  crit <- stats::qt(0.975, df = df_cluster)

  rows <- lapply(seq_len(nrow(term_map)), function(i) {
    nm <- term_map$term[i]
    assert_03(
      nm %in% names(b),
      paste0("Coefficiente dynamic mancante: ", nm, " in ", outcome)
    )
    est <- unname(b[nm])
    se <- sqrt(V[nm, nm])
    tval <- est / se
    p <- 2 * stats::pt(abs(tval), df = df_cluster, lower.tail = FALSE)

    data.frame(
      outcome = outcome,
      exposure = exposure_var,
      semestre = term_map$semestre[i],
      period_index = term_map$period_index[i],
      stima = est,
      errore_standard_cluster = se,
      statistica_t = tval,
      p_value = p,
      ci95_low = est - crit * se,
      ci95_high = est + crit * se,
      n = stats::nobs(fit),
      n_cluster = G,
      df_cluster = df_cluster,
      specifica = "FE fascia x semestre; SE cluster zona",
      stringsAsFactors = FALSE
    )
  })

  list(
    fit = fit,
    results = bind_rows(rows),
    term_map = term_map,
    data = d
  )
}

# Wald joint test con covariance matrix già cluster-robust del fixest model.
wald_cluster_terms <- function(
  fit_object,
  terms,
  G,
  label,
  null_text
) {
  b_all <- stats::coef(fit_object)
  V_all <- stats::vcov(fit_object)

  assert_03(
    all(terms %in% names(b_all)),
    paste0("Termini mancanti nel Wald: ", label)
  )

  b <- as.numeric(b_all[terms])
  V <- V_all[terms, terms, drop = FALSE]

  q <- length(terms)
  invV <- tryCatch(
    qr.solve(V),
    error = function(e) NULL
  )
  assert_03(!is.null(invV), paste0("Matrice Wald singolare: ", label))

  W <- as.numeric(t(b) %*% invV %*% b)
  Fstat <- W / q
  df_den <- G - 1L
  p <- stats::pf(Fstat, df1 = q, df2 = df_den, lower.tail = FALSE)

  data.frame(
    test = label,
    null = null_text,
    wald_chi2 = W,
    f_stat = Fstat,
    df_num = q,
    df_den = df_den,
    p_value = p,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 3. Leggi master canonico e AUDIT prima di qualunque regressione
# ------------------------------------------------------------------------------
master <- utils::read.csv(
  MASTER_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

# Dimensioni del file CANONICO appena letto, prima di creare variabili derivate
# esclusivamente in memoria nel 03.
master_input_nrow <- nrow(master)
master_input_ncol <- ncol(master)

required_cols <- c(
  "zona", "fascia", "anno", "semestre_num", "semestre", "period_index",
  "loc_medio", "compr_medio", "log_rent", "log_sale",
  "log_rent_sale_gap",
  "d_log_rent", "d_log_sale", "d_log_rent_sale_gap",
  "valid_rent_difference", "valid_sale_difference", "valid_gap_difference",
  "sample_levels_2018plus",
  "superficie_loc", "superficie_compr",
  "indice_pressione_abitativa_2011",
  "renter_share_2011", "low_employment_2011",
  "economic_distress_2011", "neet_v8_2011",
  "rdc_exposure_main", "rdc_exposure_2comp",
  "rdc_exposure_anderson", "rdc_exposure_pca1", "rdc_exposure_neet"
)

assert_03(
  all(required_cols %in% names(master)),
  paste0(
    "Master RdC incompleto. Mancano: ",
    paste(setdiff(required_cols, names(master)), collapse = ", ")
  )
)

assert_03(
  master_input_nrow == 1003L && master_input_ncol == 78L,
  paste0(
    "Dimensione master canonico inattesa: ",
    master_input_nrow, " x ", master_input_ncol,
    "; atteso 1003 x 78."
  )
)

master <- master |>
  mutate(
    zona = as.character(zona),
    fascia = factor(as.character(fascia), levels = c("B", "C", "D", "E")),
    semestre = as.character(semestre)
  ) |>
  arrange(zona, period_index)

assert_03(n_distinct(master$zona) == 59L, "Attese 59 zone nel master.")
assert_03(
  all(table(master$zona) == 17L),
  "Il master non è 59 x 17 perfettamente bilanciato."
)
assert_03(
  sum(master$valid_rent_difference == 1) == 885L,
  "Attese 885 rent first differences valide."
)
assert_03(
  sum(master$valid_sale_difference == 1) == 944L,
  "Attese 944 sale first differences valide."
)
assert_03(
  sum(master$valid_gap_difference == 1) == 885L,
  "Attese 885 gap first differences valide."
)

# Exposure deve essere time invariant per zona.
exposure_check <- master |>
  group_by(zona) |>
  summarise(
    n_main = n_distinct(rdc_exposure_main),
    n_alt2 = n_distinct(rdc_exposure_2comp),
    n_anderson = n_distinct(rdc_exposure_anderson),
    n_pca = n_distinct(rdc_exposure_pca1),
    n_neet = n_distinct(rdc_exposure_neet),
    .groups = "drop"
  )

assert_03(
  all(exposure_check$n_main == 1L) &&
    all(exposure_check$n_alt2 == 1L) &&
    all(exposure_check$n_anderson == 1L) &&
    all(exposure_check$n_pca == 1L) &&
    all(exposure_check$n_neet == 1L),
  "Una exposure RdC varia nel tempo all'interno della zona: errore."
)

zone_exposure <- master |>
  distinct(
    zona, fascia,
    rdc_exposure_main,
    rdc_exposure_2comp,
    rdc_exposure_anderson,
    rdc_exposure_pca1,
    rdc_exposure_neet,
    indice_pressione_abitativa_2011
  )

assert_03(nrow(zone_exposure) == 59L, "Exposure table non ha 59 zone.")
assert_03(
  abs(mean(zone_exposure$rdc_exposure_main)) < 1e-8,
  "RdC exposure main non ha media circa zero."
)
assert_03(
  abs(stats::sd(zone_exposure$rdc_exposure_main) - 1) < 1e-8,
  "RdC exposure main non ha SD circa 1."
)

# Costruiamo variazioni assolute senza toccare i first differences originali.
master <- master |>
  group_by(zona) |>
  arrange(period_index, .by_group = TRUE) |>
  mutate(
    d_rent_abs = loc_medio - lag(loc_medio),
    d_sale_abs = compr_medio - lag(compr_medio)
  ) |>
  ungroup()

message("")
message("03 - INPUT AUDIT PRE-MODELLI")
message("---------------------------")
message("Master canonico input: ", master_input_nrow, " x ", master_input_ncol)
message("Master di lavoro (dopo d_rent_abs e d_sale_abs): ",
        nrow(master), " x ", ncol(master))
message("Zone: ", n_distinct(master$zona), " | osservazioni/zone: ",
        paste(range(as.integer(table(master$zona))), collapse = "-"))
message("Rent FD valide: ", sum(master$valid_rent_difference == 1))
message("Sale FD valide: ", sum(master$valid_sale_difference == 1))
message("Gap FD valide: ", sum(master$valid_gap_difference == 1))
message("Exposure main time-invariant: ", all(exposure_check$n_main == 1L))
message("Exposure main mean: ", format(mean(zone_exposure$rdc_exposure_main), digits = 8))
message("Exposure main sd: ", format(sd(zone_exposure$rdc_exposure_main), digits = 8))
message("")

# ------------------------------------------------------------------------------
# 4. Sample principali
# ------------------------------------------------------------------------------
rent_fd <- master |>
  filter(valid_rent_difference == 1)

rent_semesters <- rent_fd |>
  distinct(semestre, period_index) |>
  arrange(period_index)

assert_03(nrow(rent_semesters) == 15L, "Attesi 15 semestri rent FD.")
assert_03(
  !"2018-1" %in% rent_semesters$semestre,
  "2018-1 NON deve entrare nel sample rent FD."
)
assert_03(
  all(table(rent_fd$semestre) == 59L),
  "Ogni rent-difference semester deve avere 59 zone."
)

# Per il benchmark sale usiamo gli stessi 15 periodi del rent main,
# così la comparazione temporale è perfettamente allineata.
sale_fd_aligned <- master |>
  filter(
    valid_sale_difference == 1,
    semestre %in% rent_semesters$semestre
  )

gap_fd <- master |>
  filter(
    valid_gap_difference == 1,
    semestre %in% rent_semesters$semestre
  )

assert_03(nrow(sale_fd_aligned) == 885L, "Sale aligned sample deve avere 885 righe.")
assert_03(nrow(gap_fd) == 885L, "Gap aligned sample deve avere 885 righe.")

# Levels 2018+ per gradienti comparabili e superficie locativa L.
levels_2018 <- master |>
  filter(sample_levels_2018plus == 1)

assert_03(
  all(levels_2018$superficie_loc == "L"),
  "Nel sample levels 2018+ la superficie locativa deve essere L."
)
assert_03(
  n_distinct(levels_2018$semestre) == 13L,
  "Attesi 13 semestri levels 2018H1-2024H1."
)

# ------------------------------------------------------------------------------
# 5. Baseline PRE-POLICY per il confondimento initial rent
# ------------------------------------------------------------------------------
baseline_2018 <- master |>
  filter(semestre %in% c("2018-1", "2018-2")) |>
  group_by(zona) |>
  summarise(
    baseline_log_rent_2018avg = mean(log_rent),
    baseline_log_sale_2018avg = mean(log_sale),
    baseline_log_gap_2018avg = mean(log_rent_sale_gap),
    n_baseline_2018 = n(),
    .groups = "drop"
  )

assert_03(
  nrow(baseline_2018) == 59L &&
    all(baseline_2018$n_baseline_2018 == 2L),
  "Baseline 2018 deve avere esattamente 2 osservazioni per 59 zone."
)

baseline_2019h1 <- master |>
  filter(semestre == "2019-1") |>
  transmute(
    zona,
    baseline_log_rent_2019h1 = log_rent,
    baseline_log_sale_2019h1 = log_sale,
    baseline_log_gap_2019h1 = log_rent_sale_gap
  )

assert_03(nrow(baseline_2019h1) == 59L, "Baseline 2019H1 deve avere 59 zone.")

baseline <- baseline_2018 |>
  left_join(baseline_2019h1, by = "zona") |>
  mutate(
    baseline_rent_2018_c =
      baseline_log_rent_2018avg - mean(baseline_log_rent_2018avg),
    baseline_sale_2018_c =
      baseline_log_sale_2018avg - mean(baseline_log_sale_2018avg),
    baseline_gap_2018_c =
      baseline_log_gap_2018avg - mean(baseline_log_gap_2018avg),
    baseline_rent_2019h1_c =
      baseline_log_rent_2019h1 - mean(baseline_log_rent_2019h1),
    baseline_sale_2019h1_c =
      baseline_log_sale_2019h1 - mean(baseline_log_sale_2019h1),
    baseline_rent_q5 = factor(
      dplyr::ntile(baseline_log_rent_2018avg, 5),
      levels = 1:5
    )
  )

# ------------------------------------------------------------------------------
# 6. MAIN URBAN RESULT: gradiente in livelli 2018+ e variazioni
# ------------------------------------------------------------------------------
gradient_levels_rent <- fit_band_by_semester(
  levels_2018, "log_rent", "levels_rent"
)
gradient_levels_sale <- fit_band_by_semester(
  levels_2018, "log_sale", "levels_sale"
)

gradient_levels <- bind_rows(
  gradient_levels_rent,
  gradient_levels_sale
) |>
  arrange(outcome, period_index, fascia)

write_csv_03(gradient_levels, "21_spatial_gradient_levels.csv")

gradient_changes_rent <- fit_band_by_semester(
  rent_fd, "d_log_rent", "changes_rent"
)
gradient_changes_sale <- fit_band_by_semester(
  sale_fd_aligned, "d_log_sale", "changes_sale"
)

gradient_changes <- bind_rows(
  gradient_changes_rent,
  gradient_changes_sale
) |>
  arrange(outcome, period_index, fascia)

write_csv_03(gradient_changes, "22_spatial_gradient_changes.csv")

# ------------------------------------------------------------------------------
# 7. MAIN RdC DYNAMIC EXPOSURE MODEL
# ------------------------------------------------------------------------------
dyn_rent <- fit_dynamic_exposure(
  rent_fd,
  outcome = "d_log_rent",
  exposure_var = "rdc_exposure_main"
)

dyn_sale <- fit_dynamic_exposure(
  sale_fd_aligned,
  outcome = "d_log_sale",
  exposure_var = "rdc_exposure_main",
  aligned_semesters = rent_semesters$semestre
)

dyn_gap <- fit_dynamic_exposure(
  gap_fd,
  outcome = "d_log_rent_sale_gap",
  exposure_var = "rdc_exposure_main",
  aligned_semesters = rent_semesters$semestre
)

dynamic_all <- bind_rows(
  dyn_rent$results,
  dyn_sale$results,
  dyn_gap$results
) |>
  mutate(
    outcome_label = dplyr::recode(
      outcome,
      d_log_rent = "Rent",
      d_log_sale = "Sale price",
      d_log_rent_sale_gap = "Rent-sale gap"
    )
  ) |>
  arrange(outcome, period_index)

write_csv_03(dynamic_all, "23_rdc_dynamic_main.csv")

# Joint pre-2019H2 test: i 5 slopes precedenti devono essere zero.
pre_semesters <- rent_semesters |>
  filter(period_index < unique(
    master$period_index[master$semestre == "2019-2"]
  ))

assert_03(
  identical(
    pre_semesters$semestre,
    c("2016-2", "2017-1", "2017-2", "2018-2", "2019-1")
  ),
  "Set pre-2019H2 inatteso."
)

pre_terms <- paste0("rdc__", gsub("-", "_", pre_semesters$semestre))

wald_pre_rent <- wald_cluster_terms(
  dyn_rent$fit,
  terms = pre_terms,
  G = n_distinct(rent_fd$zona),
  label = "RdC exposure pre-2019H2 - rent",
  null_text = "all pre-2019H2 within-band RdC-exposure slopes equal zero"
)

wald_pre_sale <- wald_cluster_terms(
  dyn_sale$fit,
  terms = pre_terms,
  G = n_distinct(sale_fd_aligned$zona),
  label = "RdC exposure pre-2019H2 - sale",
  null_text = "all pre-2019H2 within-band RdC-exposure slopes equal zero"
)

wald_pre_gap <- wald_cluster_terms(
  dyn_gap$fit,
  terms = pre_terms,
  G = n_distinct(gap_fd$zona),
  label = "RdC exposure pre-2019H2 - rent-sale gap",
  null_text = "all pre-2019H2 within-band RdC-exposure slopes equal zero"
)

pretrend_wald <- bind_rows(
  wald_pre_rent,
  wald_pre_sale,
  wald_pre_gap
)

write_csv_03(pretrend_wald, "23b_rdc_dynamic_pretrend_wald.csv")

# ------------------------------------------------------------------------------
# 8. 2019H2: CONFONDIMENTO INITIAL RENT - test decisivo
# ------------------------------------------------------------------------------
cs2019 <- master |>
  filter(
    semestre == "2019-2",
    valid_rent_difference == 1
  ) |>
  left_join(baseline, by = "zona") |>
  mutate(
    fascia = factor(fascia, levels = c("B", "C", "D", "E"))
  )

assert_03(nrow(cs2019) == 59L, "Cross-section 2019H2 deve avere 59 zone.")
assert_03(!anyNA(cs2019$baseline_log_rent_2018avg), "Baseline rent 2018 mancante.")

fit_2019_m1 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main,
  data = cs2019
)

fit_2019_m2 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main + baseline_rent_2018_c,
  data = cs2019
)

fit_2019_m3 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main +
    baseline_rent_2018_c + I(baseline_rent_2018_c^2),
  data = cs2019
)

fit_2019_m4 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main + baseline_rent_q5,
  data = cs2019
)

fit_2019_m5 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main + baseline_rent_2019h1_c,
  data = cs2019
)

models_2019 <- list(
  M1 = list(
    fit = fit_2019_m1,
    spec = "fascia + RdC exposure"
  ),
  M2 = list(
    fit = fit_2019_m2,
    spec = "M1 + average log rent 2018"
  ),
  M3 = list(
    fit = fit_2019_m3,
    spec = "M2 + squared average log rent 2018"
  ),
  M4 = list(
    fit = fit_2019_m4,
    spec = "M1 + quintiles of average log rent 2018"
  ),
  M5 = list(
    fit = fit_2019_m5,
    spec = "M1 + log rent 2019H1"
  )
)

initial_rent_main <- bind_rows(lapply(
  names(models_2019),
  function(nm) {
    extract_term_hc3(
      models_2019[[nm]]$fit,
      term = "rdc_exposure_main",
      model_name = nm,
      outcome_name = "d_log_rent_2019H2",
      specification = models_2019[[nm]]$spec
    )
  }
))

initial_rent_full <- bind_rows(lapply(
  names(models_2019),
  function(nm) {
    tidy_lm_hc3(
      models_2019[[nm]]$fit,
      model_name = nm,
      outcome_name = "d_log_rent_2019H2",
      specification = models_2019[[nm]]$spec
    )
  }
))

write_csv_03(
  initial_rent_main,
  "24_rdc_2019_initial_rent_models.csv"
)
write_csv_03(
  initial_rent_full,
  "24b_rdc_2019_initial_rent_full_coefficients.csv"
)

# ------------------------------------------------------------------------------
# 9. 2019H2 PLACEBOS / BENCHMARKS
# ------------------------------------------------------------------------------
# Sale-price placebo.
fit_sale_2019_m1 <- lm(
  d_log_sale ~ fascia + rdc_exposure_main,
  data = cs2019
)
fit_sale_2019_m2 <- lm(
  d_log_sale ~ fascia + rdc_exposure_main + baseline_sale_2018_c,
  data = cs2019
)
fit_sale_2019_m3 <- lm(
  d_log_sale ~ fascia + rdc_exposure_main +
    baseline_sale_2018_c + I(baseline_sale_2018_c^2),
  data = cs2019
)

# Rent-sale valuation gap.
fit_gap_2019_m1 <- lm(
  d_log_rent_sale_gap ~ fascia + rdc_exposure_main,
  data = cs2019
)
fit_gap_2019_m2 <- lm(
  d_log_rent_sale_gap ~ fascia + rdc_exposure_main + baseline_gap_2018_c,
  data = cs2019
)

placebo_models <- list(
  SALE_M1 = list(
    fit = fit_sale_2019_m1,
    outcome = "d_log_sale_2019H2",
    spec = "fascia + RdC exposure"
  ),
  SALE_M2 = list(
    fit = fit_sale_2019_m2,
    outcome = "d_log_sale_2019H2",
    spec = "M1 + average log sale price 2018"
  ),
  SALE_M3 = list(
    fit = fit_sale_2019_m3,
    outcome = "d_log_sale_2019H2",
    spec = "M2 + squared average log sale price 2018"
  ),
  GAP_M1 = list(
    fit = fit_gap_2019_m1,
    outcome = "d_log_gap_2019H2",
    spec = "fascia + RdC exposure"
  ),
  GAP_M2 = list(
    fit = fit_gap_2019_m2,
    outcome = "d_log_gap_2019H2",
    spec = "M1 + average log rent-sale gap 2018"
  )
)

placebo_results <- bind_rows(lapply(
  names(placebo_models),
  function(nm) {
    extract_term_hc3(
      placebo_models[[nm]]$fit,
      term = "rdc_exposure_main",
      model_name = nm,
      outcome_name = placebo_models[[nm]]$outcome,
      specification = placebo_models[[nm]]$spec
    )
  }
))

write_csv_03(placebo_results, "25_rdc_2019_placebo_models.csv")

# ------------------------------------------------------------------------------
# 10. Variazione ASSOLUTA canone 2019H2
# ------------------------------------------------------------------------------
fit_abs_2019_m1 <- lm(
  d_rent_abs ~ fascia + rdc_exposure_main,
  data = cs2019
)

fit_abs_2019_m2 <- lm(
  d_rent_abs ~ fascia + rdc_exposure_main + baseline_rent_2018_c,
  data = cs2019
)

fit_abs_2019_m3 <- lm(
  d_rent_abs ~ fascia + rdc_exposure_main +
    baseline_rent_2018_c + I(baseline_rent_2018_c^2),
  data = cs2019
)

absolute_models <- list(
  ABS_M1 = list(
    fit = fit_abs_2019_m1,
    spec = "fascia + RdC exposure"
  ),
  ABS_M2 = list(
    fit = fit_abs_2019_m2,
    spec = "M1 + average log rent 2018"
  ),
  ABS_M3 = list(
    fit = fit_abs_2019_m3,
    spec = "M2 + squared average log rent 2018"
  )
)

absolute_results <- bind_rows(lapply(
  names(absolute_models),
  function(nm) {
    extract_term_hc3(
      absolute_models[[nm]]$fit,
      term = "rdc_exposure_main",
      model_name = nm,
      outcome_name = "absolute_rent_change_2019H2_euro_mq_month",
      specification = absolute_models[[nm]]$spec
    )
  }
))

write_csv_03(
  absolute_results,
  "26_rdc_2019_absolute_change_models.csv"
)

# ------------------------------------------------------------------------------
# 11. 2023H2 RE-STEEPENING: associazione con exposure (NON causal label)
# ------------------------------------------------------------------------------
cs2023 <- master |>
  filter(
    semestre == "2023-2",
    valid_rent_difference == 1
  ) |>
  left_join(baseline, by = "zona") |>
  mutate(
    fascia = factor(fascia, levels = c("B", "C", "D", "E"))
  )

assert_03(nrow(cs2023) == 59L, "Cross-section 2023H2 deve avere 59 zone.")

fit_2023_rent_m1 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main,
  data = cs2023
)
fit_2023_rent_m2 <- lm(
  d_log_rent ~ fascia + rdc_exposure_main + baseline_rent_2018_c,
  data = cs2023
)
fit_2023_sale_m1 <- lm(
  d_log_sale ~ fascia + rdc_exposure_main,
  data = cs2023
)

reversal_models <- list(
  RENT_2023_M1 = list(
    fit = fit_2023_rent_m1,
    outcome = "d_log_rent_2023H2",
    spec = "fascia + RdC exposure"
  ),
  RENT_2023_M2 = list(
    fit = fit_2023_rent_m2,
    outcome = "d_log_rent_2023H2",
    spec = "M1 + average log rent 2018"
  ),
  SALE_2023_M1 = list(
    fit = fit_2023_sale_m1,
    outcome = "d_log_sale_2023H2",
    spec = "fascia + RdC exposure"
  )
)

reversal_results <- bind_rows(lapply(
  names(reversal_models),
  function(nm) {
    extract_term_hc3(
      reversal_models[[nm]]$fit,
      term = "rdc_exposure_main",
      model_name = nm,
      outcome_name = reversal_models[[nm]]$outcome,
      specification = reversal_models[[nm]]$spec
    )
  }
))

write_csv_03(
  reversal_results,
  "27_rdc_2023_reversal_models.csv"
)

# ------------------------------------------------------------------------------
# 12. Robustezza alla DEFINIZIONE dell'exposure - 2019H2
# ------------------------------------------------------------------------------
exposure_vars <- c(
  rdc_exposure_main = "Main equal-weight",
  rdc_exposure_2comp = "Renter + low employment",
  rdc_exposure_anderson = "Anderson",
  rdc_exposure_pca1 = "PCA1",
  rdc_exposure_neet = "NEET alternative"
)

alt_index_rows <- list()
k <- 1L

for (v in names(exposure_vars)) {
  f1 <- lm(
    stats::as.formula(
      paste0("d_log_rent ~ fascia + ", v)
    ),
    data = cs2019
  )
  f2 <- lm(
    stats::as.formula(
      paste0(
        "d_log_rent ~ fascia + ", v,
        " + baseline_rent_2018_c"
      )
    ),
    data = cs2019
  )

  r1 <- extract_term_hc3(
    f1, v,
    model_name = paste0(v, "_M1"),
    outcome_name = "d_log_rent_2019H2",
    specification = "fascia + exposure"
  )
  r2 <- extract_term_hc3(
    f2, v,
    model_name = paste0(v, "_M2"),
    outcome_name = "d_log_rent_2019H2",
    specification = "fascia + exposure + average log rent 2018"
  )

  r1$indice <- exposure_vars[[v]]
  r1$baseline_adjusted <- FALSE
  r2$indice <- exposure_vars[[v]]
  r2$baseline_adjusted <- TRUE

  alt_index_rows[[k]] <- r1
  alt_index_rows[[k + 1L]] <- r2
  k <- k + 2L
}

alt_index_results <- bind_rows(alt_index_rows)

write_csv_03(
  alt_index_results,
  "28_rdc_alternative_indices_2019.csv"
)

# ------------------------------------------------------------------------------
# 13. Diagnostica RdC exposure vs initial rent / vecchio vulnerability index
# ------------------------------------------------------------------------------
diag_zone <- zone_exposure |>
  left_join(baseline, by = "zona") |>
  mutate(
    exposure_dm_band =
      rdc_exposure_main - ave(rdc_exposure_main, fascia, FUN = mean),
    baseline2018_dm_band =
      baseline_log_rent_2018avg -
      ave(baseline_log_rent_2018avg, fascia, FUN = mean),
    baseline2019h1_dm_band =
      baseline_log_rent_2019h1 -
      ave(baseline_log_rent_2019h1, fascia, FUN = mean)
  )

initial_corr <- data.frame(
  confronto = c(
    "RdC exposure vs avg log rent 2018",
    "RdC exposure vs log rent 2019H1",
    "RdC exposure vs avg log rent 2018 - within-band demeaned",
    "RdC exposure vs log rent 2019H1 - within-band demeaned",
    "RdC exposure vs old housing-pressure index"
  ),
  correlazione = c(
    cor(
      diag_zone$rdc_exposure_main,
      diag_zone$baseline_log_rent_2018avg
    ),
    cor(
      diag_zone$rdc_exposure_main,
      diag_zone$baseline_log_rent_2019h1
    ),
    cor(
      diag_zone$exposure_dm_band,
      diag_zone$baseline2018_dm_band
    ),
    cor(
      diag_zone$exposure_dm_band,
      diag_zone$baseline2019h1_dm_band
    ),
    cor(
      diag_zone$rdc_exposure_main,
      diag_zone$indice_pressione_abitativa_2011
    )
  ),
  n = 59L,
  stringsAsFactors = FALSE
)

write_csv_03(
  initial_corr,
  "29_rdc_initial_rent_correlations.csv"
)

# ------------------------------------------------------------------------------
# 14. Main model summary: solo numeri pre-definiti, nessuna selezione automatica
# ------------------------------------------------------------------------------
get_dynamic_row <- function(result_df, outcome_name, semester_name) {
  hit <- result_df |>
    filter(outcome == outcome_name, semestre == semester_name)
  assert_03(nrow(hit) == 1L, "Dynamic result non trovato univocamente.")
  hit
}

main_2019_dyn <- get_dynamic_row(dynamic_all, "d_log_rent", "2019-2")
sale_2019_dyn <- get_dynamic_row(dynamic_all, "d_log_sale", "2019-2")
gap_2019_dyn <- get_dynamic_row(dynamic_all, "d_log_rent_sale_gap", "2019-2")
rent_2023_dyn <- get_dynamic_row(dynamic_all, "d_log_rent", "2023-2")

m1_row <- initial_rent_main |> filter(modello == "M1")
m2_row <- initial_rent_main |> filter(modello == "M2")
m3_row <- initial_rent_main |> filter(modello == "M3")
m4_row <- initial_rent_main |> filter(modello == "M4")
m5_row <- initial_rent_main |> filter(modello == "M5")

summary_main <- bind_rows(
  data.frame(
    risultato = "RdC exposure dynamic slope - rent 2019H2",
    stima = main_2019_dyn$stima,
    errore_standard = main_2019_dyn$errore_standard_cluster,
    p_value = main_2019_dyn$p_value,
    interpretazione = "within-band log-rent growth per +1 SD pre-policy RdC exposure",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "Joint pre-2019H2 RdC exposure slopes - rent",
    stima = wald_pre_rent$f_stat,
    errore_standard = NA_real_,
    p_value = wald_pre_rent$p_value,
    interpretazione = "F test: all five pre-2019H2 slopes jointly zero",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "2019H2 rent exposure - M1",
    stima = m1_row$stima,
    errore_standard = m1_row$errore_standard,
    p_value = m1_row$p_value,
    interpretazione = "fascia + RdC exposure; HC3",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "2019H2 rent exposure - M2 initial-rent adjusted",
    stima = m2_row$stima,
    errore_standard = m2_row$errore_standard,
    p_value = m2_row$p_value,
    interpretazione = "M1 + average log rent 2018; HC3",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "2019H2 rent exposure - M3 nonlinear initial-rent adjusted",
    stima = m3_row$stima,
    errore_standard = m3_row$errore_standard,
    p_value = m3_row$p_value,
    interpretazione = "M2 + squared average log rent 2018; HC3",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "2019H2 rent exposure - M4 initial-rent quintiles",
    stima = m4_row$stima,
    errore_standard = m4_row$errore_standard,
    p_value = m4_row$p_value,
    interpretazione = "M1 + pre-policy 2018 rent quintiles; HC3",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "2019H2 rent exposure - M5 2019H1 rent adjusted",
    stima = m5_row$stima,
    errore_standard = m5_row$errore_standard,
    p_value = m5_row$p_value,
    interpretazione = "M1 + log rent 2019H1; robustness; HC3",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "RdC exposure dynamic slope - sale 2019H2",
    stima = sale_2019_dyn$stima,
    errore_standard = sale_2019_dyn$errore_standard_cluster,
    p_value = sale_2019_dyn$p_value,
    interpretazione = "sale-price placebo, same within-band dynamic specification",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "RdC exposure dynamic slope - rent-sale gap 2019H2",
    stima = gap_2019_dyn$stima,
    errore_standard = gap_2019_dyn$errore_standard_cluster,
    p_value = gap_2019_dyn$p_value,
    interpretazione = "rent-sale valuation gap, same dynamic specification",
    stringsAsFactors = FALSE
  ),
  data.frame(
    risultato = "RdC exposure dynamic slope - rent 2023H2",
    stima = rent_2023_dyn$stima,
    errore_standard = rent_2023_dyn$errore_standard_cluster,
    p_value = rent_2023_dyn$p_value,
    interpretazione = "contextual re-steepening check; NOT labelled causal phase-out",
    stringsAsFactors = FALSE
  )
)

write_csv_03(summary_main, "30_main_model_summary.csv")

# ------------------------------------------------------------------------------
# 15. AUDIT FORMALIZZATO
# ------------------------------------------------------------------------------
idx_2019h2 <- unique(master$period_index[master$semestre == "2019-2"])
idx_2023h2 <- unique(master$period_index[master$semestre == "2023-2"])

audit <- data.frame(
  controllo = c(
    "Master input rows",
    "Master input columns",
    "Master working columns after 2 derived changes",
    "Balanced zones",
    "Rows per zone",
    "Valid rent differences",
    "Valid sale differences",
    "Valid gap differences",
    "Rent FD semesters",
    "2018H1 excluded from rent FD",
    "Sale placebo aligned rows",
    "Gap aligned rows",
    "Levels 2018+ semesters",
    "Levels 2018+ rent surface all L",
    "Exposure time invariant",
    "Exposure mean on 59 zones",
    "Exposure sd on 59 zones",
    "Baseline 2018 zones",
    "Baseline 2018 obs per zone",
    "2019H2 cross-section",
    "2023H2 cross-section",
    "2019H2 period index",
    "2023H2 period index"
  ),
  valore = c(
    master_input_nrow,
    master_input_ncol,
    ncol(master),
    n_distinct(master$zona),
    paste(range(as.integer(table(master$zona))), collapse = "-"),
    sum(master$valid_rent_difference == 1),
    sum(master$valid_sale_difference == 1),
    sum(master$valid_gap_difference == 1),
    nrow(rent_semesters),
    !("2018-1" %in% rent_semesters$semestre),
    nrow(sale_fd_aligned),
    nrow(gap_fd),
    n_distinct(levels_2018$semestre),
    all(levels_2018$superficie_loc == "L"),
    all(exposure_check$n_main == 1L),
    mean(zone_exposure$rdc_exposure_main),
    sd(zone_exposure$rdc_exposure_main),
    nrow(baseline_2018),
    paste(range(baseline_2018$n_baseline_2018), collapse = "-"),
    nrow(cs2019),
    nrow(cs2023),
    idx_2019h2,
    idx_2023h2
  ),
  esito = c(
    ifelse(master_input_nrow == 1003L, "PASS", "FAIL"),
    ifelse(master_input_ncol == 78L, "PASS", "FAIL"),
    ifelse(ncol(master) == 80L, "PASS", "FAIL"),
    ifelse(n_distinct(master$zona) == 59L, "PASS", "FAIL"),
    ifelse(all(table(master$zona) == 17L), "PASS", "FAIL"),
    ifelse(sum(master$valid_rent_difference == 1) == 885L, "PASS", "FAIL"),
    ifelse(sum(master$valid_sale_difference == 1) == 944L, "PASS", "FAIL"),
    ifelse(sum(master$valid_gap_difference == 1) == 885L, "PASS", "FAIL"),
    ifelse(nrow(rent_semesters) == 15L, "PASS", "FAIL"),
    ifelse(!("2018-1" %in% rent_semesters$semestre), "PASS", "FAIL"),
    ifelse(nrow(sale_fd_aligned) == 885L, "PASS", "FAIL"),
    ifelse(nrow(gap_fd) == 885L, "PASS", "FAIL"),
    ifelse(n_distinct(levels_2018$semestre) == 13L, "PASS", "FAIL"),
    ifelse(all(levels_2018$superficie_loc == "L"), "PASS", "FAIL"),
    ifelse(all(exposure_check$n_main == 1L), "PASS", "FAIL"),
    ifelse(abs(mean(zone_exposure$rdc_exposure_main)) < 1e-8, "PASS", "FAIL"),
    ifelse(abs(sd(zone_exposure$rdc_exposure_main) - 1) < 1e-8, "PASS", "FAIL"),
    ifelse(nrow(baseline_2018) == 59L, "PASS", "FAIL"),
    ifelse(all(baseline_2018$n_baseline_2018 == 2L), "PASS", "FAIL"),
    ifelse(nrow(cs2019) == 59L, "PASS", "FAIL"),
    ifelse(nrow(cs2023) == 59L, "PASS", "FAIL"),
    "INFO",
    "INFO"
  ),
  stringsAsFactors = FALSE
)

# Scriviamo SEMPRE l'audit prima di fermarci: se un controllo fallisce,
# il file rimane disponibile per la diagnosi.
write_csv_03(audit, "20_main_model_audit.csv")

failed_audit <- audit |>
  filter(esito == "FAIL")

if (nrow(failed_audit) > 0L) {
  message("")
  message("==============================================================")
  message("AUDIT 03 - CONTROLLI FALLITI")
  message("==============================================================")
  for (i in seq_len(nrow(failed_audit))) {
    message(
      "- ", failed_audit$controllo[i],
      " | valore osservato = ", failed_audit$valore[i]
    )
  }
  message("")
  message(
    "Il file completo e' stato scritto in: ",
    file.path(OUTPUT_DIR, "20_main_model_audit.csv")
  )
  stop(
    paste0(
      "Audit 03 fallito in ", nrow(failed_audit),
      " controllo/i: ",
      paste(failed_audit$controllo, collapse = "; ")
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 16. FIGURE
# ------------------------------------------------------------------------------

# Figure 11: main dynamic RdC slope sui rents.
plot_rent_dyn <- dynamic_all |>
  filter(outcome == "d_log_rent") |>
  arrange(period_index)

fig11 <- ggplot(
  plot_rent_dyn,
  aes(x = period_index, y = stima)
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(
    xintercept = idx_2019h2 - 0.5,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = ci95_low, ymax = ci95_high),
    width = 0.12
  ) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = plot_rent_dyn$period_index,
    labels = plot_rent_dyn$semestre
  ) +
  labs(
    title = "Pre-policy RdC exposure and residential rent changes",
    subtitle = "Within-OMI-band slope by semester; 95% cluster-robust CI",
    x = NULL,
    y = "Coefficient per +1 SD RdC exposure",
    caption = "Dashed line marks the first OMI semester covering the introduction of RdC (2019H2)."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(OUTPUT_DIR, "fig11_rdc_dynamic_rent.png"),
  fig11,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 12: rent vs sale placebo, same 15 periods.
plot_rs <- dynamic_all |>
  filter(outcome %in% c("d_log_rent", "d_log_sale")) |>
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = c("Rent", "Sale price")
    )
  )

fig12 <- ggplot(
  plot_rs,
  aes(
    x = period_index,
    y = stima,
    linetype = outcome_label,
    shape = outcome_label,
    group = outcome_label
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_vline(
    xintercept = idx_2019h2 - 0.5,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = rent_semesters$period_index,
    labels = rent_semesters$semestre
  ) +
  labs(
    title = "RdC exposure: residential rents versus sale prices",
    subtitle = "Within-band dynamic slopes; sale prices are the housing-market benchmark",
    x = NULL,
    y = "Coefficient per +1 SD RdC exposure",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(OUTPUT_DIR, "fig12_rdc_dynamic_rent_sale.png"),
  fig12,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 13: sensibilità 2019 al controllo initial rent.
sens_plot <- initial_rent_main |>
  mutate(
    modello = factor(
      modello,
      levels = c("M1", "M2", "M3", "M4", "M5"),
      labels = c(
        "M1: band + exposure",
        "M2: + 2018 rent",
        "M3: + nonlinear 2018 rent",
        "M4: + 2018 rent quintiles",
        "M5: + 2019H1 rent"
      )
    )
  )

fig13 <- ggplot(
  sens_plot,
  aes(x = modello, y = stima)
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_errorbar(
    aes(ymin = ci95_low, ymax = ci95_high),
    width = 0.12
  ) +
  geom_point(size = 2) +
  labs(
    title = "2019H2 RdC-exposure coefficient and initial-rent adjustment",
    subtitle = "Outcome: change in log residential OMI rent quotation; HC3 confidence intervals",
    x = NULL,
    y = "Coefficient per +1 SD RdC exposure"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(
  file.path(OUTPUT_DIR, "fig13_rdc_2019_initial_rent_sensitivity.png"),
  fig13,
  width = 9,
  height = 6,
  dpi = 300
)

# Figure 14: livello del rent gradient, differenziali C/D/E vs B.
grad_plot <- gradient_levels |>
  filter(
    outcome == "log_rent",
    fascia %in% c("C", "D", "E")
  )

fig14 <- ggplot(
  grad_plot,
  aes(
    x = period_index,
    y = differenziale_vs_B,
    linetype = fascia,
    shape = fascia,
    group = fascia
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = sort(unique(grad_plot$period_index)),
    labels = grad_plot |>
      distinct(period_index, semestre) |>
      arrange(period_index) |>
      pull(semestre)
  ) +
  labs(
    title = "Naples residential rent gradient relative to OMI band B",
    subtitle = "Log-rent differentials; flattening means C/D/E move upward toward zero",
    x = NULL,
    y = "Log-rent differential versus band B",
    linetype = "OMI band",
    shape = "OMI band"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggsave(
  file.path(OUTPUT_DIR, "fig14_rent_gradient_levels_main.png"),
  fig14,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 17. README metodologico
# ------------------------------------------------------------------------------
readme <- c(
  "V10 - 03 MAIN MODELS",
  "====================",
  "",
  "STATUS",
  "Questo script NON modifica il dataset e NON ridefinisce la RdC exposure.",
  "La definizione main è quella congelata nel 02.",
  "",
  "MAIN DYNAMIC RDC SPECIFICATION",
  "Outcome: first difference of log OMI residential rent quotation.",
  "For each semester the model estimates one RdC-exposure slope while absorbing",
  "OMI-band x semester fixed effects. Standard errors are clustered by OMI zone.",
  "",
  "IMPORTANT INTERPRETATION",
  "The semester-specific coefficient is the within-band association between",
  "a +1 SD pre-policy RdC exposure and rent growth in that semester.",
  "It is NOT automatically a causal treatment effect.",
  "",
  "PRE-TREND",
  "The joint pre-2019H2 test evaluates whether all five pre-policy exposure",
  "slopes are jointly zero. Failure to reject is reassuring but is not proof",
  "of causal parallel trends.",
  "",
  "INITIAL-RENT CONFOUNDING",
  "The decisive alternative explanation is convergence from initially low rents.",
  "The primary adjustment uses average log rent in 2018, because it is fully",
  "pre-policy and does not share log(Rent_2019H1) with the 2019H2 first difference.",
  "2019H1 rent is kept only as an additional robustness check.",
  "",
  "MODEL HIERARCHY - 2019H2",
  "M1: fascia + RdC exposure.",
  "M2: M1 + average log rent 2018.",
  "M3: M2 + squared average log rent 2018.",
  "M4: M1 + quintiles of average log rent 2018.",
  "M5: M1 + log rent 2019H1.",
  "No model is selected automatically according to p-values.",
  "",
  "PLACEBOS / BENCHMARKS",
  "- sale-price first differences",
  "- rent-sale valuation-gap first differences",
  "- absolute euro/mq/month rent changes",
  "",
  "2023H2",
  "The script documents the re-steepening and reports the exposure association.",
  "It does NOT label 2023H2 as a causal RdC phase-out experiment.",
  "",
  "ROBUST EXPOSURE DEFINITIONS",
  "Main, 2-component, Anderson, PCA1 and NEET versions are estimated in parallel",
  "for 2019H2. The main index cannot be replaced based on which is significant.",
  "",
  "INFERENCE",
  "- Dynamic pooled models: SE clustered by zone (59 clusters).",
  "- 2019/2023 single-semester cross-sections: HC3 heteroskedasticity-robust SE.",
  "- Joint pre-trend Wald uses the cluster-robust covariance matrix and F(q, G-1).",
  "",
  "CAUSAL LANGUAGE",
  "Until the initial-rent confounding and measurement-validation evidence is",
  "jointly assessed, use 'evidence consistent with', 'association',",
  "'spatial exposure', or 'quasi-experimental pattern', not a definitive",
  "'causal effect of RdC on rents'."
)

writeLines(
  readme,
  con = file.path(OUTPUT_DIR, "README_03_MAIN_MODELS.txt"),
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 18. Console summary
# ------------------------------------------------------------------------------
message("")
message("==============================================================")
message("03_main_models.R COMPLETATO")
message("==============================================================")
message("Master canonico input: ", master_input_nrow, " x ", master_input_ncol)
message("Master di lavoro: ", nrow(master), " x ", ncol(master))
message("Zone: ", n_distinct(master$zona))
message("Rent FD sample: ", nrow(rent_fd), " (", nrow(rent_semesters), " semestri)")
message(
  "2019H2 dynamic RdC-rent slope: ",
  sprintf("%.6f", main_2019_dyn$stima),
  " | p = ", sprintf("%.6f", main_2019_dyn$p_value)
)
message(
  "Joint pre-2019H2 RdC-rent test p = ",
  sprintf("%.6f", wald_pre_rent$p_value)
)
message(
  "2019H2 M2 (adjusted for average 2018 rent): ",
  sprintf("%.6f", m2_row$stima),
  " | p = ", sprintf("%.6f", m2_row$p_value)
)
message(
  "2019H2 sale placebo dynamic slope: ",
  sprintf("%.6f", sale_2019_dyn$stima),
  " | p = ", sprintf("%.6f", sale_2019_dyn$p_value)
)
message(
  "2023H2 RdC-rent slope (contextual): ",
  sprintf("%.6f", rent_2023_dyn$stima),
  " | p = ", sprintf("%.6f", rent_2023_dyn$p_value)
)
message("Output: ", normalizePath(OUTPUT_DIR, winslash = "/", mustWork = TRUE))
message("Nessuna specifica è stata selezionata automaticamente in base al p-value.")
