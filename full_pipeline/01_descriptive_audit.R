# ==============================================================================
# V10 - NAPLES RENT REPRICING
# 01_descriptive_audit.R
# ==============================================================================
# Scopo:
#   verificare e caratterizzare il fatto empirico PRIMA dei modelli principali.
#
# Questo script:
#   1. legge SOLO il master creato da 00_build_data.R;
#   2. verifica 59 zone x 17 semestri, 2016-1 / 2024-1, 2024-2 escluso;
#   3. descrive il repricing 2019-2 e il catch-up 2023-2 in log, percentuali
#      e variazioni assolute;
#   4. misura dispersione e rank delle quotazioni tra zone;
#   5. stima il gradiente descrittivo rispetto alla distanza dal core B;
#   6. verifica il ruolo del livello iniziale 2019-1;
#   7. verifica le associazioni ISTAT 2011 con e senza controllo per livello
#      iniziale e fascia;
#   8. produce una diagnostica PRE-2019 in prime differenze, escludendo 2018-1;
#   9. rilegge i grezzi OMI SOLO per audit di tutte le tipologie nel 2019-2
#      e 2023-2.
#
# IMPORTANTE:
#   - nessun coefficiente prodotto qui e' interpretato causalmente;
#   - 2019-2 non e' definito come trattamento;
#   - i p-value dei modelli cross-section sono diagnostici; l'inferenza finale
#     sara' definita in 02_main_models.R;
#   - 2024-2 e' escluso per disegno.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "tidyr", "stringr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Pacchetti R mancanti: ", paste(missing_packages, collapse = ", "),
    "\nInstallarli con install.packages(c(",
    paste(sprintf("'%s'", missing_packages), collapse = ", "), ")).",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

# ------------------------------------------------------------------------------
# 0. Percorsi e funzioni di sicurezza
# ------------------------------------------------------------------------------
script_path_v10 <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(e) NA_character_
)
PROJECT_DIR <- if (!is.na(script_path_v10) && nzchar(script_path_v10)) {
  dirname(script_path_v10)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

INPUT_DIR <- file.path(PROJECT_DIR, "input")
PROCESSED_DIR <- file.path(PROJECT_DIR, "data_processed_v10")
OUTPUT_DIR <- file.path(PROJECT_DIR, "output_v10")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

MASTER_PATH <- file.path(OUTPUT_DIR, "master_naples_housing_panel.csv")

EXPECTED_SEMESTERS <- c(
  as.vector(rbind(paste0(2016:2023, "-1"), paste0(2016:2023, "-2"))),
  "2024-1"
)
EXPECTED_BANDS <- c("B", "C", "D", "E")
EXPECTED_ZONES <- 59L
EXPECTED_ROWS <- EXPECTED_ZONES * length(EXPECTED_SEMESTERS)

assert_v10 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

write_csv_v10 <- function(x, filename) {
  utils::write.csv(
    x, file.path(OUTPUT_DIR, filename), row.names = FALSE, na = ""
  )
}

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  stats::cor(x[ok], y[ok], method = method)
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE))
}

tidy_lm_v10 <- function(model, model_name, outcome, specification) {
  sm <- summary(model)
  tab <- sm$coefficients
  data.frame(
    modello = model_name,
    outcome = outcome,
    specifica = specification,
    termine = rownames(tab),
    stima = as.numeric(tab[, 1]),
    errore_standard = as.numeric(tab[, 2]),
    statistica_t = as.numeric(tab[, 3]),
    p_value = as.numeric(tab[, 4]),
    n = stats::nobs(model),
    r2 = unname(sm$r.squared),
    r2_adj = unname(sm$adj.r.squared),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ------------------------------------------------------------------------------
# 1. Lettura e audit bloccante del master
# ------------------------------------------------------------------------------
assert_v10(
  file.exists(MASTER_PATH),
  paste0(
    "Master non trovato: ", MASTER_PATH,
    "\nEseguire prima 00_build_data.R."
  )
)

master <- utils::read.csv(
  MASTER_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_master_cols <- c(
  "zona", "fascia", "anno", "semestre_num", "semestre",
  "compr_min", "compr_max", "loc_min", "loc_max",
  "superficie_compr", "superficie_loc", "compr_medio", "loc_medio",
  "log_sale", "log_rent", "log_rent_sale_gap",
  "d_log_sale", "d_log_rent", "d_log_rent_sale_gap",
  "valid_rent_difference", "valid_sale_difference", "valid_gap_difference",
  "distance_to_B_core_km",
  "quota_affitto_2011", "spazio_mq_procapite_2011",
  "quota_vuote_2011", "quota_terziaria_2011",
  "quota_occupati_15plus_2011"
)
missing_master_cols <- setdiff(required_master_cols, names(master))
assert_v10(
  !length(missing_master_cols),
  paste0(
    "Colonne mancanti nel master: ",
    paste(missing_master_cols, collapse = ", ")
  )
)

master <- master |>
  mutate(
    fascia = factor(fascia, levels = EXPECTED_BANDS),
    semestre_f = factor(semestre, levels = EXPECTED_SEMESTERS)
  ) |>
  arrange(zona, anno, semestre_num)

assert_v10(nrow(master) == EXPECTED_ROWS,
           paste0("Attese ", EXPECTED_ROWS, " righe; trovate ", nrow(master), "."))
assert_v10(dplyr::n_distinct(master$zona) == EXPECTED_ZONES,
           "Il master non contiene 59 zone distinte.")
assert_v10(!anyDuplicated(master[c("zona", "semestre")]),
           "Duplicati zona-semestre nel master.")
assert_v10(
  setequal(unique(master$semestre), EXPECTED_SEMESTERS) &&
    dplyr::n_distinct(master$semestre) == length(EXPECTED_SEMESTERS),
  "Il master non copre esattamente 2016-1 / 2024-1."
)
assert_v10(!"2024-2" %in% master$semestre,
           "ERRORE: 2024-2 e' entrato nel master.")
assert_v10(all(as.character(master$fascia) %in% EXPECTED_BANDS),
           "Fascia OMI inattesa nel master.")
assert_v10(all(is.finite(master$loc_medio) & master$loc_medio > 0),
           "loc_medio contiene valori mancanti/non positivi.")
assert_v10(all(is.finite(master$compr_medio) & master$compr_medio > 0),
           "compr_medio contiene valori mancanti/non positivi.")
assert_v10(all(is.finite(master$distance_to_B_core_km)),
           "Distanza dal core B mancante/non finita.")

main_2018 <- master |> filter(anno >= 2018L)
assert_v10(all(main_2018$superficie_loc == "L"),
           "Dal 2018 la locazione non e' sempre su superficie L.")
assert_v10(all(main_2018$superficie_compr == "L"),
           "Dal 2018 la compravendita non e' sempre su superficie L.")

# Il cambio N -> L avviene in 2018-1: la differenza locativa deve essere invalida.
assert_v10(
  all(master$valid_rent_difference[master$semestre == "2018-1"] == 0),
  "2018-1 risulta erroneamente valido per la variazione locativa."
)

# ------------------------------------------------------------------------------
# 2. Trend per fascia e indici base 2019-1
# ------------------------------------------------------------------------------
band_trends <- master |>
  group_by(fascia, semestre, semestre_f) |>
  summarise(
    n_zone = n(),
    mean_log_rent = mean(log_rent),
    median_log_rent = median(log_rent),
    mean_log_sale = mean(log_sale),
    median_log_sale = median(log_sale),
    mean_log_rent_sale_gap = mean(log_rent_sale_gap),
    median_log_rent_sale_gap = median(log_rent_sale_gap),
    mean_rent = mean(loc_medio),
    median_rent = median(loc_medio),
    mean_sale = mean(compr_medio),
    median_sale = median(compr_medio),
    .groups = "drop"
  ) |>
  arrange(fascia, semestre_f) |>
  group_by(fascia) |>
  mutate(
    rent_index_2019_1 = 100 * exp(
      mean_log_rent - mean_log_rent[semestre == "2019-1"]
    ),
    sale_index_2019_1 = 100 * exp(
      mean_log_sale - mean_log_sale[semestre == "2019-1"]
    )
  ) |>
  ungroup()

assert_v10(all(band_trends$n_zone > 0), "Fascia-semestre senza zone.")
write_csv_v10(band_trends, "01_band_trends.csv")

# ------------------------------------------------------------------------------
# 3. Funzione per transizioni chiave: log, percentuali e variazioni assolute
# ------------------------------------------------------------------------------
make_transition <- function(data, from, to, label) {
  sub <- data |>
    filter(semestre %in% c(from, to)) |>
    select(
      zona, fascia, semestre,
      loc_medio, compr_medio, log_rent, log_sale,
      distance_to_B_core_km,
      quota_affitto_2011, spazio_mq_procapite_2011,
      quota_vuote_2011, quota_terziaria_2011,
      quota_occupati_15plus_2011
    )

  assert_v10(
    nrow(sub) == EXPECTED_ZONES * 2L,
    paste0("Transizione ", label, ": non ci sono 59 zone in entrambi i periodi.")
  )

  w <- sub |>
    pivot_wider(
      id_cols = c(
        zona, fascia, distance_to_B_core_km,
        quota_affitto_2011, spazio_mq_procapite_2011,
        quota_vuote_2011, quota_terziaria_2011,
        quota_occupati_15plus_2011
      ),
      names_from = semestre,
      values_from = c(loc_medio, compr_medio, log_rent, log_sale),
      names_sep = "__"
    ) |>
    mutate(
      transizione = label,
      rent_initial = .data[[paste0("loc_medio__", from)]],
      rent_final = .data[[paste0("loc_medio__", to)]],
      sale_initial = .data[[paste0("compr_medio__", from)]],
      sale_final = .data[[paste0("compr_medio__", to)]],
      log_rent_initial = .data[[paste0("log_rent__", from)]],
      log_rent_final = .data[[paste0("log_rent__", to)]],
      log_sale_initial = .data[[paste0("log_sale__", from)]],
      log_sale_final = .data[[paste0("log_sale__", to)]],
      d_log_rent = log_rent_final - log_rent_initial,
      d_log_sale = log_sale_final - log_sale_initial,
      d_log_rent_sale_gap = d_log_rent - d_log_sale,
      pct_rent = rent_final / rent_initial - 1,
      pct_sale = sale_final / sale_initial - 1,
      abs_rent = rent_final - rent_initial,
      abs_sale = sale_final - sale_initial
    ) |>
    select(
      transizione, zona, fascia, distance_to_B_core_km,
      rent_initial, rent_final, d_log_rent, pct_rent, abs_rent,
      sale_initial, sale_final, d_log_sale, pct_sale, abs_sale,
      d_log_rent_sale_gap,
      log_rent_initial, log_rent_final, log_sale_initial, log_sale_final,
      quota_affitto_2011, spazio_mq_procapite_2011,
      quota_vuote_2011, quota_terziaria_2011,
      quota_occupati_15plus_2011
    )

  assert_v10(nrow(w) == EXPECTED_ZONES,
             paste0("Transizione ", label, ": righe finali diverse da 59."))
  assert_v10(all(is.finite(w$d_log_rent)),
             paste0("Transizione ", label, ": d_log_rent non finita."))
  assert_v10(all(is.finite(w$d_log_sale)),
             paste0("Transizione ", label, ": d_log_sale non finita."))

  summary_band <- w |>
    group_by(fascia) |>
    summarise(
      n_zone = n(),
      mean_d_log_rent = mean(d_log_rent),
      median_d_log_rent = median(d_log_rent),
      mean_pct_rent = mean(pct_rent),
      median_pct_rent = median(pct_rent),
      mean_abs_rent = mean(abs_rent),
      median_abs_rent = median(abs_rent),
      share_rent_up = mean(d_log_rent > 1e-10),
      share_rent_flat = mean(abs(d_log_rent) <= 1e-10),
      share_rent_down = mean(d_log_rent < -1e-10),
      mean_d_log_sale = mean(d_log_sale),
      median_d_log_sale = median(d_log_sale),
      mean_pct_sale = mean(pct_sale),
      median_pct_sale = median(pct_sale),
      mean_abs_sale = mean(abs_sale),
      median_abs_sale = median(abs_sale),
      share_sale_up = mean(d_log_sale > 1e-10),
      share_sale_flat = mean(abs(d_log_sale) <= 1e-10),
      share_sale_down = mean(d_log_sale < -1e-10),
      mean_d_log_rent_sale_gap = mean(d_log_rent_sale_gap),
      median_d_log_rent_sale_gap = median(d_log_rent_sale_gap),
      .groups = "drop"
    ) |>
    mutate(transizione = label, .before = 1)

  list(detail = w, summary = summary_band)
}

break19 <- make_transition(master, "2019-1", "2019-2", "2019-1 -> 2019-2")
catch23 <- make_transition(master, "2023-1", "2023-2", "2023-1 -> 2023-2")

write_csv_v10(break19$detail, "02_break_2019_2_zone_detail.csv")
write_csv_v10(break19$summary, "02b_break_2019_2_by_band.csv")
write_csv_v10(catch23$detail, "03_catchup_2023_2_zone_detail.csv")
write_csv_v10(catch23$summary, "03b_catchup_2023_2_by_band.csv")

# ------------------------------------------------------------------------------
# 4. Dispersione cross-zone nel tempo
# ------------------------------------------------------------------------------
dispersion <- master |>
  group_by(semestre, semestre_f) |>
  summarise(
    n_zone = n(),
    sd_log_rent = sd(log_rent),
    iqr_log_rent = IQR(log_rent),
    p90_p10_log_rent = safe_quantile(log_rent, 0.90) - safe_quantile(log_rent, 0.10),
    cv_rent = sd(loc_medio) / mean(loc_medio),
    sd_log_sale = sd(log_sale),
    iqr_log_sale = IQR(log_sale),
    p90_p10_log_sale = safe_quantile(log_sale, 0.90) - safe_quantile(log_sale, 0.10),
    cv_sale = sd(compr_medio) / mean(compr_medio),
    .groups = "drop"
  ) |>
  arrange(semestre_f)

assert_v10(all(dispersion$n_zone == EXPECTED_ZONES),
           "Dispersione: almeno un semestre non contiene 59 zone.")
write_csv_v10(dispersion, "04_cross_zone_dispersion.csv")

# ------------------------------------------------------------------------------
# 5. Stabilita' dell'ordinamento delle zone: rank correlation
# ------------------------------------------------------------------------------
rank_rows <- lapply(seq_len(length(EXPECTED_SEMESTERS) - 1L), function(i) {
  from <- EXPECTED_SEMESTERS[i]
  to <- EXPECTED_SEMESTERS[i + 1L]

  a <- master |>
    filter(semestre == from) |>
    select(zona, rent_from = loc_medio, sale_from = compr_medio)
  b <- master |>
    filter(semestre == to) |>
    select(zona, rent_to = loc_medio, sale_to = compr_medio)
  j <- inner_join(a, b, by = "zona")

  assert_v10(nrow(j) == EXPECTED_ZONES,
             paste0("Rank transition ", from, " -> ", to, ": meno di 59 zone."))

  data.frame(
    from = from,
    to = to,
    surface_break_rent = as.integer(to == "2018-1"),
    spearman_rent = safe_cor(j$rent_from, j$rent_to, "spearman"),
    pearson_rent = safe_cor(j$rent_from, j$rent_to, "pearson"),
    spearman_sale = safe_cor(j$sale_from, j$sale_to, "spearman"),
    pearson_sale = safe_cor(j$sale_from, j$sale_to, "pearson"),
    stringsAsFactors = FALSE
  )
})
rank_stability <- bind_rows(rank_rows)
write_csv_v10(rank_stability, "05_rank_stability_adjacent_semesters.csv")

# ------------------------------------------------------------------------------
# 6. Gradiente descrittivo continuo: log valore ~ distanza dal core B
# ------------------------------------------------------------------------------
gradient_rows <- lapply(EXPECTED_SEMESTERS, function(s) {
  d <- master |> filter(semestre == s)
  assert_v10(nrow(d) == EXPECTED_ZONES,
             paste0("Gradiente ", s, ": campione diverso da 59 zone."))

  fit_rent <- stats::lm(log_rent ~ distance_to_B_core_km, data = d)
  fit_sale <- stats::lm(log_sale ~ distance_to_B_core_km, data = d)
  tr <- summary(fit_rent)$coefficients["distance_to_B_core_km", ]
  ts <- summary(fit_sale)$coefficients["distance_to_B_core_km", ]

  data.frame(
    semestre = s,
    slope_rent = unname(tr[1]),
    se_rent = unname(tr[2]),
    p_rent = unname(tr[4]),
    r2_rent = summary(fit_rent)$r.squared,
    slope_sale = unname(ts[1]),
    se_sale = unname(ts[2]),
    p_sale = unname(ts[4]),
    r2_sale = summary(fit_sale)$r.squared,
    stringsAsFactors = FALSE
  )
})
gradient <- bind_rows(gradient_rows) |>
  mutate(semestre_f = factor(semestre, levels = EXPECTED_SEMESTERS)) |>
  arrange(semestre_f)
write_csv_v10(gradient, "06_distance_gradient_by_semester.csv")

# ------------------------------------------------------------------------------
# 7. Diagnostica 2019-2: fascia vs livello iniziale
# ------------------------------------------------------------------------------
b19 <- break19$detail |>
  mutate(fascia = factor(fascia, levels = EXPECTED_BANDS))

initial_models <- list(
  rent_band_only = stats::lm(d_log_rent ~ fascia, data = b19),
  rent_initial_only = stats::lm(d_log_rent ~ log_rent_initial, data = b19),
  rent_band_plus_initial = stats::lm(
    d_log_rent ~ fascia + log_rent_initial, data = b19
  ),
  sale_band_only = stats::lm(d_log_sale ~ fascia, data = b19),
  sale_initial_only = stats::lm(d_log_sale ~ log_sale_initial, data = b19),
  sale_band_plus_initial = stats::lm(
    d_log_sale ~ fascia + log_sale_initial, data = b19
  )
)

initial_model_meta <- data.frame(
  modello = names(initial_models),
  outcome = c(
    "d_log_rent", "d_log_rent", "d_log_rent",
    "d_log_sale", "d_log_sale", "d_log_sale"
  ),
  specifica = c(
    "fascia_only", "initial_level_only", "fascia_plus_initial_level",
    "fascia_only", "initial_level_only", "fascia_plus_initial_level"
  ),
  stringsAsFactors = FALSE
)

initial_model_coefficients <- bind_rows(lapply(seq_along(initial_models), function(i) {
  tidy_lm_v10(
    initial_models[[i]],
    initial_model_meta$modello[i],
    initial_model_meta$outcome[i],
    initial_model_meta$specifica[i]
  )
}))
write_csv_v10(initial_model_coefficients, "07_initial_level_diagnostic_models.csv")

initial_level_correlations <- data.frame(
  relazione = c(
    "log_rent_2019_1_vs_d_log_rent_2019_2",
    "rent_2019_1_vs_pct_rent_2019_2",
    "log_sale_2019_1_vs_d_log_sale_2019_2",
    "sale_2019_1_vs_pct_sale_2019_2"
  ),
  pearson = c(
    safe_cor(b19$log_rent_initial, b19$d_log_rent, "pearson"),
    safe_cor(b19$rent_initial, b19$pct_rent, "pearson"),
    safe_cor(b19$log_sale_initial, b19$d_log_sale, "pearson"),
    safe_cor(b19$sale_initial, b19$pct_sale, "pearson")
  ),
  spearman = c(
    safe_cor(b19$log_rent_initial, b19$d_log_rent, "spearman"),
    safe_cor(b19$rent_initial, b19$pct_rent, "spearman"),
    safe_cor(b19$log_sale_initial, b19$d_log_sale, "spearman"),
    safe_cor(b19$sale_initial, b19$pct_sale, "spearman")
  ),
  stringsAsFactors = FALSE
)
write_csv_v10(initial_level_correlations, "07b_initial_level_correlations.csv")

# Correlazione residua entro fascia: rimuove solo medie di fascia, non e' causale.
within_band_cor <- b19 |>
  group_by(fascia) |>
  mutate(
    log_rent_initial_dm = log_rent_initial - mean(log_rent_initial),
    d_log_rent_dm = d_log_rent - mean(d_log_rent),
    log_sale_initial_dm = log_sale_initial - mean(log_sale_initial),
    d_log_sale_dm = d_log_sale - mean(d_log_sale)
  ) |>
  ungroup()

within_band_initial_correlations <- data.frame(
  relazione = c(
    "within_band_log_rent_initial_vs_d_log_rent",
    "within_band_log_sale_initial_vs_d_log_sale"
  ),
  pearson = c(
    safe_cor(within_band_cor$log_rent_initial_dm, within_band_cor$d_log_rent_dm),
    safe_cor(within_band_cor$log_sale_initial_dm, within_band_cor$d_log_sale_dm)
  ),
  stringsAsFactors = FALSE
)
write_csv_v10(
  within_band_initial_correlations,
  "07c_within_band_initial_level_correlations.csv"
)

# ------------------------------------------------------------------------------
# 8. ISTAT 2011: correlazioni e modelli con/senza controllo per livello iniziale
# ------------------------------------------------------------------------------
istat_vars <- c(
  "quota_affitto_2011",
  "spazio_mq_procapite_2011",
  "quota_vuote_2011",
  "quota_terziaria_2011",
  "quota_occupati_15plus_2011"
)

istat_correlations <- bind_rows(lapply(istat_vars, function(v) {
  data.frame(
    variabile = v,
    corr_break_rent_pearson = safe_cor(b19$d_log_rent, b19[[v]], "pearson"),
    corr_break_rent_spearman = safe_cor(b19$d_log_rent, b19[[v]], "spearman"),
    corr_initial_rent_pearson = safe_cor(b19$log_rent_initial, b19[[v]], "pearson"),
    corr_initial_rent_spearman = safe_cor(b19$log_rent_initial, b19[[v]], "spearman"),
    corr_break_sale_pearson = safe_cor(b19$d_log_sale, b19[[v]], "pearson"),
    stringsAsFactors = FALSE
  )
}))
write_csv_v10(istat_correlations, "08_istat2011_correlations_2019_break.csv")

istat_model_rows <- list()
istat_model_i <- 1L
for (v in istat_vars) {
  specs <- list(
    unadjusted = stats::as.formula(paste0("d_log_rent ~ ", v)),
    band_adjusted = stats::as.formula(paste0("d_log_rent ~ fascia + ", v)),
    band_initial_adjusted = stats::as.formula(
      paste0("d_log_rent ~ fascia + log_rent_initial + ", v)
    )
  )

  for (spec_name in names(specs)) {
    fit <- stats::lm(specs[[spec_name]], data = b19)
    tab <- tidy_lm_v10(
      fit,
      paste0("istat_", v, "_", spec_name),
      "d_log_rent",
      spec_name
    )
    tab$esposizione <- v
    tab$coefficiente_esposizione <- tab$termine == v
    istat_model_rows[[istat_model_i]] <- tab
    istat_model_i <- istat_model_i + 1L
  }
}
istat_models <- bind_rows(istat_model_rows)
write_csv_v10(istat_models, "08b_istat2011_diagnostic_models.csv")

# ------------------------------------------------------------------------------
# 9. PRE-2019: dinamiche in prime differenze, escluso 2018-1
# ------------------------------------------------------------------------------
pre_periods <- c("2016-2", "2017-1", "2017-2", "2018-2", "2019-1")

pre_diff <- master |>
  filter(semestre %in% pre_periods) |>
  mutate(
    fascia = factor(fascia, levels = EXPECTED_BANDS),
    semestre_pre = factor(semestre, levels = pre_periods)
  )

assert_v10(nrow(pre_diff) == EXPECTED_ZONES * length(pre_periods),
           "Campione pre-2019 incompleto.")
assert_v10(all(pre_diff$valid_rent_difference == 1),
           "Il pre-period contiene una variazione locativa non valida.")
assert_v10(all(is.finite(pre_diff$d_log_rent)),
           "d_log_rent mancante nel pre-period.")
assert_v10(all(is.finite(pre_diff$d_log_sale)),
           "d_log_sale mancante nel pre-period.")

pre_band_changes <- pre_diff |>
  group_by(semestre, semestre_pre, fascia) |>
  summarise(
    n_zone = n(),
    mean_d_log_rent = mean(d_log_rent),
    median_d_log_rent = median(d_log_rent),
    mean_d_log_sale = mean(d_log_sale),
    median_d_log_sale = median(d_log_sale),
    .groups = "drop"
  ) |>
  arrange(semestre_pre, fascia)
write_csv_v10(pre_band_changes, "09_pre2019_changes_by_band.csv")

# Diagnostica congiunta: il modello con interazioni fascia x semestre migliora
# l'additivo? E' un F-test descrittivo, NON il test inferenziale finale.
# Modello ristretto: ogni semestre puo avere una crescita comune, ma non ci
# sono differenze B/C/D/E. Modello completo: fascia e fascia x semestre.
# Il confronto testa congiuntamente se, nel pre-period, le fasce hanno dinamiche
# diverse da quelle comuni.
pre_rent_restricted <- stats::lm(d_log_rent ~ semestre_pre, data = pre_diff)
pre_rent_full <- stats::lm(d_log_rent ~ fascia * semestre_pre, data = pre_diff)
pre_sale_restricted <- stats::lm(d_log_sale ~ semestre_pre, data = pre_diff)
pre_sale_full <- stats::lm(d_log_sale ~ fascia * semestre_pre, data = pre_diff)

pre_rent_anova <- stats::anova(pre_rent_restricted, pre_rent_full)
pre_sale_anova <- stats::anova(pre_sale_restricted, pre_sale_full)

pretrend_diagnostic <- data.frame(
  outcome = c("d_log_rent", "d_log_sale"),
  null = c(
    "nessuna dinamica temporale differenziale tra fasce nel pre-period",
    "nessuna dinamica temporale differenziale tra fasce nel pre-period"
  ),
  f_stat = c(pre_rent_anova$F[2], pre_sale_anova$F[2]),
  df_num = c(pre_rent_anova$Df[2], pre_sale_anova$Df[2]),
  df_den = c(stats::df.residual(pre_rent_full), stats::df.residual(pre_sale_full)),
  p_value = c(pre_rent_anova$`Pr(>F)`[2], pre_sale_anova$`Pr(>F)`[2]),
  nota = "diagnostica OLS descrittiva; inferenza finale in 02_main_models.R",
  stringsAsFactors = FALSE
)
write_csv_v10(pretrend_diagnostic, "09b_pre2019_joint_diagnostic.csv")

# ------------------------------------------------------------------------------
# 10. Audit OMI di TUTTE le tipologie: 2019-2 e 2023-2
# ------------------------------------------------------------------------------
parse_number_it_v10 <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N.D.", "ND", "-")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

read_semicolon_zip_v10 <- function(zip_path, member, skip = 0L) {
  exdir <- tempfile("v10_audit_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(
    zip_path, files = member, exdir = exdir,
    junkpaths = TRUE, overwrite = TRUE
  )
  p <- file.path(exdir, basename(member))
  assert_v10(file.exists(p), paste0("Impossibile estrarre ", member, "."))

  out <- utils::read.csv2(
    p, skip = skip, header = TRUE, stringsAsFactors = FALSE,
    check.names = FALSE, fileEncoding = "Windows-1252",
    na.strings = c("", "NA"), quote = "\"", comment.char = "", fill = TRUE
  )

  normalize_utf8 <- function(x) {
    if (!is.character(x)) return(x)
    y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = NA_character_))
    bad <- is.na(y) & !is.na(x)
    if (any(bad)) {
      y[bad] <- suppressWarnings(iconv(
        x[bad], from = "Windows-1252", to = "UTF-8", sub = ""
      ))
    }
    y
  }

  out[] <- lapply(out, normalize_utf8)
  nm <- normalize_utf8(names(out))
  nm <- sub("^\\ufeff", "", trimws(nm))
  names(out) <- nm

  unnamed <- is.na(names(out)) | !nzchar(names(out))
  empty_col <- vapply(
    out,
    function(z) all(is.na(z) | trimws(as.character(z)) == ""),
    logical(1)
  )
  drop_cols <- unnamed & empty_col
  if (any(drop_cols)) out <- out[, !drop_cols, drop = FALSE]

  assert_v10(!any(is.na(names(out)) | !nzchar(names(out))),
             paste0("Colonna senza nome non vuota in ", basename(member), "."))
  assert_v10(!anyDuplicated(names(out)),
             paste0("Nomi duplicati in ", basename(member), "."))
  out
}

resolve_omi_zip_collection <- function() {
  processed_candidates <- c(
    file.path(PROCESSED_DIR, "unpacked_omi_raw"),
    file.path(PROJECT_DIR, "omi_raw"),
    file.path(INPUT_DIR, "omi_raw")
  )
  for (d in processed_candidates) {
    if (dir.exists(d)) {
      z <- list.files(d, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
      if (length(z)) return(normalizePath(z, winslash = "/", mustWork = TRUE))
    }
  }

  aggregate_candidates <- c(
    file.path(INPUT_DIR, "omi_raw.zip"),
    file.path(PROJECT_DIR, "omi_raw.zip")
  )
  agg <- aggregate_candidates[file.exists(aggregate_candidates)]
  assert_v10(length(agg) >= 1L,
             "Audit tipologie: omi_raw/ oppure omi_raw.zip non trovato.")

  exdir <- file.path(PROCESSED_DIR, "unpacked_omi_raw_audit")
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(agg[1], exdir = exdir)
  z <- list.files(exdir, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  assert_v10(length(z) > 0L, "Audit tipologie: nessuno ZIP interno in omi_raw.zip.")
  normalizePath(z, winslash = "/", mustWork = TRUE)
}

period_from_zip <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  value_member <- members[grepl("_VALORI\\.csv$", members, ignore.case = TRUE)]
  if (length(value_member) != 1L) return(NA_character_)
  m <- stringr::str_match(
    basename(value_member), "_(20[0-9]{2})([12])_VALORI\\.csv$"
  )
  if (is.na(m[1, 1])) return(NA_character_)
  paste0(m[1, 2], "-", m[1, 3])
}

read_all_types_period <- function(zip_path, semester) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  value_member <- members[grepl("_VALORI\\.csv$", members, ignore.case = TRUE)]
  assert_v10(length(value_member) == 1L,
             paste0("File *_VALORI.csv non univoco in ", basename(zip_path), "."))

  raw <- read_semicolon_zip_v10(zip_path, value_member, skip = 1L)
  required <- c(
    "Comune_amm", "Fascia", "Zona", "Cod_Tip", "Descr_Tipologia", "Stato",
    "Compr_min", "Compr_max", "Loc_min", "Loc_max",
    "Sup_NL_compr", "Sup_NL_loc"
  )
  missing <- setdiff(required, names(raw))
  assert_v10(!length(missing), paste0(
    "Audit tipologie ", semester, ": colonne mancanti: ",
    paste(missing, collapse = ", ")
  ))

  raw |>
    transmute(
      comune = str_to_upper(str_squish(as.character(Comune_amm))),
      fascia = str_to_upper(str_squish(as.character(Fascia))),
      zona = str_to_upper(str_squish(as.character(Zona))),
      cod_tip = str_squish(as.character(Cod_Tip)),
      descr_tipologia = str_squish(as.character(Descr_Tipologia)),
      stato = str_to_upper(str_squish(as.character(Stato))),
      compr_min = parse_number_it_v10(Compr_min),
      compr_max = parse_number_it_v10(Compr_max),
      loc_min = parse_number_it_v10(Loc_min),
      loc_max = parse_number_it_v10(Loc_max),
      superficie_compr = str_to_upper(str_squish(as.character(Sup_NL_compr))),
      superficie_loc = str_to_upper(str_squish(as.character(Sup_NL_loc))),
      semestre = semester
    ) |>
    filter(comune == "F839", nzchar(zona), stato == "NORMALE") |>
    mutate(
      compr_medio = ifelse(
        is.finite(compr_min) & is.finite(compr_max),
        (compr_min + compr_max) / 2, NA_real_
      ),
      loc_medio = ifelse(
        is.finite(loc_min) & is.finite(loc_max),
        (loc_min + loc_max) / 2, NA_real_
      )
    ) |>
    select(
      zona, fascia, cod_tip, descr_tipologia, semestre,
      superficie_compr, superficie_loc, compr_medio, loc_medio
    )
}

all_omi_zips <- resolve_omi_zip_collection()
zip_index <- data.frame(
  zip_path = all_omi_zips,
  semestre = vapply(all_omi_zips, period_from_zip, character(1)),
  stringsAsFactors = FALSE
) |>
  filter(!is.na(semestre))

audit_periods <- c("2019-1", "2019-2", "2023-1", "2023-2")
zip_needed <- zip_index |> filter(semestre %in% audit_periods)
assert_v10(
  nrow(zip_needed) == length(audit_periods) &&
    setequal(zip_needed$semestre, audit_periods),
  "Audit tipologie: non trovo univocamente 2019-1, 2019-2, 2023-1, 2023-2."
)

all_types <- bind_rows(lapply(seq_len(nrow(zip_needed)), function(i) {
  read_all_types_period(zip_needed$zip_path[i], zip_needed$semestre[i])
}))

# Una riga per zona-tipologia-semestre e' necessaria per un confronto pulito.
dup_all_types <- all_types |>
  count(zona, cod_tip, semestre, name = "n") |>
  filter(n > 1L)
assert_v10(
  nrow(dup_all_types) == 0L,
  "Audit tipologie: duplicati zona-tipologia-semestre; confronto non univoco."
)

make_all_types_transition <- function(data, from, to, label) {
  w <- data |>
    filter(semestre %in% c(from, to)) |>
    select(
      zona, fascia, cod_tip, descr_tipologia, semestre,
      superficie_compr, superficie_loc, compr_medio, loc_medio
    ) |>
    pivot_wider(
      id_cols = c(zona, fascia, cod_tip),
      names_from = semestre,
      values_from = c(
        descr_tipologia, superficie_compr, superficie_loc,
        compr_medio, loc_medio
      ),
      names_sep = "__"
    ) |>
    mutate(
      transizione = label,
      descrizione = dplyr::coalesce(
        .data[[paste0("descr_tipologia__", to)]],
        .data[[paste0("descr_tipologia__", from)]]
      ),
      rent_initial = .data[[paste0("loc_medio__", from)]],
      rent_final = .data[[paste0("loc_medio__", to)]],
      sale_initial = .data[[paste0("compr_medio__", from)]],
      sale_final = .data[[paste0("compr_medio__", to)]],
      surface_rent_initial = .data[[paste0("superficie_loc__", from)]],
      surface_rent_final = .data[[paste0("superficie_loc__", to)]],
      surface_sale_initial = .data[[paste0("superficie_compr__", from)]],
      surface_sale_final = .data[[paste0("superficie_compr__", to)]],
      same_surface_rent = !is.na(surface_rent_initial) & !is.na(surface_rent_final) &
        surface_rent_initial == surface_rent_final,
      same_surface_sale = !is.na(surface_sale_initial) & !is.na(surface_sale_final) &
        surface_sale_initial == surface_sale_final,
      d_log_rent = ifelse(
        same_surface_rent & is.finite(rent_initial) & rent_initial > 0 &
          is.finite(rent_final) & rent_final > 0,
        log(rent_final) - log(rent_initial), NA_real_
      ),
      d_log_sale = ifelse(
        same_surface_sale & is.finite(sale_initial) & sale_initial > 0 &
          is.finite(sale_final) & sale_final > 0,
        log(sale_final) - log(sale_initial), NA_real_
      ),
      pct_rent = ifelse(
        same_surface_rent & is.finite(rent_initial) & rent_initial > 0 & is.finite(rent_final),
        rent_final / rent_initial - 1, NA_real_
      ),
      pct_sale = ifelse(
        same_surface_sale & is.finite(sale_initial) & sale_initial > 0 & is.finite(sale_final),
        sale_final / sale_initial - 1, NA_real_
      )
    )

  s <- w |>
    group_by(cod_tip, descrizione) |>
    summarise(
      n_surface_mismatch_rent = sum(!same_surface_rent, na.rm = TRUE),
      n_zone_rent = sum(is.finite(d_log_rent)),
      mean_d_log_rent = ifelse(n_zone_rent > 0, mean(d_log_rent, na.rm = TRUE), NA_real_),
      median_d_log_rent = ifelse(n_zone_rent > 0, median(d_log_rent, na.rm = TRUE), NA_real_),
      median_pct_rent = ifelse(n_zone_rent > 0, median(pct_rent, na.rm = TRUE), NA_real_),
      share_rent_up = ifelse(n_zone_rent > 0, mean(d_log_rent > 1e-10, na.rm = TRUE), NA_real_),
      share_rent_flat = ifelse(n_zone_rent > 0, mean(abs(d_log_rent) <= 1e-10, na.rm = TRUE), NA_real_),
      n_surface_mismatch_sale = sum(!same_surface_sale, na.rm = TRUE),
      n_zone_sale = sum(is.finite(d_log_sale)),
      mean_d_log_sale = ifelse(n_zone_sale > 0, mean(d_log_sale, na.rm = TRUE), NA_real_),
      median_d_log_sale = ifelse(n_zone_sale > 0, median(d_log_sale, na.rm = TRUE), NA_real_),
      median_pct_sale = ifelse(n_zone_sale > 0, median(pct_sale, na.rm = TRUE), NA_real_),
      share_sale_up = ifelse(n_zone_sale > 0, mean(d_log_sale > 1e-10, na.rm = TRUE), NA_real_),
      share_sale_flat = ifelse(n_zone_sale > 0, mean(abs(d_log_sale) <= 1e-10, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) |>
    mutate(transizione = label, .before = 1) |>
    arrange(suppressWarnings(as.numeric(cod_tip)), cod_tip)

  list(detail = w, summary = s)
}

all19 <- make_all_types_transition(
  all_types, "2019-1", "2019-2", "2019-1 -> 2019-2"
)
all23 <- make_all_types_transition(
  all_types, "2023-1", "2023-2", "2023-1 -> 2023-2"
)
write_csv_v10(all19$detail, "10_all_types_2019_2_detail.csv")
write_csv_v10(all19$summary, "10b_all_types_2019_2_summary.csv")
write_csv_v10(all23$detail, "10c_all_types_2023_2_detail.csv")
write_csv_v10(all23$summary, "10d_all_types_2023_2_summary.csv")

# ------------------------------------------------------------------------------
# 11. Grafici descrittivi
# ------------------------------------------------------------------------------
base_theme <- theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

p_rent <- ggplot(
  band_trends,
  aes(x = semestre_f, y = rent_index_2019_1, group = fascia, linetype = fascia)
) +
  geom_hline(yintercept = 100, linewidth = 0.35) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_vline(xintercept = match("2019-2", EXPECTED_SEMESTERS), linetype = "dotted") +
  geom_vline(xintercept = match("2020-1", EXPECTED_SEMESTERS), linetype = "dashed") +
  geom_vline(xintercept = match("2023-2", EXPECTED_SEMESTERS), linetype = "dotdash") +
  labs(
    title = "Napoli: quotazioni locative residenziali per fascia OMI",
    subtitle = "Indice geometrico: 2019-1 = 100; 2024-2 escluso",
    x = NULL, y = "Indice", linetype = "Fascia"
  ) + base_theme

ggsave(
  file.path(OUTPUT_DIR, "fig01_rent_index_by_band.png"),
  p_rent, width = 10, height = 6, dpi = 300, bg = "white"
)

p_sale <- ggplot(
  band_trends,
  aes(x = semestre_f, y = sale_index_2019_1, group = fascia, linetype = fascia)
) +
  geom_hline(yintercept = 100, linewidth = 0.35) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_vline(xintercept = match("2019-2", EXPECTED_SEMESTERS), linetype = "dotted") +
  geom_vline(xintercept = match("2020-1", EXPECTED_SEMESTERS), linetype = "dashed") +
  geom_vline(xintercept = match("2023-2", EXPECTED_SEMESTERS), linetype = "dotdash") +
  labs(
    title = "Napoli: quotazioni di compravendita per fascia OMI",
    subtitle = "Indice geometrico: 2019-1 = 100; stessa tipologia del panel locativo",
    x = NULL, y = "Indice", linetype = "Fascia"
  ) + base_theme

ggsave(
  file.path(OUTPUT_DIR, "fig02_sale_index_by_band.png"),
  p_sale, width = 10, height = 6, dpi = 300, bg = "white"
)

p_disp_rent <- ggplot(
  dispersion,
  aes(x = factor(semestre, levels = EXPECTED_SEMESTERS), y = sd_log_rent, group = 1)
) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  geom_vline(xintercept = match("2019-2", EXPECTED_SEMESTERS), linetype = "dotted") +
  geom_vline(xintercept = match("2023-2", EXPECTED_SEMESTERS), linetype = "dotdash") +
  labs(
    title = "Dispersione spaziale delle quotazioni locative",
    subtitle = "Deviazione standard cross-zone di log(rent)",
    x = NULL, y = "SD log(rent)"
  ) + base_theme

ggsave(
  file.path(OUTPUT_DIR, "fig03_rent_dispersion.png"),
  p_disp_rent, width = 10, height = 6, dpi = 300, bg = "white"
)

p_disp_sale <- ggplot(
  dispersion,
  aes(x = factor(semestre, levels = EXPECTED_SEMESTERS), y = sd_log_sale, group = 1)
) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  geom_vline(xintercept = match("2019-2", EXPECTED_SEMESTERS), linetype = "dotted") +
  geom_vline(xintercept = match("2023-2", EXPECTED_SEMESTERS), linetype = "dotdash") +
  labs(
    title = "Dispersione spaziale delle quotazioni di compravendita",
    subtitle = "Deviazione standard cross-zone di log(sale price)",
    x = NULL, y = "SD log(sale price)"
  ) + base_theme

ggsave(
  file.path(OUTPUT_DIR, "fig04_sale_dispersion.png"),
  p_disp_sale, width = 10, height = 6, dpi = 300, bg = "white"
)

p_initial <- ggplot(
  b19,
  aes(x = log_rent_initial, y = d_log_rent, shape = fascia)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, linetype = "dashed") +
  labs(
    title = "Livello iniziale e crescita locativa nel 2019-2",
    subtitle = "59 zone OMI; relazione descrittiva, non causale",
    x = "log rent, 2019-1", y = "Delta log rent, 2019-2", shape = "Fascia"
  ) + theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())

ggsave(
  file.path(OUTPUT_DIR, "fig05_initial_rent_vs_2019_growth.png"),
  p_initial, width = 8, height = 6, dpi = 300, bg = "white"
)

# Per visualizzare la differenza tra percentuale e variazione assoluta.
p_abs <- ggplot(
  break19$summary,
  aes(x = fascia, y = mean_abs_rent)
) +
  geom_col() +
  labs(
    title = "Variazione assoluta media dei canoni OMI, 2019-2",
    subtitle = "Euro per m2 al mese; midpoint della forchetta OMI",
    x = "Fascia OMI", y = "Delta euro/m2/mese"
  ) + theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())

ggsave(
  file.path(OUTPUT_DIR, "fig06_2019_absolute_rent_change_by_band.png"),
  p_abs, width = 7, height = 5, dpi = 300, bg = "white"
)

p_gradient <- ggplot(
  gradient,
  aes(x = semestre_f, y = slope_rent, group = 1)
) +
  geom_hline(yintercept = 0, linewidth = 0.35) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.5) +
  geom_vline(xintercept = match("2019-2", EXPECTED_SEMESTERS), linetype = "dotted") +
  geom_vline(xintercept = match("2023-2", EXPECTED_SEMESTERS), linetype = "dotdash") +
  labs(
    title = "Evoluzione del gradiente locativo rispetto alla distanza",
    subtitle = "Coefficiente di log(rent) su km dal core B; descrittivo",
    x = NULL, y = "Slope log(rent) / km"
  ) + base_theme

ggsave(
  file.path(OUTPUT_DIR, "fig07_rent_distance_gradient.png"),
  p_gradient, width = 10, height = 6, dpi = 300, bg = "white"
)

p_reversal <- ggplot(
  catch23$detail,
  aes(x = log_rent_initial, y = d_log_rent, shape = fascia)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x, linetype = "dashed") +
  labs(
    title = "Livello iniziale e variazione locativa nel 2023-2",
    subtitle = "Diagnostica del catch-up/riapertura del gradiente",
    x = "log rent, 2023-1", y = "Delta log rent, 2023-2", shape = "Fascia"
  ) + theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())

ggsave(
  file.path(OUTPUT_DIR, "fig08_2023_reversal.png"),
  p_reversal, width = 8, height = 6, dpi = 300, bg = "white"
)

# ------------------------------------------------------------------------------
# 12. Audit finale e README
# ------------------------------------------------------------------------------
rank_2019 <- rank_stability |>
  filter(from == "2019-1", to == "2019-2")
rank_2023 <- rank_stability |>
  filter(from == "2023-1", to == "2023-2")

disp_2019_1 <- dispersion |> filter(semestre == "2019-1")
disp_2019_2 <- dispersion |> filter(semestre == "2019-2")
disp_2023_1 <- dispersion |> filter(semestre == "2023-1")
disp_2023_2 <- dispersion |> filter(semestre == "2023-2")

initial_rent_coef <- initial_model_coefficients |>
  filter(
    modello == "rent_band_plus_initial",
    termine == "log_rent_initial"
  )
assert_v10(nrow(rank_2019) == 1L, "Rank 2019 non univoco.")
assert_v10(nrow(rank_2023) == 1L, "Rank 2023 non univoco.")
assert_v10(nrow(disp_2019_1) == 1L && nrow(disp_2019_2) == 1L,
           "Dispersione 2019 non univoca.")
assert_v10(nrow(disp_2023_1) == 1L && nrow(disp_2023_2) == 1L,
           "Dispersione 2023 non univoca.")
assert_v10(nrow(initial_rent_coef) == 1L,
           "Coefficiente log_rent_initial non univoco nel modello diagnostico.")

# Recupera il coefficiente delle abitazioni civili dall'audit all-types.
type20_2019 <- all19$summary |> filter(cod_tip == "20")
type20_2023 <- all23$summary |> filter(cod_tip == "20")
assert_v10(nrow(type20_2019) == 1L,
           "Audit all-types 2019: Cod_Tip 20 non univoco.")
assert_v10(nrow(type20_2023) == 1L,
           "Audit all-types 2023: Cod_Tip 20 non univoco.")

audit01 <- data.frame(
  controllo = c(
    "Master osservazioni",
    "Master zone",
    "Master semestri",
    "2024-2 escluso",
    "2018-1 escluso dalle variazioni locative",
    "Righe break 2019-2",
    "Righe catch-up 2023-2",
    "Spearman rent 2019-1/2019-2",
    "SD log rent 2019-1",
    "SD log rent 2019-2",
    "Spearman rent 2023-1/2023-2",
    "SD log rent 2023-1",
    "SD log rent 2023-2",
    "Coeff. initial rent nel modello fascia + initial rent",
    "P-value coeff. initial rent",
    "Pre-2019 joint diagnostic rent p-value",
    "Pre-2019 joint diagnostic sale p-value",
    "All-types: zone rent Cod_Tip 20 nel 2019",
    "All-types: mediana d_log_rent Cod_Tip 20 nel 2019",
    "All-types: zone rent Cod_Tip 20 nel 2023",
    "All-types: mediana d_log_rent Cod_Tip 20 nel 2023"
  ),
  valore = c(
    as.character(nrow(master)),
    as.character(dplyr::n_distinct(master$zona)),
    as.character(dplyr::n_distinct(master$semestre)),
    ifelse(!"2024-2" %in% master$semestre, "SI", "NO"),
    ifelse(all(master$valid_rent_difference[master$semestre == "2018-1"] == 0), "SI", "NO"),
    as.character(nrow(break19$detail)),
    as.character(nrow(catch23$detail)),
    format(rank_2019$spearman_rent, digits = 8),
    format(disp_2019_1$sd_log_rent, digits = 8),
    format(disp_2019_2$sd_log_rent, digits = 8),
    format(rank_2023$spearman_rent, digits = 8),
    format(disp_2023_1$sd_log_rent, digits = 8),
    format(disp_2023_2$sd_log_rent, digits = 8),
    format(initial_rent_coef$stima, digits = 8),
    format(initial_rent_coef$p_value, digits = 8),
    format(pretrend_diagnostic$p_value[pretrend_diagnostic$outcome == "d_log_rent"], digits = 8),
    format(pretrend_diagnostic$p_value[pretrend_diagnostic$outcome == "d_log_sale"], digits = 8),
    as.character(type20_2019$n_zone_rent),
    format(type20_2019$median_d_log_rent, digits = 8),
    as.character(type20_2023$n_zone_rent),
    format(type20_2023$median_d_log_rent, digits = 8)
  ),
  esito = c(
    ifelse(nrow(master) == EXPECTED_ROWS, "PASS", "FAIL"),
    ifelse(dplyr::n_distinct(master$zona) == EXPECTED_ZONES, "PASS", "FAIL"),
    ifelse(dplyr::n_distinct(master$semestre) == 17L, "PASS", "FAIL"),
    ifelse(!"2024-2" %in% master$semestre, "PASS", "FAIL"),
    ifelse(all(master$valid_rent_difference[master$semestre == "2018-1"] == 0), "PASS", "FAIL"),
    ifelse(nrow(break19$detail) == EXPECTED_ZONES, "PASS", "FAIL"),
    ifelse(nrow(catch23$detail) == EXPECTED_ZONES, "PASS", "FAIL"),
    "INFO", "INFO", "INFO", "INFO", "INFO", "INFO",
    "INFO", "INFO", "INFO", "INFO", "PASS", "INFO", "PASS", "INFO"
  ),
  stringsAsFactors = FALSE
)
write_csv_v10(audit01, "11_audit_summary_01.csv")

readme_lines <- c(
  "V10 - 01 DESCRIPTIVE AUDIT",
  "==========================",
  "",
  "SCOPO",
  "- Descrivere e stress-testare il repricing locativo prima dei modelli principali.",
  "- Nessun coefficiente di questo script e' interpretato causalmente.",
  "- 2019-2 e 2023-2 sono transizioni osservate, non trattamenti.",
  "",
  "CAMPIONE",
  paste0("- Periodo: ", EXPECTED_SEMESTERS[1], " / ", tail(EXPECTED_SEMESTERS, 1), "."),
  paste0("- Zone bilanciate: ", EXPECTED_ZONES, "."),
  paste0("- Osservazioni: ", EXPECTED_ROWS, "."),
  "- 2024-2 escluso per disegno.",
  "- La variazione locativa 2018-1 non viene usata perche attraversa N -> L.",
  "",
  "OUTPUT CHIAVE",
  "- 02b_break_2019_2_by_band.csv: variazioni 2019-2 in log, %, euro/m2/mese.",
  "- 04_cross_zone_dispersion.csv: SD/IQR/P90-P10 dei valori tra zone.",
  "- 05_rank_stability_adjacent_semesters.csv: stabilita del ranking delle zone.",
  "- 06_distance_gradient_by_semester.csv: slope rispetto alla distanza dal core B.",
  "- 07_initial_level_diagnostic_models.csv: fascia vs livello iniziale.",
  "- 08b_istat2011_diagnostic_models.csv: ISTAT con/senza livello iniziale e fascia.",
  "- 09b_pre2019_joint_diagnostic.csv: diagnostica delle dinamiche pre-2019.",
  "- 10b/10d: audit di tutte le tipologie immobiliari nel 2019-2 e 2023-2.",
  "- 11_audit_summary_01.csv: checkpoint meccanico finale.",
  "",
  "INTERPRETAZIONE",
  "- Una compressione della dispersione o del gradiente e' un fatto descrittivo.",
  "- Una relazione negativa tra livello iniziale e crescita puo essere coerente con convergenza,",
  "  mean reversion o aggiornamento discreto OMI; questo script non distingue causalmente le alternative.",
  "- Le caratteristiche ISTAT 2011 sono preesistenti ma non automaticamente esogene.",
  "- I p-value OLS cross-section qui prodotti sono diagnostici; la specificazione e l'inferenza finale",
  "  verranno definite in 02_main_models.R.",
  ""
)
writeLines(readme_lines, file.path(OUTPUT_DIR, "README_01_DESCRIPTIVE_AUDIT.txt"))

message("============================================================")
message("01_descriptive_audit.R COMPLETATO")
message("Master: 59 zone x 17 semestri; 2024-2 escluso.")
message("Controllare per primi:")
message("  output_v10/11_audit_summary_01.csv")
message("  output_v10/02b_break_2019_2_by_band.csv")
message("  output_v10/04_cross_zone_dispersion.csv")
message("  output_v10/07_initial_level_diagnostic_models.csv")
message("  output_v10/09b_pre2019_joint_diagnostic.csv")
message("  output_v10/10b_all_types_2019_2_summary.csv")
message("Nessuna interpretazione causale viene prodotta da questo script.")
message("============================================================")
