# Public portfolio extract from the MSc thesis pipeline
# Main econometric models: rent-gradient changes and RdC exposure

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "tidyr", "fixest", "sandwich")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(fixest)
  library(sandwich)
})

assert_ok <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

# HC3 inference for cross-sectional specifications.
tidy_lm_hc3 <- function(fit) {
  V <- sandwich::vcovHC(fit, type = "HC3")
  b <- coef(fit)
  se <- sqrt(diag(V))
  df_r <- df.residual(fit)
  tval <- b / se
  pval <- 2 * pt(abs(tval), df = df_r, lower.tail = FALSE)

  data.frame(
    term = names(b),
    estimate = as.numeric(b),
    std_error = as.numeric(se),
    t = as.numeric(tval),
    p_value = as.numeric(pval),
    n = nobs(fit),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# 1. Spatial-gradient changes by OMI band
# -----------------------------------------------------------------------------

fit_band_change <- function(master, semester, outcome = "d_log_rent") {
  d <- master |>
    filter(semestre == semester) |>
    mutate(fascia = factor(fascia, levels = c("B", "C", "D", "E")))

  assert_ok(nrow(d) == 59L, paste0("Expected 59 zones in ", semester, "."))

  fit <- lm(reformulate("fascia", response = outcome), data = d)
  tidy_lm_hc3(fit)
}

# Example:
# fit_band_change(master, "2019-2", "d_log_rent")
# fit_band_change(master, "2019-2", "d_log_sale")

# -----------------------------------------------------------------------------
# 2. Dynamic exposure model
# -----------------------------------------------------------------------------
# For each valid rent-difference semester, create Exposure x Semester terms and
# absorb OMI-band x semester fixed effects. Standard errors are clustered by
# zone. The coefficients are semester-specific within-band exposure slopes.
# -----------------------------------------------------------------------------

fit_dynamic_exposure <- function(master, outcome = "d_log_rent",
                                 exposure = "rdc_exposure_main") {
  d <- master |>
    filter(is.finite(.data[[outcome]])) |>
    mutate(
      fascia = factor(fascia, levels = c("B", "C", "D", "E")),
      fascia_semestre = interaction(fascia, semestre, drop = TRUE)
    )

  semesters <- d |>
    distinct(semestre, period_index) |>
    arrange(period_index)

  term_map <- data.frame(
    semestre = semesters$semestre,
    period_index = semesters$period_index,
    term = paste0("rdc__", gsub("-", "_", semesters$semestre)),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(term_map))) {
    s <- term_map$semestre[i]
    nm <- term_map$term[i]
    d[[nm]] <- ifelse(d$semestre == s, d[[exposure]], 0)
  }

  fml <- as.formula(
    paste0(outcome, " ~ ", paste(term_map$term, collapse = " + "),
           " | fascia_semestre")
  )

  fit <- feols(fml, data = d, cluster = ~zona, notes = FALSE)

  b <- coef(fit)
  V <- vcov(fit)
  G <- n_distinct(d$zona)
  df_c <- G - 1L

  results <- lapply(seq_len(nrow(term_map)), function(i) {
    nm <- term_map$term[i]
    est <- unname(b[nm])
    se <- sqrt(V[nm, nm])
    tval <- est / se
    p <- 2 * pt(abs(tval), df = df_c, lower.tail = FALSE)

    data.frame(
      semestre = term_map$semestre[i],
      period_index = term_map$period_index[i],
      estimate = est,
      std_error_cluster = se,
      t = tval,
      p_value = p,
      n_cluster = G,
      stringsAsFactors = FALSE
    )
  }) |>
    bind_rows()

  list(model = fit, results = results, term_map = term_map)
}

# -----------------------------------------------------------------------------
# 3. Key 2019H2 identification check
# -----------------------------------------------------------------------------
# M1: OMI band + RdC exposure
# M2: M1 + average 2018 log rent
# The second specification is the preferred control for initial-rent
# confounding because the baseline is fully determined before the 2019 rollout.
# -----------------------------------------------------------------------------

fit_2019_initial_rent_models <- function(master) {
  baseline <- master |>
    filter(semestre %in% c("2018-1", "2018-2")) |>
    group_by(zona) |>
    summarise(
      baseline_log_rent_2018 = mean(log_rent),
      .groups = "drop"
    )

  d <- master |>
    filter(semestre == "2019-2", is.finite(d_log_rent)) |>
    left_join(baseline, by = "zona") |>
    mutate(
      fascia = factor(fascia, levels = c("B", "C", "D", "E")),
      baseline_rent_2018_c = baseline_log_rent_2018 - mean(baseline_log_rent_2018)
    )

  assert_ok(nrow(d) == 59L, "2019H2 cross-section must contain 59 zones.")

  m1 <- lm(d_log_rent ~ fascia + rdc_exposure_main, data = d)
  m2 <- lm(d_log_rent ~ fascia + rdc_exposure_main + baseline_rent_2018_c, data = d)
  m3 <- lm(
    d_log_rent ~ fascia + rdc_exposure_main +
      baseline_rent_2018_c + I(baseline_rent_2018_c^2),
    data = d
  )

  list(
    M1 = tidy_lm_hc3(m1),
    M2 = tidy_lm_hc3(m2),
    M3 = tidy_lm_hc3(m3)
  )
}

# -----------------------------------------------------------------------------
# Interpretation used in the thesis
# -----------------------------------------------------------------------------
# A positive 2019H2 coefficient documents a stronger rental-valuation increase
# in more exposed zones within the same broad OMI band. Because exposure is
# strongly correlated with initially low rents, the coefficient is not treated
# as a causal RdC effect. The attenuation after adding the 2018 rent baseline is
# therefore a central result rather than a robustness detail to be hidden.
