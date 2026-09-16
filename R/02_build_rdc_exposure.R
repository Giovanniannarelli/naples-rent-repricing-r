# Public portfolio extract from the MSc thesis pipeline
# Construct a pre-policy territorial exposure index for Reddito di Cittadinanza

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "tidyr", "stringr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

assert_ok <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

zscore_ref <- function(x, ref) {
  mu <- mean(x[ref], na.rm = TRUE)
  sigma <- sd(x[ref], na.rm = TRUE)
  assert_ok(is.finite(mu) && is.finite(sigma) && sigma > 0,
            "Cannot standardise exposure component.")
  (x - mu) / sigma
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# -----------------------------------------------------------------------------
# Inputs
# -----------------------------------------------------------------------------
# `zone_characteristics` is the OMI-zone aggregation of 2011 census data.
# `economic_distress` is the 8milaCensus V6 indicator aggregated from ACE to
# OMI zones using household weights. Neither rent nor sale outcomes enter the
# exposure construction.
# -----------------------------------------------------------------------------

build_rdc_exposure <- function(zone_characteristics, economic_distress,
                               balanced_zones) {
  required <- c("zona", "quota_affitto_2011", "quota_occupati_15plus_2011")
  assert_ok(all(required %in% names(zone_characteristics)),
            "Missing 2011 zone characteristics.")
  assert_ok(all(c("zona", "economic_distress_2011") %in% names(economic_distress)),
            "Missing economic-distress input.")

  exposure <- zone_characteristics |>
    select(zona, quota_affitto_2011, quota_occupati_15plus_2011) |>
    left_join(economic_distress, by = "zona") |>
    mutate(
      balanced_panel = zona %in% balanced_zones,
      renter_share_2011 = quota_affitto_2011,
      low_employment_2011 = 1 - quota_occupati_15plus_2011
    )

  assert_ok(sum(exposure$balanced_panel) == 59L,
            "Expected 59 balanced zones in exposure reference sample.")

  ref <- exposure$balanced_panel

  exposure <- exposure |>
    mutate(
      z_renter_share_2011 = zscore_ref(renter_share_2011, ref),
      z_low_employment_2011 = zscore_ref(low_employment_2011, ref),
      z_economic_distress_2011 = zscore_ref(economic_distress_2011, ref),
      rdc_exposure_main_raw = (
        z_renter_share_2011 +
        z_low_employment_2011 +
        z_economic_distress_2011
      ) / 3,
      rdc_exposure_main = zscore_ref(rdc_exposure_main_raw, ref)
    )

  # Alternative definitions used only as robustness checks.
  X_ref <- as.matrix(exposure[ref, c(
    "z_renter_share_2011",
    "z_low_employment_2011",
    "z_economic_distress_2011"
  )])
  X_all <- as.matrix(exposure[, c(
    "z_renter_share_2011",
    "z_low_employment_2011",
    "z_economic_distress_2011"
  )])

  cov_ref <- cov(X_ref, use = "complete.obs")
  one <- rep(1, ncol(cov_ref))
  invcov <- tryCatch(qr.solve(cov_ref, one), error = function(e) one)
  weights <- as.numeric(invcov / sum(invcov))
  exposure$rdc_exposure_anderson <- zscore_ref(as.numeric(X_all %*% weights), ref)

  pca <- prcomp(X_ref, center = FALSE, scale. = FALSE)
  loading <- pca$rotation[, 1]
  if (sum(loading) < 0) loading <- -loading
  exposure$rdc_exposure_pca1 <- zscore_ref(as.numeric(X_all %*% loading), ref)

  exposure |>
    arrange(zona)
}

# -----------------------------------------------------------------------------
# Design principle
# -----------------------------------------------------------------------------
# The exposure index is pre-policy by construction. Its components come from
# 2011 socioeconomic characteristics and are fixed before looking at 2019 rent
# outcomes. This reduces outcome-driven specification choices, but it does not
# by itself identify a causal treatment effect.
