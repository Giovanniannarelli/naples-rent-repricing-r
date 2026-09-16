# Public portfolio extract from the MSc thesis pipeline
# Build and validate the Naples OMI x ISTAT panel

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "tidyr", "stringr", "sf")
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
  library(sf)
})

PROJECT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
INPUT_DIR <- file.path(PROJECT_DIR, "input")
OUTPUT_DIR <- file.path(PROJECT_DIR, "output_v10")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

EXPECTED_SEMESTERS <- c(
  as.vector(rbind(paste0(2016:2023, "-1"), paste0(2016:2023, "-2"))),
  "2024-1"
)

assert_ok <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

parse_number_it <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N.D.", "ND", "-")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

safe_ratio <- function(num, den) {
  ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)
}

zscore_ref <- function(x, reference) {
  mu <- mean(x[reference], na.rm = TRUE)
  sigma <- sd(x[reference], na.rm = TRUE)
  assert_ok(is.finite(sigma) && sigma > 0, "Invalid reference standard deviation.")
  (x - mu) / sigma
}

# -----------------------------------------------------------------------------
# 1. OMI panel construction
# -----------------------------------------------------------------------------
# The original thesis pipeline reads the official OMI semiannual ZIP archives,
# filters Naples (Comune_amm == F839), keeps residential type 20 in NORMAL
# condition, and constructs midpoint rent and sale valuations.
#
# Expected columns after import:
# zona, fascia, anno, semestre_num, semestre,
# compr_min, compr_max, loc_min, loc_max,
# superficie_compr, superficie_loc
# -----------------------------------------------------------------------------

build_omi_panel <- function(omi) {
  required <- c(
    "zona", "fascia", "anno", "semestre_num", "semestre",
    "compr_min", "compr_max", "loc_min", "loc_max",
    "superficie_compr", "superficie_loc"
  )
  assert_ok(all(required %in% names(omi)), "OMI input is missing required columns.")

  omi <- omi |>
    filter(semestre %in% EXPECTED_SEMESTERS) |>
    mutate(
      compr_medio = (compr_min + compr_max) / 2,
      loc_medio = (loc_min + loc_max) / 2,
      log_sale = log(compr_medio),
      log_rent = log(loc_medio),
      log_rent_sale_gap = log(12 * loc_medio) - log(compr_medio),
      period_index = match(semestre, EXPECTED_SEMESTERS)
    ) |>
    arrange(zona, period_index) |>
    group_by(zona) |>
    mutate(
      d_log_sale = log_sale - lag(log_sale),
      d_log_rent = log_rent - lag(log_rent),
      d_log_rent_sale_gap = log_rent_sale_gap - lag(log_rent_sale_gap)
    ) |>
    ungroup()

  assert_ok(!anyDuplicated(omi[c("zona", "semestre")]), "Duplicate OMI zone-semester rows.")
  assert_ok(all(omi$loc_medio > 0 & omi$compr_medio > 0), "Non-positive OMI midpoint.")

  # OMI changes the rental surface convention in 2018H1. That transition is
  # explicitly removed from rental first differences in the thesis design.
  omi <- omi |>
    mutate(
      d_log_rent = ifelse(semestre == "2018-1", NA_real_, d_log_rent),
      d_log_rent_sale_gap = ifelse(semestre == "2018-1", NA_real_, d_log_rent_sale_gap),
      valid_rent_difference = as.integer(is.finite(d_log_rent)),
      valid_sale_difference = as.integer(is.finite(d_log_sale)),
      valid_gap_difference = as.integer(is.finite(d_log_rent_sale_gap))
    )

  zone_counts <- omi |> count(zona, name = "n_semesters")
  balanced_zones <- zone_counts |>
    filter(n_semesters == length(EXPECTED_SEMESTERS)) |>
    pull(zona)

  balanced <- omi |> filter(zona %in% balanced_zones)
  assert_ok(length(balanced_zones) == 59L, "Expected 59 balanced OMI zones.")
  assert_ok(nrow(balanced) == 59L * 17L, "Balanced panel must contain 59 x 17 rows.")

  balanced
}

# -----------------------------------------------------------------------------
# 2. Census-section -> OMI-zone aggregation
# -----------------------------------------------------------------------------
# In the original pipeline, 2011 census sections are assigned to OMI zones
# using sf::st_point_on_surface() and a spatial join. Boundary ambiguities are
# resolved using the largest polygon-overlap area.
# -----------------------------------------------------------------------------

aggregate_census_to_zones <- function(assigned_sections) {
  required <- c(
    "zona", "P1", "P46", "P47", "P60", "P61", "P128",
    "A2", "A6", "A44", "A46", "A47", "A48", "PF1", "PF2", "PF3"
  )
  assert_ok(all(required %in% names(assigned_sections)),
            "Assigned census data are missing required variables.")

  zone <- assigned_sections |>
    group_by(zona) |>
    summarise(across(all_of(setdiff(required, "zona")), ~sum(.x, na.rm = TRUE)),
              .groups = "drop") |>
    mutate(
      quota_affitto_2011 = safe_ratio(A46, PF1),
      quota_proprieta_2011 = safe_ratio(A47, PF1),
      spazio_mq_procapite_2011 = safe_ratio(A44, PF2),
      quota_vuote_2011 = safe_ratio(A6, A2 + A6),
      quota_terziaria_2011 = safe_ratio(P47, P46),
      quota_occupati_15plus_2011 = safe_ratio(P61, P60 + P128)
    )

  tenure_gap <- sum(abs(zone$PF1 - (zone$A46 + zone$A47 + zone$A48)), na.rm = TRUE) /
    sum(zone$PF1, na.rm = TRUE)
  assert_ok(tenure_gap < 0.001, "Tenure counts are inconsistent with household totals.")

  zone
}

# -----------------------------------------------------------------------------
# 3. Merge and final structural checks
# -----------------------------------------------------------------------------

build_master <- function(omi_balanced, istat_zone, zone_geography = NULL) {
  master <- omi_balanced |>
    left_join(istat_zone, by = "zona")

  if (!is.null(zone_geography)) {
    master <- master |>
      left_join(zone_geography, by = c("zona", "fascia"))
  }

  assert_ok(nrow(master) == 1003L, "Final panel must contain 1,003 observations.")
  assert_ok(n_distinct(master$zona) == 59L, "Final panel must contain 59 zones.")
  assert_ok(n_distinct(master$semestre) == 17L, "Final panel must contain 17 semesters.")
  assert_ok(all(is.finite(master$quota_affitto_2011)), "Missing ISTAT zone characteristics.")

  master
}

# The complete thesis version also exports:
# - a zone geography table and adjacency list;
# - spatial GeoPackage files;
# - OMI/ISTAT coverage audits;
# - an NTN municipal-series context file;
# - a data dictionary and baseline profile by OMI band.
