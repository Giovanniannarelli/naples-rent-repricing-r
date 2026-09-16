# Public portfolio extract from the MSc thesis pipeline
# External validation of pre-policy RdC exposure at Municipality level

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "sf")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

assert_ok <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# -----------------------------------------------------------------------------
# 1. Aggregate OMI-zone exposure to Naples' ten Municipalities
# -----------------------------------------------------------------------------
# `sections_2021` is a census-section sf object containing:
# SEZ21_ID, municipalita, PF1 (resident families).
# `omi_zones` is an sf object containing zone code and rdc_exposure_main.
# -----------------------------------------------------------------------------

aggregate_exposure_to_municipality <- function(sections_2021, omi_zones) {
  assert_ok(inherits(sections_2021, "sf"), "sections_2021 must be an sf object.")
  assert_ok(inherits(omi_zones, "sf"), "omi_zones must be an sf object.")
  assert_ok(all(c("SEZ21_ID", "municipalita", "PF1") %in% names(sections_2021)),
            "Missing 2021 census-section variables.")
  assert_ok(all(c("zona", "rdc_exposure_main") %in% names(omi_zones)),
            "Missing OMI exposure variables.")

  sections_metric <- st_transform(sections_2021, 32633)
  omi_metric <- st_transform(omi_zones, 32633)

  # First try an interior-point assignment.
  points <- suppressWarnings(st_point_on_surface(sections_metric))
  assigned_raw <- suppressWarnings(
    st_join(points |> select(SEZ21_ID),
            omi_metric |> select(zona, rdc_exposure_main),
            join = st_intersects,
            left = TRUE)
  ) |>
    st_drop_geometry()

  # Keep unique assignments here; in the full thesis pipeline ambiguous cases
  # are resolved using the zone with the largest polygon-overlap area.
  assignment_counts <- assigned_raw |>
    count(SEZ21_ID, name = "n")
  unique_ids <- assignment_counts |>
    filter(n == 1L) |>
    pull(SEZ21_ID)

  assigned <- assigned_raw |>
    filter(SEZ21_ID %in% unique_ids) |>
    select(SEZ21_ID, zona, rdc_exposure_main)

  section_validation <- sections_2021 |>
    st_drop_geometry() |>
    left_join(assigned, by = "SEZ21_ID")

  section_validation |>
    group_by(municipalita) |>
    summarise(
      predicted_exposure = weighted_mean_safe(rdc_exposure_main, PF1),
      mapped_families = sum(PF1[is.finite(rdc_exposure_main)], na.rm = TRUE),
      total_families = sum(PF1, na.rm = TRUE),
      coverage = mapped_families / total_families,
      .groups = "drop"
    )
}

# -----------------------------------------------------------------------------
# 2. Compare predicted exposure with observed administrative RdC incidence
# -----------------------------------------------------------------------------

validate_against_rdc_caseload <- function(predicted, actual) {
  # `actual` must contain Municipality-level RdC beneficiary households and
  # resident-family denominators.
  required_actual <- c("municipalita", "nuclei_rdc", "famiglie_2021")
  assert_ok(all(required_actual %in% names(actual)),
            "Administrative validation data are incomplete.")

  d <- predicted |>
    inner_join(actual, by = "municipalita") |>
    mutate(rdc_incidence = nuclei_rdc / famiglie_2021)

  assert_ok(nrow(d) == 10L, "Validation should contain ten Municipalities.")

  pearson <- cor.test(d$predicted_exposure, d$rdc_incidence, method = "pearson")
  spearman <- suppressWarnings(
    cor.test(d$predicted_exposure, d$rdc_incidence,
             method = "spearman", exact = FALSE)
  )

  correlations <- data.frame(
    method = c("Pearson", "Spearman"),
    correlation = c(unname(pearson$estimate), unname(spearman$estimate)),
    p_value = c(pearson$p.value, spearman$p.value),
    n_municipalities = nrow(d),
    stringsAsFactors = FALSE
  )

  # Leave-one-Municipality-out influence diagnostic.
  loo <- bind_rows(lapply(seq_len(nrow(d)), function(i) {
    x <- d[-i, , drop = FALSE]
    data.frame(
      omitted_municipality = d$municipalita[i],
      pearson = cor(x$predicted_exposure, x$rdc_incidence, method = "pearson"),
      spearman = cor(x$predicted_exposure, x$rdc_incidence, method = "spearman"),
      stringsAsFactors = FALSE
    )
  }))

  list(data = d, correlations = correlations, leave_one_out = loo)
}

# This exercise validates the territorial exposure construct. It does not
# validate individual eligibility, take-up, or a causal effect of RdC on rents.
