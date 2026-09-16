# Public portfolio extract from the MSc thesis pipeline
# Publication-oriented figures for spatial repricing and RdC exposure

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

# -----------------------------------------------------------------------------
# 1. Rent gradient over time by OMI band
# -----------------------------------------------------------------------------

plot_rent_gradient <- function(master) {
  d <- master |>
    group_by(semestre, period_index, fascia) |>
    summarise(mean_log_rent = mean(log_rent, na.rm = TRUE), .groups = "drop") |>
    arrange(period_index, fascia)

  ggplot(d, aes(x = period_index, y = mean_log_rent,
                group = fascia, linetype = fascia)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    scale_x_continuous(
      breaks = sort(unique(d$period_index)),
      labels = d |>
        distinct(period_index, semestre) |>
        arrange(period_index) |>
        pull(semestre)
    ) +
    labs(
      x = "Semester",
      y = "Mean log OMI rent valuation",
      linetype = "OMI band"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# -----------------------------------------------------------------------------
# 2. Semester-specific RdC exposure coefficients
# -----------------------------------------------------------------------------

plot_dynamic_exposure <- function(dynamic_results) {
  ggplot(dynamic_results,
         aes(x = period_index, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std_error_cluster,
          ymax = estimate + 1.96 * std_error_cluster),
      width = 0.15
    ) +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = dynamic_results$period_index,
      labels = dynamic_results$semestre
    ) +
    labs(
      x = "Semester",
      y = "Exposure coefficient",
      caption = "Within-band association; standard errors clustered by OMI zone"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# -----------------------------------------------------------------------------
# 3. Initial-rent confounding diagnostic
# -----------------------------------------------------------------------------

plot_initial_rent_sensitivity <- function(model_table) {
  # Expected columns: model, estimate, std_error for the RdC exposure term.
  ggplot(model_table,
         aes(x = model, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std_error,
          ymax = estimate + 1.96 * std_error),
      width = 0.15
    ) +
    geom_point(size = 2.2) +
    labs(
      x = NULL,
      y = "RdC exposure coefficient",
      caption = "Sensitivity to alternative controls for pre-policy rent levels"
    ) +
    theme_minimal(base_size = 11)
}

# -----------------------------------------------------------------------------
# 4. Validation scatter plot
# -----------------------------------------------------------------------------

plot_external_validation <- function(validation_data) {
  ggplot(validation_data,
         aes(x = predicted_exposure, y = rdc_incidence)) +
    geom_point(size = 2.4) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      x = "Predicted pre-policy RdC exposure",
      y = "Observed RdC incidence",
      caption = "Municipality-level external validation"
    ) +
    theme_minimal(base_size = 11)
}

# Example export:
# ggsave("output_v10/fig_dynamic_rdc.png",
#        plot_dynamic_exposure(dynamic_results), width = 8, height = 5, dpi = 300)
