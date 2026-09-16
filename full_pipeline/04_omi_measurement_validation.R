# ==============================================================================
# 04_omi_measurement_validation.R
# V10 - OMI MEASUREMENT VALIDATION / UPDATE-PATTERN AUDIT
# Naples, all property types, 2016H1-2024H1
# ==============================================================================
#
# OBIETTIVO
# ---------
# Valutare quanto gli episodi 2019H2 e 2023H2 siano:
#   (i) eccezionali rispetto agli altri aggiornamenti semestrali OMI;
#   (ii) specifici alle locazioni residenziali;
#   (iii) presenti anche nei prezzi di compravendita;
#   (iv) presenti anche nelle tipologie non residenziali;
#   (v) caratterizzati da aggiornamenti discreti / "a scatti".
#
# Questo script NON identifica un effetto causale e NON modifica il master.
# Serve a valutare la validita' di misura del risultato principale.
#
# PRINCIPI CONSERVATIVI
# ---------------------
# 1) Fonte di verita': input/omi_raw.zip (o copia omonima nella project root).
# 2) 2024H2 non entra: attesi esattamente 17 semestri 2016H1-2024H1.
# 3) Stato == NORMALE per i confronti all-types:
#    evita confronti tra stati qualitativi diversi e rende zona-tipologia univoca.
# 4) Una differenza di rent/sale e' valida SOLO se la convenzione di superficie
#    e' la stessa nei due semestri consecutivi.
# 5) Main residential panel: Cod_Tip 20, 59 zone bilanciate prese dal master V10.
# 6) "Residential" = Cod_Tip 1, 19, 20, 21.
#    Tutte le altre tipologie OMI presenti sono classificate "Nonresidential".
# 7) I cambiamenti sono descritti come OMI quotations/valuations, non contratti.
#
# COSA MISURIAMO
# --------------
# - midpoint, lower bound e upper bound separatamente;
# - share unchanged / up / down;
# - quota di variazioni circa +10% (±1 punto percentuale);
# - quota di variazioni quasi esattamente +10% (±0.1 pp);
# - valore modale della variazione arrotondata a 0.1 pp e sua frequenza;
# - quota di osservazioni in cui entrambi i bounds si muovono;
# - confronto rent vs sale;
# - confronto residential vs nonresidential;
# - rank di 2019H2 e 2023H2 fra tutti i semestri validi.
#
# NOTA SUI +10%
# --------------
# Il test "near +10%" e' SOLO una diagnostica di lumpiness/discrete updating.
# Non prova da solo una revisione amministrativa.
#
# INPUT
# -----
# input/omi_raw.zip
# output_v10/master_naples_housing_panel_rdc.csv
#
# OUTPUT in output_v10/
# ---------------------
# 40_omi_measurement_audit.csv
# 41_omi_type_coverage_by_semester.csv
# 42_omi_all_types_transition_detail.csv
# 43_omi_type_transition_summary.csv
# 44_omi_segment_transition_summary.csv
# 45_omi_type20_transition_summary.csv
# 46_omi_type20_2019_2023_zone_detail.csv
# 47_omi_transition_exceptionality.csv
# 48_omi_update_breadth_by_transition.csv
# 49_omi_modal_change_patterns.csv
# 50_omi_measurement_key_findings.csv
# fig15_omi_type20_rent_sale_changes.png
# fig16_omi_type20_share_changed.png
# fig17_omi_residential_nonresidential.png
# fig18_omi_2019_2023_property_types.png
# README_04_OMI_MEASUREMENT.txt
#
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Packages / paths
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "stringr", "ggplot2")
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

INPUT_DIR <- file.path(PROJECT_DIR, "input")
OUTPUT_DIR <- file.path(PROJECT_DIR, "output_v10")
PROCESSED_DIR <- file.path(PROJECT_DIR, "data_processed_v10")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

assert_04 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

write_csv_04 <- function(x, filename) {
  utils::write.csv(
    x,
    file.path(OUTPUT_DIR, filename),
    row.names = FALSE,
    na = ""
  )
}

find_input_04 <- function(filename) {
  candidates <- c(
    file.path(INPUT_DIR, filename),
    file.path(PROJECT_DIR, filename)
  )
  hit <- candidates[file.exists(candidates)]
  assert_04(
    length(hit) >= 1L,
    paste0(
      "File richiesto non trovato: ", filename,
      "\nControllati:\n- ", paste(candidates, collapse = "\n- ")
    )
  )
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

OMI_RAW_ZIP <- find_input_04("omi_raw.zip")
MASTER_PATH <- file.path(OUTPUT_DIR, "master_naples_housing_panel_rdc.csv")

assert_04(
  file.exists(MASTER_PATH),
  "Manca output_v10/master_naples_housing_panel_rdc.csv. Eseguire prima 02 e 03."
)

EXPECTED_SEMESTERS <- c(
  "2016-1", "2016-2",
  "2017-1", "2017-2",
  "2018-1", "2018-2",
  "2019-1", "2019-2",
  "2020-1", "2020-2",
  "2021-1", "2021-2",
  "2022-1", "2022-2",
  "2023-1", "2023-2",
  "2024-1"
)

RESIDENTIAL_CODES <- c(1L, 19L, 20L, 21L)

EXPECTED_TYPE_CODES <- c(
  1L, 5L, 6L, 7L, 8L, 9L, 10L,
  13L, 14L, 15L, 16L, 18L, 19L, 20L, 21L
)

TOL_ZERO <- 1e-10

# ------------------------------------------------------------------------------
# 1. Utility functions
# ------------------------------------------------------------------------------
parse_number_it_04 <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N.D.", "ND", "-", "....")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

normalize_utf8_04 <- function(x) {
  if (!is.character(x)) return(x)

  y <- suppressWarnings(iconv(
    x, from = "", to = "UTF-8", sub = NA_character_
  ))
  bad <- is.na(y) & !is.na(x)

  if (any(bad)) {
    y[bad] <- suppressWarnings(iconv(
      x[bad],
      from = "Windows-1252",
      to = "UTF-8",
      sub = ""
    ))
  }
  y
}

read_semicolon_zip_04 <- function(zip_path, member, skip = 0L) {
  exdir <- tempfile("omi04_member_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(
    zip_path,
    files = member,
    exdir = exdir,
    junkpaths = TRUE,
    overwrite = TRUE
  )

  p <- file.path(exdir, basename(member))
  assert_04(
    file.exists(p),
    paste0("Impossibile estrarre ", member, " da ", basename(zip_path), ".")
  )

  out <- utils::read.csv2(
    p,
    skip = skip,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "Windows-1252",
    na.strings = c("", "NA"),
    quote = "\"",
    comment.char = "",
    fill = TRUE
  )

  out[] <- lapply(out, normalize_utf8_04)

  nm <- normalize_utf8_04(names(out))
  nm <- sub("^\\ufeff", "", trimws(nm))
  names(out) <- nm

  unnamed <- is.na(names(out)) | !nzchar(names(out))
  empty_col <- vapply(
    out,
    function(z) all(is.na(z) | trimws(as.character(z)) == ""),
    logical(1)
  )

  drop_cols <- unnamed & empty_col
  if (any(drop_cols)) {
    out <- out[, !drop_cols, drop = FALSE]
  }

  assert_04(
    !any(is.na(names(out)) | !nzchar(names(out))),
    paste0("Colonna senza nome non vuota in ", basename(member), ".")
  )
  assert_04(
    !anyDuplicated(names(out)),
    paste0("Nomi colonna duplicati in ", basename(member), ".")
  )

  out
}

period_from_nested_zip_04 <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  value_member <- members[
    grepl("_VALORI\\.csv$", members, ignore.case = TRUE)
  ]

  if (length(value_member) != 1L) return(NA_character_)

  m <- stringr::str_match(
    basename(value_member),
    "_(20[0-9]{2})([12])_VALORI\\.csv$"
  )

  if (is.na(m[1, 1])) return(NA_character_)
  paste0(m[1, 2], "-", m[1, 3])
}

period_index_04 <- function(semester) {
  year <- as.integer(substr(semester, 1, 4))
  half <- as.integer(substr(semester, 6, 6))
  (year - 2016L) * 2L + half
}

safe_mean_04 <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

safe_median_04 <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  stats::median(x)
}

safe_sd_04 <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

safe_share_04 <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

mode_value_round_04 <- function(x, digits = 1L, exclude_zero = FALSE) {
  x <- x[is.finite(x)]
  if (exclude_zero) {
    x <- x[abs(x) > TOL_ZERO]
  }
  if (!length(x)) return(NA_real_)

  xr <- round(x, digits = digits)
  tab <- sort(table(xr), decreasing = TRUE)

  # In caso di tie scegliamo il valore con minore valore assoluto;
  # e poi quello numericamente minore. La scelta e' solo diagnostica.
  top_n <- max(tab)
  candidates <- as.numeric(names(tab)[tab == top_n])
  candidates <- candidates[
    order(abs(candidates), candidates)
  ]
  candidates[1]
}

mode_share_round_04 <- function(x, digits = 1L, exclude_zero = FALSE) {
  x <- x[is.finite(x)]
  if (exclude_zero) {
    x <- x[abs(x) > TOL_ZERO]
  }
  if (!length(x)) return(NA_real_)

  xr <- round(x, digits = digits)
  max(table(xr)) / length(xr)
}

# ------------------------------------------------------------------------------
# 2. Unpack aggregate omi_raw.zip -> 17 nested ZIPs
# ------------------------------------------------------------------------------
unpack_dir <- file.path(PROCESSED_DIR, "unpacked_omi_raw_measurement")

if (dir.exists(unpack_dir)) {
  unlink(unpack_dir, recursive = TRUE, force = TRUE)
}
dir.create(unpack_dir, recursive = TRUE, showWarnings = FALSE)

utils::unzip(
  OMI_RAW_ZIP,
  exdir = unpack_dir,
  overwrite = TRUE
)

nested_zips <- list.files(
  unpack_dir,
  pattern = "\\.zip$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

assert_04(
  length(nested_zips) == 17L,
  paste0(
    "Attesi 17 ZIP OMI interni 2016H1-2024H1; trovati ",
    length(nested_zips), "."
  )
)

zip_index <- data.frame(
  zip_path = normalizePath(
    nested_zips,
    winslash = "/",
    mustWork = TRUE
  ),
  semestre = vapply(
    nested_zips,
    period_from_nested_zip_04,
    character(1)
  ),
  stringsAsFactors = FALSE
) |>
  mutate(
    period_index = period_index_04(semestre)
  ) |>
  arrange(period_index)

assert_04(
  !anyNA(zip_index$semestre),
  "Almeno uno ZIP interno non permette di ricostruire il semestre."
)
assert_04(
  !anyDuplicated(zip_index$semestre),
  "Semestri duplicati negli ZIP OMI."
)
assert_04(
  identical(zip_index$semestre, EXPECTED_SEMESTERS),
  paste0(
    "Sequenza semestri inattesa.\nTrovata: ",
    paste(zip_index$semestre, collapse = ", ")
  )
)
assert_04(
  !"2024-2" %in% zip_index$semestre,
  "2024H2 non deve entrare in questo progetto."
)

# ------------------------------------------------------------------------------
# 3. Read all NORMAL-state quotations, all property types, all semesters
# ------------------------------------------------------------------------------
read_one_omi_period_04 <- function(zip_path, semester) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  value_member <- members[
    grepl("_VALORI\\.csv$", members, ignore.case = TRUE)
  ]

  assert_04(
    length(value_member) == 1L,
    paste0(
      "File *_VALORI.csv non univoco in ",
      basename(zip_path), "."
    )
  )

  raw <- read_semicolon_zip_04(
    zip_path,
    value_member,
    skip = 1L
  )

  required <- c(
    "Comune_amm",
    "Fascia",
    "Zona",
    "Cod_Tip",
    "Descr_Tipologia",
    "Stato",
    "Compr_min",
    "Compr_max",
    "Sup_NL_compr",
    "Loc_min",
    "Loc_max",
    "Sup_NL_loc"
  )

  missing <- setdiff(required, names(raw))
  assert_04(
    !length(missing),
    paste0(
      "Colonne mancanti in ", semester, ": ",
      paste(missing, collapse = ", ")
    )
  )

  raw |>
    transmute(
      comune = str_to_upper(
        str_squish(as.character(Comune_amm))
      ),
      fascia = str_to_upper(
        str_squish(as.character(Fascia))
      ),
      zona = str_to_upper(
        str_squish(as.character(Zona))
      ),
      cod_tip = suppressWarnings(
        as.integer(str_squish(as.character(Cod_Tip)))
      ),
      descr_tipologia = str_squish(
        as.character(Descr_Tipologia)
      ),
      stato = str_to_upper(
        str_squish(as.character(Stato))
      ),
      compr_min = parse_number_it_04(Compr_min),
      compr_max = parse_number_it_04(Compr_max),
      superficie_compr = str_to_upper(
        str_squish(as.character(Sup_NL_compr))
      ),
      loc_min = parse_number_it_04(Loc_min),
      loc_max = parse_number_it_04(Loc_max),
      superficie_loc = str_to_upper(
        str_squish(as.character(Sup_NL_loc))
      ),
      semestre = semester,
      period_index = period_index_04(semester)
    ) |>
    filter(
      comune == "F839",
      nzchar(zona),
      stato == "NORMALE"
    ) |>
    mutate(
      segmento = ifelse(
        cod_tip %in% RESIDENTIAL_CODES,
        "Residential",
        "Nonresidential"
      ),
      compr_medio = ifelse(
        is.finite(compr_min) &
          is.finite(compr_max),
        (compr_min + compr_max) / 2,
        NA_real_
      ),
      loc_medio = ifelse(
        is.finite(loc_min) &
          is.finite(loc_max),
        (loc_min + loc_max) / 2,
        NA_real_
      )
    )
}

all_types <- bind_rows(lapply(
  seq_len(nrow(zip_index)),
  function(i) {
    read_one_omi_period_04(
      zip_index$zip_path[i],
      zip_index$semestre[i]
    )
  }
))

assert_04(
  setequal(sort(unique(all_types$cod_tip)), EXPECTED_TYPE_CODES),
  paste0(
    "Set Cod_Tip inatteso: ",
    paste(sort(unique(all_types$cod_tip)), collapse = ", ")
  )
)

dup_all <- all_types |>
  count(
    semestre,
    zona,
    cod_tip,
    name = "n"
  ) |>
  filter(n > 1L)

assert_04(
  nrow(dup_all) == 0L,
  paste0(
    "Duplicati zona-tipologia-semestre nello stato NORMALE: ",
    nrow(dup_all), "."
  )
)

# Descrizione deve essere stabile per codice.
descr_check <- all_types |>
  distinct(cod_tip, descr_tipologia) |>
  count(cod_tip, name = "n_descr") |>
  filter(n_descr > 1L)

assert_04(
  nrow(descr_check) == 0L,
  "Un Cod_Tip ha descrizioni diverse nel tempo."
)

# ------------------------------------------------------------------------------
# 4. Coverage by semester/type
# ------------------------------------------------------------------------------
type_coverage <- all_types |>
  group_by(
    semestre,
    period_index,
    segmento,
    cod_tip,
    descr_tipologia
  ) |>
  summarise(
    n_zone = n_distinct(zona),
    n_rent_quoted = sum(
      is.finite(loc_min) & is.finite(loc_max)
    ),
    n_sale_quoted = sum(
      is.finite(compr_min) & is.finite(compr_max)
    ),
    surface_rent_values = paste(
      sort(unique(superficie_loc[nzchar(superficie_loc)])),
      collapse = "|"
    ),
    surface_sale_values = paste(
      sort(unique(superficie_compr[nzchar(superficie_compr)])),
      collapse = "|"
    ),
    .groups = "drop"
  ) |>
  arrange(period_index, cod_tip)

write_csv_04(
  type_coverage,
  "41_omi_type_coverage_by_semester.csv"
)

# ------------------------------------------------------------------------------
# 5. Build ADJACENT transition panel
# ------------------------------------------------------------------------------
transition_detail <- all_types |>
  arrange(
    zona,
    cod_tip,
    period_index
  ) |>
  group_by(
    zona,
    cod_tip
  ) |>
  mutate(
    prev_semestre = lag(semestre),
    prev_period_index = lag(period_index),

    prev_fascia = lag(fascia),
    prev_descr_tipologia = lag(descr_tipologia),

    prev_superficie_loc = lag(superficie_loc),
    prev_superficie_compr = lag(superficie_compr),

    prev_loc_min = lag(loc_min),
    prev_loc_max = lag(loc_max),
    prev_loc_medio = lag(loc_medio),

    prev_compr_min = lag(compr_min),
    prev_compr_max = lag(compr_max),
    prev_compr_medio = lag(compr_medio)
  ) |>
  ungroup() |>
  mutate(
    adjacent_transition =
      is.finite(prev_period_index) &
      (period_index - prev_period_index == 1L),

    same_surface_rent =
      adjacent_transition &
      !is.na(prev_superficie_loc) &
      !is.na(superficie_loc) &
      prev_superficie_loc == superficie_loc,

    same_surface_sale =
      adjacent_transition &
      !is.na(prev_superficie_compr) &
      !is.na(superficie_compr) &
      prev_superficie_compr == superficie_compr,

    valid_rent =
      same_surface_rent &
      is.finite(prev_loc_medio) &
      prev_loc_medio > 0 &
      is.finite(loc_medio) &
      loc_medio > 0,

    valid_sale =
      same_surface_sale &
      is.finite(prev_compr_medio) &
      prev_compr_medio > 0 &
      is.finite(compr_medio) &
      compr_medio > 0,

    d_log_rent = ifelse(
      valid_rent,
      log(loc_medio) - log(prev_loc_medio),
      NA_real_
    ),

    pct_rent = ifelse(
      valid_rent,
      100 * (loc_medio / prev_loc_medio - 1),
      NA_real_
    ),

    pct_loc_min = ifelse(
      valid_rent &
        is.finite(prev_loc_min) &
        prev_loc_min > 0 &
        is.finite(loc_min),
      100 * (loc_min / prev_loc_min - 1),
      NA_real_
    ),

    pct_loc_max = ifelse(
      valid_rent &
        is.finite(prev_loc_max) &
        prev_loc_max > 0 &
        is.finite(loc_max),
      100 * (loc_max / prev_loc_max - 1),
      NA_real_
    ),

    d_log_sale = ifelse(
      valid_sale,
      log(compr_medio) - log(prev_compr_medio),
      NA_real_
    ),

    pct_sale = ifelse(
      valid_sale,
      100 * (compr_medio / prev_compr_medio - 1),
      NA_real_
    ),

    pct_compr_min = ifelse(
      valid_sale &
        is.finite(prev_compr_min) &
        prev_compr_min > 0 &
        is.finite(compr_min),
      100 * (compr_min / prev_compr_min - 1),
      NA_real_
    ),

    pct_compr_max = ifelse(
      valid_sale &
        is.finite(prev_compr_max) &
        prev_compr_max > 0 &
        is.finite(compr_max),
      100 * (compr_max / prev_compr_max - 1),
      NA_real_
    ),

    rent_flat = ifelse(
      valid_rent,
      abs(pct_rent) <= TOL_ZERO,
      NA
    ),
    rent_up = ifelse(
      valid_rent,
      pct_rent > TOL_ZERO,
      NA
    ),
    rent_down = ifelse(
      valid_rent,
      pct_rent < -TOL_ZERO,
      NA
    ),

    sale_flat = ifelse(
      valid_sale,
      abs(pct_sale) <= TOL_ZERO,
      NA
    ),
    sale_up = ifelse(
      valid_sale,
      pct_sale > TOL_ZERO,
      NA
    ),
    sale_down = ifelse(
      valid_sale,
      pct_sale < -TOL_ZERO,
      NA
    ),

    rent_near_plus10 = ifelse(
      valid_rent,
      abs(pct_rent - 10) <= 1,
      NA
    ),
    rent_exact_plus10 = ifelse(
      valid_rent,
      abs(pct_rent - 10) <= 0.1,
      NA
    ),

    both_rent_bounds_changed = ifelse(
      valid_rent,
      abs(pct_loc_min) > TOL_ZERO &
        abs(pct_loc_max) > TOL_ZERO,
      NA
    ),

    both_sale_bounds_changed = ifelse(
      valid_sale,
      abs(pct_compr_min) > TOL_ZERO &
        abs(pct_compr_max) > TOL_ZERO,
      NA
    ),

    rent_bound_change_gap_pp = ifelse(
      valid_rent,
      abs(pct_loc_min - pct_loc_max),
      NA_real_
    ),

    sale_bound_change_gap_pp = ifelse(
      valid_sale,
      abs(pct_compr_min - pct_compr_max),
      NA_real_
    )
  ) |>
  filter(
    adjacent_transition
  ) |>
  arrange(
    period_index,
    segmento,
    cod_tip,
    zona
  )

assert_04(
  min(transition_detail$period_index) == 2L &&
    max(transition_detail$period_index) == 17L,
  "Transition panel non copre correttamente le 16 transizioni."
)

write_csv_04(
  transition_detail,
  "42_omi_all_types_transition_detail.csv"
)

# ------------------------------------------------------------------------------
# 6. Summary helper
# ------------------------------------------------------------------------------
summarise_transition_04 <- function(data, group_vars) {
  data |>
    group_by(
      across(all_of(group_vars))
    ) |>
    summarise(
      n_matched = n(),

      n_valid_rent = sum(valid_rent),
      n_surface_mismatch_rent =
        sum(adjacent_transition & !same_surface_rent),

      mean_pct_rent =
        safe_mean_04(pct_rent[valid_rent]),
      median_pct_rent =
        safe_median_04(pct_rent[valid_rent]),
      sd_pct_rent =
        safe_sd_04(pct_rent[valid_rent]),

      share_flat_rent =
        safe_share_04(
          abs(pct_rent[valid_rent]) <= TOL_ZERO
        ),
      share_up_rent =
        safe_share_04(
          pct_rent[valid_rent] > TOL_ZERO
        ),
      share_down_rent =
        safe_share_04(
          pct_rent[valid_rent] < -TOL_ZERO
        ),
      share_changed_rent =
        safe_share_04(
          abs(pct_rent[valid_rent]) > TOL_ZERO
        ),

      share_near_plus10_rent =
        safe_share_04(
          abs(pct_rent[valid_rent] - 10) <= 1
        ),
      share_exact_plus10_rent =
        safe_share_04(
          abs(pct_rent[valid_rent] - 10) <= 0.1
        ),

      modal_pct_rent_round_0_1pp =
        mode_value_round_04(
          pct_rent[valid_rent],
          digits = 1L,
          exclude_zero = FALSE
        ),
      modal_share_rent =
        mode_share_round_04(
          pct_rent[valid_rent],
          digits = 1L,
          exclude_zero = FALSE
        ),
      modal_nonzero_pct_rent_round_0_1pp =
        mode_value_round_04(
          pct_rent[valid_rent],
          digits = 1L,
          exclude_zero = TRUE
        ),
      modal_nonzero_share_rent =
        mode_share_round_04(
          pct_rent[valid_rent],
          digits = 1L,
          exclude_zero = TRUE
        ),

      mean_pct_loc_min =
        safe_mean_04(pct_loc_min[valid_rent]),
      median_pct_loc_min =
        safe_median_04(pct_loc_min[valid_rent]),
      mean_pct_loc_max =
        safe_mean_04(pct_loc_max[valid_rent]),
      median_pct_loc_max =
        safe_median_04(pct_loc_max[valid_rent]),

      share_both_rent_bounds_changed =
        safe_share_04(
          both_rent_bounds_changed[valid_rent]
        ),
      median_rent_bound_gap_pp =
        safe_median_04(
          rent_bound_change_gap_pp[valid_rent]
        ),

      n_valid_sale = sum(valid_sale),
      n_surface_mismatch_sale =
        sum(adjacent_transition & !same_surface_sale),

      mean_pct_sale =
        safe_mean_04(pct_sale[valid_sale]),
      median_pct_sale =
        safe_median_04(pct_sale[valid_sale]),
      sd_pct_sale =
        safe_sd_04(pct_sale[valid_sale]),

      share_flat_sale =
        safe_share_04(
          abs(pct_sale[valid_sale]) <= TOL_ZERO
        ),
      share_up_sale =
        safe_share_04(
          pct_sale[valid_sale] > TOL_ZERO
        ),
      share_down_sale =
        safe_share_04(
          pct_sale[valid_sale] < -TOL_ZERO
        ),
      share_changed_sale =
        safe_share_04(
          abs(pct_sale[valid_sale]) > TOL_ZERO
        ),

      modal_pct_sale_round_0_1pp =
        mode_value_round_04(
          pct_sale[valid_sale],
          digits = 1L,
          exclude_zero = FALSE
        ),
      modal_share_sale =
        mode_share_round_04(
          pct_sale[valid_sale],
          digits = 1L,
          exclude_zero = FALSE
        ),

      mean_pct_compr_min =
        safe_mean_04(pct_compr_min[valid_sale]),
      median_pct_compr_min =
        safe_median_04(pct_compr_min[valid_sale]),
      mean_pct_compr_max =
        safe_mean_04(pct_compr_max[valid_sale]),
      median_pct_compr_max =
        safe_median_04(pct_compr_max[valid_sale]),

      share_both_sale_bounds_changed =
        safe_share_04(
          both_sale_bounds_changed[valid_sale]
        ),
      median_sale_bound_gap_pp =
        safe_median_04(
          sale_bound_change_gap_pp[valid_sale]
        ),

      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# 7. Summary by property type and broad segment
# ------------------------------------------------------------------------------
type_summary <- summarise_transition_04(
  transition_detail,
  c(
    "semestre",
    "period_index",
    "segmento",
    "cod_tip",
    "descr_tipologia"
  )
) |>
  arrange(
    period_index,
    segmento,
    cod_tip
  )

segment_summary <- summarise_transition_04(
  transition_detail,
  c(
    "semestre",
    "period_index",
    "segmento"
  )
) |>
  arrange(
    period_index,
    segmento
  )

write_csv_04(
  type_summary,
  "43_omi_type_transition_summary.csv"
)

write_csv_04(
  segment_summary,
  "44_omi_segment_transition_summary.csv"
)

# ------------------------------------------------------------------------------
# 8. Main Cod_Tip 20 balanced 59-zone panel
# ------------------------------------------------------------------------------
master <- utils::read.csv(
  MASTER_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

assert_04(
  nrow(master) == 1003L,
  paste0(
    "Master V10 inatteso: ", nrow(master),
    " righe invece di 1003."
  )
)

balanced_zones <- sort(
  unique(as.character(master$zona))
)

assert_04(
  length(balanced_zones) == 59L,
  "Il master V10 non identifica 59 zone bilanciate."
)

main20_levels <- all_types |>
  filter(
    cod_tip == 20L,
    zona %in% balanced_zones
  )

assert_04(
  nrow(main20_levels) == 1003L,
  paste0(
    "Cod_Tip 20 balanced panel inatteso: ",
    nrow(main20_levels),
    " invece di 1003."
  )
)

assert_04(
  all(table(main20_levels$zona) == 17L),
  "Cod_Tip 20 non e' 59 x 17 nel balanced panel."
)

main20 <- transition_detail |>
  filter(
    cod_tip == 20L,
    zona %in% balanced_zones
  )

assert_04(
  sum(main20$valid_rent) == 885L,
  paste0(
    "Attese 885 differenze rent valide Cod_Tip20; trovate ",
    sum(main20$valid_rent), "."
  )
)

assert_04(
  sum(main20$valid_sale) == 944L,
  paste0(
    "Attese 944 differenze sale valide Cod_Tip20; trovate ",
    sum(main20$valid_sale), "."
  )
)

main20_2018h1 <- main20 |>
  filter(semestre == "2018-1")

assert_04(
  nrow(main20_2018h1) == 59L &&
    sum(main20_2018h1$valid_rent) == 0L &&
    all(
      main20_2018h1$prev_superficie_loc == "N" &
        main20_2018h1$superficie_loc == "L"
    ),
  "Il cambio di convenzione N->L del rent 2018H1 non e' quello atteso."
)

type20_summary <- summarise_transition_04(
  main20,
  c(
    "semestre",
    "period_index"
  )
) |>
  filter(
    n_valid_rent > 0
  ) |>
  arrange(period_index)

assert_04(
  nrow(type20_summary) == 15L,
  paste0(
    "Attesi 15 semestri di differenze rent valide Cod_Tip20; trovati ",
    nrow(type20_summary), "."
  )
)

write_csv_04(
  type20_summary,
  "45_omi_type20_transition_summary.csv"
)

type20_special_detail <- main20 |>
  filter(
    semestre %in% c("2019-2", "2023-2")
  ) |>
  select(
    semestre,
    period_index,
    zona,
    fascia,
    prev_semestre,
    prev_superficie_loc,
    superficie_loc,
    prev_loc_min,
    loc_min,
    prev_loc_max,
    loc_max,
    prev_loc_medio,
    loc_medio,
    pct_loc_min,
    pct_loc_max,
    pct_rent,
    rent_flat,
    rent_up,
    rent_near_plus10,
    rent_exact_plus10,
    both_rent_bounds_changed,
    rent_bound_change_gap_pp,
    prev_compr_min,
    compr_min,
    prev_compr_max,
    compr_max,
    prev_compr_medio,
    compr_medio,
    pct_compr_min,
    pct_compr_max,
    pct_sale,
    sale_flat,
    sale_up
  ) |>
  arrange(
    semestre,
    fascia,
    zona
  )

assert_04(
  nrow(type20_special_detail) == 118L,
  "2019H2 + 2023H2 Cod_Tip20 devono contenere 118 righe."
)

write_csv_04(
  type20_special_detail,
  "46_omi_type20_2019_2023_zone_detail.csv"
)

# ------------------------------------------------------------------------------
# 9. Exceptionality ranks across ALL valid transitions
# ------------------------------------------------------------------------------
segment_exceptionality <- segment_summary |>
  filter(
    segmento == "Residential",
    n_valid_rent > 0
  ) |>
  arrange(period_index) |>
  mutate(
    rank_median_rent_desc =
      min_rank(desc(median_pct_rent)),
    rank_mean_rent_desc =
      min_rank(desc(mean_pct_rent)),
    rank_share_changed_desc =
      min_rank(desc(share_changed_rent)),
    rank_share_near10_desc =
      min_rank(desc(share_near_plus10_rent)),
    rank_sale_median_desc =
      min_rank(desc(median_pct_sale)),
    sample = "All residential types"
  )

type20_exceptionality <- type20_summary |>
  mutate(
    rank_median_rent_desc =
      min_rank(desc(median_pct_rent)),
    rank_mean_rent_desc =
      min_rank(desc(mean_pct_rent)),
    rank_share_changed_desc =
      min_rank(desc(share_changed_rent)),
    rank_share_near10_desc =
      min_rank(desc(share_near_plus10_rent)),
    rank_sale_median_desc =
      min_rank(desc(median_pct_sale)),
    sample = "Cod_Tip20 balanced 59"
  )

exceptionality <- bind_rows(
  segment_exceptionality,
  type20_exceptionality
) |>
  select(
    sample,
    semestre,
    period_index,
    n_valid_rent,
    mean_pct_rent,
    median_pct_rent,
    share_changed_rent,
    share_near_plus10_rent,
    median_pct_sale,
    rank_mean_rent_desc,
    rank_median_rent_desc,
    rank_share_changed_desc,
    rank_share_near10_desc,
    rank_sale_median_desc
  ) |>
  arrange(
    sample,
    period_index
  )

write_csv_04(
  exceptionality,
  "47_omi_transition_exceptionality.csv"
)

# ------------------------------------------------------------------------------
# 10. Breadth of updating across property types
# ------------------------------------------------------------------------------
update_breadth <- type_summary |>
  filter(
    n_valid_rent >= 5L
  ) |>
  mutate(
    type_majority_changed_rent =
      share_changed_rent >= 0.50,
    type_majority_up_rent =
      share_up_rent >= 0.50
  ) |>
  group_by(
    semestre,
    period_index,
    segmento
  ) |>
  summarise(
    n_types_with_rent = n(),
    median_type_share_changed =
      median(share_changed_rent, na.rm = TRUE),
    median_type_share_up =
      median(share_up_rent, na.rm = TRUE),
    n_types_majority_changed =
      sum(type_majority_changed_rent, na.rm = TRUE),
    n_types_majority_up =
      sum(type_majority_up_rent, na.rm = TRUE),
    max_type_median_pct_rent =
      max(median_pct_rent, na.rm = TRUE),
    median_of_type_median_pct_rent =
      median(median_pct_rent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(
    period_index,
    segmento
  )

write_csv_04(
  update_breadth,
  "48_omi_update_breadth_by_transition.csv"
)

# ------------------------------------------------------------------------------
# 11. Modal / discrete change patterns
# ------------------------------------------------------------------------------
modal_patterns <- type_summary |>
  filter(
    n_valid_rent > 0
  ) |>
  select(
    semestre,
    period_index,
    segmento,
    cod_tip,
    descr_tipologia,
    n_valid_rent,
    share_flat_rent,
    share_changed_rent,
    share_near_plus10_rent,
    share_exact_plus10_rent,
    modal_pct_rent_round_0_1pp,
    modal_share_rent,
    modal_nonzero_pct_rent_round_0_1pp,
    modal_nonzero_share_rent,
    median_pct_rent,
    mean_pct_rent,
    median_pct_loc_min,
    median_pct_loc_max,
    share_both_rent_bounds_changed,
    median_rent_bound_gap_pp
  ) |>
  arrange(
    period_index,
    segmento,
    cod_tip
  )

write_csv_04(
  modal_patterns,
  "49_omi_modal_change_patterns.csv"
)

# ------------------------------------------------------------------------------
# 12. Key findings table - mechanically extracted, no causal labels
# ------------------------------------------------------------------------------
get_one_04 <- function(df, semester, segment = NULL) {
  out <- df |>
    filter(semestre == semester)

  if (!is.null(segment)) {
    out <- out |>
      filter(segmento == segment)
  }

  assert_04(
    nrow(out) == 1L,
    paste0(
      "Riga non univoca per ", semester,
      ifelse(is.null(segment), "", paste0(" / ", segment)), "."
    )
  )
  out
}

t20_19 <- get_one_04(type20_summary, "2019-2")
t20_23 <- get_one_04(type20_summary, "2023-2")

res_19 <- get_one_04(
  segment_summary,
  "2019-2",
  "Residential"
)
nonres_19 <- get_one_04(
  segment_summary,
  "2019-2",
  "Nonresidential"
)
res_23 <- get_one_04(
  segment_summary,
  "2023-2",
  "Residential"
)
nonres_23 <- get_one_04(
  segment_summary,
  "2023-2",
  "Nonresidential"
)

rank_t20_19 <- type20_exceptionality |>
  filter(semestre == "2019-2")
rank_t20_23 <- type20_exceptionality |>
  filter(semestre == "2023-2")
rank_res_19 <- segment_exceptionality |>
  filter(semestre == "2019-2")
rank_res_23 <- segment_exceptionality |>
  filter(semestre == "2023-2")

key_findings <- data.frame(
  finding = c(
    "Type20 median rent change 2019H2 (%)",
    "Type20 median sale change 2019H2 (%)",
    "Type20 share rent unchanged 2019H2",
    "Type20 share rent near +10% 2019H2",
    "Type20 share both rent bounds changed 2019H2",
    "Type20 rank median rent change 2019H2",
    "All residential median rent change 2019H2 (%)",
    "Nonresidential median rent change 2019H2 (%)",
    "All residential median sale change 2019H2 (%)",
    "Type20 median rent change 2023H2 (%)",
    "Type20 median sale change 2023H2 (%)",
    "Type20 share rent unchanged 2023H2",
    "Type20 share rent near +10% 2023H2",
    "Type20 share both rent bounds changed 2023H2",
    "Type20 rank median rent change 2023H2",
    "All residential median rent change 2023H2 (%)",
    "Nonresidential median rent change 2023H2 (%)",
    "All residential median sale change 2023H2 (%)",
    "Residential rank median rent 2019H2",
    "Residential rank median rent 2023H2"
  ),
  value = c(
    t20_19$median_pct_rent,
    t20_19$median_pct_sale,
    t20_19$share_flat_rent,
    t20_19$share_near_plus10_rent,
    t20_19$share_both_rent_bounds_changed,
    rank_t20_19$rank_median_rent_desc,
    res_19$median_pct_rent,
    nonres_19$median_pct_rent,
    res_19$median_pct_sale,
    t20_23$median_pct_rent,
    t20_23$median_pct_sale,
    t20_23$share_flat_rent,
    t20_23$share_near_plus10_rent,
    t20_23$share_both_rent_bounds_changed,
    rank_t20_23$rank_median_rent_desc,
    res_23$median_pct_rent,
    nonres_23$median_pct_rent,
    res_23$median_pct_sale,
    rank_res_19$rank_median_rent_desc,
    rank_res_23$rank_median_rent_desc
  ),
  stringsAsFactors = FALSE
)

write_csv_04(
  key_findings,
  "50_omi_measurement_key_findings.csv"
)

# ------------------------------------------------------------------------------
# 13. Formal audit
# ------------------------------------------------------------------------------
audit <- data.frame(
  controllo = c(
    "Nested OMI ZIPs",
    "Semester sequence",
    "2024H2 absent",
    "Expected property-type codes",
    "No duplicate normal zone-type-semester",
    "Balanced Type20 level rows",
    "Balanced Type20 zones",
    "Balanced Type20 periods",
    "Valid Type20 rent differences",
    "Valid Type20 sale differences",
    "2018H1 Type20 rent differences excluded",
    "2018H1 Type20 surface N to L",
    "Valid Type20 rent transition periods",
    "Type20 2019H2 zones",
    "Type20 2023H2 zones",
    "2019H2 Residential segment available",
    "2019H2 Nonresidential segment available",
    "2023H2 Residential segment available",
    "2023H2 Nonresidential segment available"
  ),
  valore = c(
    length(nested_zips),
    paste(zip_index$semestre, collapse = ","),
    !("2024-2" %in% zip_index$semestre),
    paste(sort(unique(all_types$cod_tip)), collapse = ","),
    nrow(dup_all),
    nrow(main20_levels),
    n_distinct(main20_levels$zona),
    n_distinct(main20_levels$semestre),
    sum(main20$valid_rent),
    sum(main20$valid_sale),
    sum(main20_2018h1$valid_rent),
    all(
      main20_2018h1$prev_superficie_loc == "N" &
        main20_2018h1$superficie_loc == "L"
    ),
    nrow(type20_summary),
    sum(
      main20$semestre == "2019-2" &
        main20$valid_rent
    ),
    sum(
      main20$semestre == "2023-2" &
        main20$valid_rent
    ),
    nrow(
      segment_summary |>
        filter(
          semestre == "2019-2",
          segmento == "Residential"
        )
    ),
    nrow(
      segment_summary |>
        filter(
          semestre == "2019-2",
          segmento == "Nonresidential"
        )
    ),
    nrow(
      segment_summary |>
        filter(
          semestre == "2023-2",
          segmento == "Residential"
        )
    ),
    nrow(
      segment_summary |>
        filter(
          semestre == "2023-2",
          segmento == "Nonresidential"
        )
    )
  ),
  esito = c(
    ifelse(length(nested_zips) == 17L, "PASS", "FAIL"),
    ifelse(
      identical(zip_index$semestre, EXPECTED_SEMESTERS),
      "PASS", "FAIL"
    ),
    ifelse(!("2024-2" %in% zip_index$semestre), "PASS", "FAIL"),
    ifelse(
      setequal(sort(unique(all_types$cod_tip)), EXPECTED_TYPE_CODES),
      "PASS", "FAIL"
    ),
    ifelse(nrow(dup_all) == 0L, "PASS", "FAIL"),
    ifelse(nrow(main20_levels) == 1003L, "PASS", "FAIL"),
    ifelse(n_distinct(main20_levels$zona) == 59L, "PASS", "FAIL"),
    ifelse(n_distinct(main20_levels$semestre) == 17L, "PASS", "FAIL"),
    ifelse(sum(main20$valid_rent) == 885L, "PASS", "FAIL"),
    ifelse(sum(main20$valid_sale) == 944L, "PASS", "FAIL"),
    ifelse(sum(main20_2018h1$valid_rent) == 0L, "PASS", "FAIL"),
    ifelse(
      all(
        main20_2018h1$prev_superficie_loc == "N" &
          main20_2018h1$superficie_loc == "L"
      ),
      "PASS", "FAIL"
    ),
    ifelse(nrow(type20_summary) == 15L, "PASS", "FAIL"),
    ifelse(
      sum(
        main20$semestre == "2019-2" &
          main20$valid_rent
      ) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      sum(
        main20$semestre == "2023-2" &
          main20$valid_rent
      ) == 59L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(
        segment_summary |>
          filter(
            semestre == "2019-2",
            segmento == "Residential"
          )
      ) == 1L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(
        segment_summary |>
          filter(
            semestre == "2019-2",
            segmento == "Nonresidential"
          )
      ) == 1L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(
        segment_summary |>
          filter(
            semestre == "2023-2",
            segmento == "Residential"
          )
      ) == 1L,
      "PASS", "FAIL"
    ),
    ifelse(
      nrow(
        segment_summary |>
          filter(
            semestre == "2023-2",
            segmento == "Nonresidential"
          )
      ) == 1L,
      "PASS", "FAIL"
    )
  ),
  stringsAsFactors = FALSE
)

write_csv_04(
  audit,
  "40_omi_measurement_audit.csv"
)

failed <- audit |>
  filter(esito == "FAIL")

if (nrow(failed) > 0L) {
  message("")
  message("==============================================================")
  message("AUDIT 04 - CONTROLLI FALLITI")
  message("==============================================================")
  for (i in seq_len(nrow(failed))) {
    message(
      "- ", failed$controllo[i],
      " | valore = ", failed$valore[i]
    )
  }
  stop(
    paste0(
      "Audit 04 fallito in ",
      nrow(failed),
      " controllo/i."
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 14. Figures
# ------------------------------------------------------------------------------

# Figure 15 - Type20 rent vs sale median change.
fig15_data <- type20_summary |>
  select(
    semestre,
    period_index,
    median_pct_rent,
    median_pct_sale
  ) |>
  pivot_longer(
    cols = c(
      median_pct_rent,
      median_pct_sale
    ),
    names_to = "market",
    values_to = "median_change"
  ) |>
  mutate(
    market = recode(
      market,
      median_pct_rent = "Residential rent quotation",
      median_pct_sale = "Residential sale quotation"
    )
  )

fig15 <- ggplot(
  fig15_data,
  aes(
    x = period_index,
    y = median_change,
    linetype = market,
    shape = market,
    group = market
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = type20_summary$period_index,
    labels = type20_summary$semestre
  ) +
  labs(
    title = "OMI Type 20: median semiannual quotation changes",
    subtitle = "Balanced 59-zone panel; invalid rent surface transition 2018H1 excluded",
    x = NULL,
    y = "Median change (%)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig15_omi_type20_rent_sale_changes.png"
  ),
  fig15,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 16 - share of zones changed.
fig16_data <- type20_summary |>
  select(
    semestre,
    period_index,
    share_changed_rent,
    share_changed_sale
  ) |>
  pivot_longer(
    cols = c(
      share_changed_rent,
      share_changed_sale
    ),
    names_to = "market",
    values_to = "share_changed"
  ) |>
  mutate(
    market = recode(
      market,
      share_changed_rent = "Rent",
      share_changed_sale = "Sale"
    )
  )

fig16 <- ggplot(
  fig16_data,
  aes(
    x = period_index,
    y = share_changed,
    linetype = market,
    shape = market,
    group = market
  )
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = type20_summary$period_index,
    labels = type20_summary$semestre
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  labs(
    title = "How often do OMI Type 20 quotations change?",
    subtitle = "Share of the 59 balanced zones with a non-zero semiannual update",
    x = NULL,
    y = "Share of zones changed",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig16_omi_type20_share_changed.png"
  ),
  fig16,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 17 - Residential vs nonresidential median rent change.
fig17_data <- segment_summary |>
  filter(
    n_valid_rent > 0
  )

fig17 <- ggplot(
  fig17_data,
  aes(
    x = period_index,
    y = median_pct_rent,
    linetype = segmento,
    shape = segmento,
    group = segmento
  )
) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = sort(unique(fig17_data$period_index)),
    labels = fig17_data |>
      distinct(period_index, semestre) |>
      arrange(period_index) |>
      pull(semestre)
  ) +
  labs(
    title = "OMI rent updates: residential versus nonresidential property types",
    subtitle = "Median matched zone-type change by semester",
    x = NULL,
    y = "Median rent-quotation change (%)",
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig17_omi_residential_nonresidential.png"
  ),
  fig17,
  width = 10,
  height = 6,
  dpi = 300
)

# Figure 18 - by type in 2019H2 and 2023H2.
fig18_data <- type_summary |>
  filter(
    semestre %in% c("2019-2", "2023-2"),
    n_valid_rent >= 5
  ) |>
  mutate(
    descr_tipologia = factor(
      descr_tipologia,
      levels = rev(
        unique(
          descr_tipologia[
            order(
              segmento,
              cod_tip
            )
          ]
        )
      )
    )
  )

fig18 <- ggplot(
  fig18_data,
  aes(
    x = median_pct_rent,
    y = descr_tipologia,
    shape = semestre
  )
) +
  geom_vline(
    xintercept = 0,
    linewidth = 0.4
  ) +
  geom_point(
    size = 2.4,
    position = position_dodge(
      width = 0.5
    )
  ) +
  labs(
    title = "Which OMI property types moved in 2019H2 and 2023H2?",
    subtitle = "Median rent-quotation change by property type; types with at least 5 matched observations",
    x = "Median change (%)",
    y = NULL,
    shape = "Semester"
  ) +
  theme_minimal(base_size = 10)

ggsave(
  file.path(
    OUTPUT_DIR,
    "fig18_omi_2019_2023_property_types.png"
  ),
  fig18,
  width = 10,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 15. README
# ------------------------------------------------------------------------------
readme <- c(
  "V10 - 04 OMI MEASUREMENT VALIDATION",
  "====================================",
  "",
  "PURPOSE",
  "This script evaluates whether the 2019H2 and 2023H2 episodes look like",
  "general OMI database revisions or residential-rental-specific valuation updates.",
  "",
  "SOURCE",
  "Raw OMI semester ZIPs, 2016H1-2024H1 only. 2024H2 is excluded.",
  "",
  "STATE",
  "All-property-type comparisons use Stato = NORMALE to preserve one",
  "zone-type observation and avoid comparing different quality states.",
  "",
  "SURFACE CONVENTION",
  "A first difference is valid only when Sup_NL is unchanged across adjacent semesters.",
  "For Type 20 rents the N-to-L change in 2018H1 is therefore excluded automatically.",
  "",
  "MAIN TYPE 20 SAMPLE",
  "59 balanced OMI zones x 17 semesters = 1003 level observations.",
  "Valid rent differences = 885.",
  "Valid sale differences = 944.",
  "",
  "RESIDENTIAL CODES",
  "1 Ville e Villini; 19 Abitazioni signorili;",
  "20 Abitazioni civili; 21 Abitazioni di tipo economico.",
  "",
  "DISCRETE UPDATE DIAGNOSTICS",
  "Near +10% means within +/-1 percentage point of +10%.",
  "Exact +10% means within +/-0.1 percentage point.",
  "These diagnostics reveal lumpiness but do not by themselves establish",
  "an administrative revision.",
  "",
  "INTERPRETATION RULE",
  "If large updates are concentrated in residential rents while sale quotations",
  "and nonresidential rents remain broadly flat, a universal OMI database revision",
  "is less plausible. However, residential-rent-specific appraisal updating can",
  "still not be ruled out internally.",
  "",
  "LANGUAGE",
  "Until external validation is combined with this audit, prefer:",
  "'OMI residential rental quotations/valuations' and 'OMI rental repricing'",
  "rather than treating the figures as observed contract rents.",
  "",
  "NO CAUSAL SELECTION",
  "This script contains no outcome-based model selection and does not alter",
  "the frozen RdC exposure or the main models from 03."
)

writeLines(
  readme,
  file.path(
    OUTPUT_DIR,
    "README_04_OMI_MEASUREMENT.txt"
  ),
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 16. Console summary
# ------------------------------------------------------------------------------
message("")
message("==============================================================")
message("04_omi_measurement_validation.R COMPLETATO")
message("==============================================================")
message("OMI semesters: ", nrow(zip_index), " (2016H1-2024H1)")
message("Normal-state all-type rows: ", nrow(all_types))
message("Type20 balanced levels: ", nrow(main20_levels))
message("Type20 valid rent differences: ", sum(main20$valid_rent))
message("Type20 valid sale differences: ", sum(main20$valid_sale))
message("")
message(
  "2019H2 Type20 median rent change: ",
  sprintf("%.3f%%", t20_19$median_pct_rent),
  " | sale: ",
  sprintf("%.3f%%", t20_19$median_pct_sale)
)
message(
  "2019H2 Type20 share unchanged: ",
  sprintf("%.1f%%", 100 * t20_19$share_flat_rent),
  " | near +10%%: ",
  sprintf("%.1f%%", 100 * t20_19$share_near_plus10_rent)
)
message(
  "2019H2 all-residential median rent: ",
  sprintf("%.3f%%", res_19$median_pct_rent),
  " | nonresidential: ",
  sprintf("%.3f%%", nonres_19$median_pct_rent)
)
message("")
message(
  "2023H2 Type20 median rent change: ",
  sprintf("%.3f%%", t20_23$median_pct_rent),
  " | sale: ",
  sprintf("%.3f%%", t20_23$median_pct_sale)
)
message(
  "2023H2 Type20 share unchanged: ",
  sprintf("%.1f%%", 100 * t20_23$share_flat_rent),
  " | near +10%%: ",
  sprintf("%.1f%%", 100 * t20_23$share_near_plus10_rent)
)
message(
  "2023H2 all-residential median rent: ",
  sprintf("%.3f%%", res_23$median_pct_rent),
  " | nonresidential: ",
  sprintf("%.3f%%", nonres_23$median_pct_rent)
)
message("")
message(
  "Type20 median-rent rank: 2019H2 = ",
  rank_t20_19$rank_median_rent_desc,
  " / ", nrow(type20_exceptionality),
  "; 2023H2 = ",
  rank_t20_23$rank_median_rent_desc,
  " / ", nrow(type20_exceptionality)
)
message(
  "All-residential median-rent rank: 2019H2 = ",
  rank_res_19$rank_median_rent_desc,
  " / ", nrow(segment_exceptionality),
  "; 2023H2 = ",
  rank_res_23$rank_median_rent_desc,
  " / ", nrow(segment_exceptionality)
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
  "Interpretare questi risultati come audit delle QUOTAZIONI OMI, ",
  "non come osservazioni di contratti individuali."
)
