# ==============================================================================
# 07_rdc_exposure_external_validation.R
# V10 - EXTERNAL VALIDATION OF PRE-POLICY RdC EXPOSURE
# Naples: OMI-zone exposure (2011) vs observed RdC incidence by Municipality
# VERSIONE: V2_FINAL - gestisce sezioni OMI non assegnate senza stop; P1 rimosso
# ==============================================================================
#
# OBIETTIVO
# ---------
# Validare esternamente rdc_exposure_main, costruita nel 02 esclusivamente con
# informazioni pre-policy del 2011, verificando se le aree di Napoli con maggiore
# esposizione predetta mostrano anche maggiore incidenza osservata del RdC.
#
# QUESTA E' UNA VALIDAZIONE DESCRITTIVA, NON UN TEST CAUSALE.
# - Non modifica 00/02/03/04/05/06.
# - Non ricostruisce e non ri-stima rdc_exposure_main.
# - Non usa outcome OMI nella validazione.
# - I conteggi RdC sono dati amministrativi comunali osservati ex post.
#
# FONTI
# -----
# 1) output_v10/12_rdc_exposure_zone_59.csv
#    Main exposure sulle 59 zone del pannello bilanciato.
# 2) output_v10/12b_rdc_exposure_zone_60.csv
#    Stessa exposure applicata alle 60 zone; la standardizzazione resta fissata
#    sulle 59 zone del pannello, come definito nel 02.
# 3) output_v10/zones_omi_rdc_exposure.gpkg
#    Geometrie OMI congelate dal 02.
# 4) input/istat_recent/Comuni_2021.zip
#    Contiene Napoli_indicatori_2021_sezioni.xlsx:
#      SEZ21_ID = sezione di censimento 2021
#      COM_ASC1 = sub-area amministrativa di primo livello (10 Municipalita')
#      PF1      = famiglie residenti totali
# 5) input/istat_recent/R15_21.zip
#    Geometrie delle sezioni di censimento 2021 della Campania.
# 6) PAL Comune di Napoli, Annualita' 2020, conservato sotto input/validation/
#    (o input/rdc_validation/). I 10 conteggi sono trascritti esplicitamente qui
#    sotto per rendere il codice trasparente e non dipendere dal parsing del PDF.
#
# MISURE
# ------
# Incidenza RdC osservata nella Municipalita' m:
#   rdc_incidence_m = nuclei_RdC_m / famiglie_residenti_2021_m
#
# Exposure municipale predetta:
#   predicted_exposure_m =
#     sum_s(PF1_s * rdc_exposure_main_OMI(s)) / sum_s(PF1_s)
# dove s indica le sezioni ISTAT 2021 della Municipalita' m coperte dalle zone OMI.
# Le eventuali sezioni esterne alla copertura OMI non vengono imputate alla zona
# piu' vicina: vengono escluse solo dall'aggregazione dell'exposure e documentate.
#
# MAIN VALIDATION
# ---------------
# Usa le 60 zone OMI, perche' il numeratore/denominatore RdC copre l'intera citta'.
# Questo NON ridefinisce l'indice: 12b usa la stessa scala standardizzata sulle
# 59 zone del panel. La versione 59-zone e' salvata come sensitivity check.
#
# OUTPUT NUOVI in output_v10/
# ---------------------------
# 70_rdc_validation_municipality.csv
# 71_rdc_validation_correlations.csv
# 72_rdc_validation_coverage_audit.csv
# 73_rdc_validation_leave_one_out.csv
# 74_rdc_validation_unassigned_sections.csv
# fig22_rdc_exposure_vs_actual_incidence.png
# README_07_RDC_EXTERNAL_VALIDATION.txt
#
# ============================================================================== 

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Pacchetti e percorsi
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "stringr", "sf", "readxl", "ggplot2")
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
  library(stringr)
  library(sf)
  library(readxl)
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
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

assert_07 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

write_csv_07 <- function(x, filename) {
  utils::write.csv(
    x,
    file.path(OUTPUT_DIR, filename),
    row.names = FALSE,
    na = ""
  )
}

id_character_07 <- function(x) {
  if (is.numeric(x)) {
    out <- format(x, scientific = FALSE, trim = TRUE, digits = 22)
  } else {
    out <- as.character(x)
  }
  out <- stringr::str_trim(out)
  out <- sub("\\.0+$", "", out)
  out
}

safe_ratio_07 <- function(num, den) {
  ifelse(is.finite(num) & is.finite(den) & den > 0, num / den, NA_real_)
}

weighted_mean_07 <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

find_existing_07 <- function(filename, extra_dirs = character(), allow_copy_suffix = FALSE) {
  search_dirs <- unique(c(INPUT_DIR, PROJECT_DIR, OUTPUT_DIR, extra_dirs))
  search_dirs <- search_dirs[dir.exists(search_dirs)]
  candidates <- unique(file.path(search_dirs, filename))
  hit <- candidates[file.exists(candidates)]

  # Fallback controllato per nomi creati da Windows/browser, es. Comuni_2021(1).zip.
  # Viene usato solo se il nome canonico non esiste e se la variante e' univoca.
  if (!length(hit) && allow_copy_suffix) {
    ext <- tools::file_ext(filename)
    stem <- sub(paste0("\\.", ext, "$"), "", filename)
    pattern <- paste0(
      "^", stem, "(\\([0-9]+\\))?\\.", ext, "$"
    )
    fallback <- unique(unlist(lapply(search_dirs, function(d) {
      list.files(d, pattern = pattern, full.names = TRUE, ignore.case = FALSE)
    })))
    assert_07(
      length(fallback) <= 1L,
      paste0(
        "Piu' copie candidate trovate per ", filename, ":\n- ",
        paste(fallback, collapse = "\n- "),
        "\nLasciare una sola copia o rinominare quella canonica."
      )
    )
    hit <- fallback
  }

  assert_07(
    length(hit) >= 1L,
    paste0(
      "File richiesto non trovato: ", filename,
      "\nDirectory controllate:\n- ", paste(search_dirs, collapse = "\n- ")
    )
  )
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

ISTAT_RECENT_DIR <- file.path(INPUT_DIR, "istat_recent")
VALIDATION_DIRS <- c(
  file.path(INPUT_DIR, "validation"),
  file.path(INPUT_DIR, "rdc_validation")
)

COMUNI21_PATH <- find_existing_07(
  "Comuni_2021.zip",
  extra_dirs = ISTAT_RECENT_DIR,
  allow_copy_suffix = TRUE
)
R15_21_PATH <- find_existing_07(
  "R15_21.zip",
  extra_dirs = ISTAT_RECENT_DIR,
  allow_copy_suffix = TRUE
)

EXPOSURE59_PATH <- file.path(OUTPUT_DIR, "12_rdc_exposure_zone_59.csv")
EXPOSURE60_PATH <- file.path(OUTPUT_DIR, "12b_rdc_exposure_zone_60.csv")
OMI_GPKG_PATH <- file.path(OUTPUT_DIR, "zones_omi_rdc_exposure.gpkg")

for (p in c(EXPOSURE59_PATH, EXPOSURE60_PATH, OMI_GPKG_PATH)) {
  assert_07(file.exists(p), paste0("Input canonico mancante: ", p))
}

# Il PAL serve come documentazione della fonte. Il PDF NON viene parsato.
pal_candidates <- unique(unlist(lapply(
  VALIDATION_DIRS[file.exists(VALIDATION_DIRS)],
  function(d) list.files(
    d,
    pattern = "(PAL|Piano.*Attuazione.*Locale).*\\.pdf$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
)))

assert_07(
  length(pal_candidates) >= 1L,
  paste0(
    "PDF PAL non trovato sotto input/validation o input/rdc_validation. ",
    "Il file e' richiesto come documentazione della fonte."
  )
)
PAL_PATH <- normalizePath(pal_candidates[1], winslash = "/", mustWork = TRUE)

# ------------------------------------------------------------------------------
# 1. Exposure congelata dal 02: audit 59 vs 60
# ------------------------------------------------------------------------------
exposure59 <- utils::read.csv(
  EXPOSURE59_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)
exposure60 <- utils::read.csv(
  EXPOSURE60_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

required_exposure <- c("zona", "fascia", "rdc_exposure_main")
assert_07(all(required_exposure %in% names(exposure59)),
          "12_rdc_exposure_zone_59.csv: colonne richieste mancanti.")
assert_07(all(required_exposure %in% names(exposure60)),
          "12b_rdc_exposure_zone_60.csv: colonne richieste mancanti.")
assert_07(nrow(exposure59) == 59L && !anyDuplicated(exposure59$zona),
          "Exposure 59 inattesa: attese 59 zone univoche.")
assert_07(nrow(exposure60) == 60L && !anyDuplicated(exposure60$zona),
          "Exposure 60 inattesa: attese 60 zone univoche.")

exposure59 <- exposure59 |>
  transmute(
    zona = as.character(zona),
    fascia = as.character(fascia),
    rdc_exposure_main = as.numeric(rdc_exposure_main)
  )

exposure60 <- exposure60 |>
  transmute(
    zona = as.character(zona),
    fascia = as.character(fascia),
    rdc_exposure_main = as.numeric(rdc_exposure_main)
  )

assert_07(all(is.finite(exposure59$rdc_exposure_main)),
          "Exposure main 59 contiene valori non finiti.")
assert_07(all(is.finite(exposure60$rdc_exposure_main)),
          "Exposure main 60 contiene valori non finiti.")

same59 <- exposure59 |>
  select(zona, exposure59 = rdc_exposure_main) |>
  inner_join(
    exposure60 |> select(zona, exposure60 = rdc_exposure_main),
    by = "zona"
  )

assert_07(nrow(same59) == 59L,
          "Le 59 zone main non sono tutte presenti nel file 60-zone.")
assert_07(
  max(abs(same59$exposure59 - same59$exposure60)) < 1e-12,
  "Exposure 59 e 60 non coincidono sulle 59 zone comuni. Fermarsi e verificare il 02."
)

# ------------------------------------------------------------------------------
# 2. ISTAT 2021: SEZ21_ID, COM_ASC1, PF1
# ------------------------------------------------------------------------------
zip_index_21 <- utils::unzip(COMUNI21_PATH, list = TRUE)
member21 <- zip_index_21$Name[
  basename(zip_index_21$Name) == "Napoli_indicatori_2021_sezioni.xlsx"
]
assert_07(
  length(member21) == 1L,
  "Comuni_2021.zip non contiene univocamente Napoli_indicatori_2021_sezioni.xlsx."
)

tmp21 <- tempfile("rdc07_istat21_")
dir.create(tmp21, recursive = TRUE, showWarnings = FALSE)
utils::unzip(COMUNI21_PATH, files = member21, exdir = tmp21, overwrite = TRUE)
xlsx21 <- file.path(tmp21, member21)
assert_07(file.exists(xlsx21), "Impossibile estrarre il file ISTAT Napoli 2021.")

napoli21_raw <- suppressMessages(readxl::read_excel(
  xlsx21,
  sheet = "Napoli_2021"
))

required_istat21 <- c("SEZ21_ID", "COM_ASC1", "PF1")
assert_07(
  all(required_istat21 %in% names(napoli21_raw)),
  paste0(
    "Napoli_2021: colonne mancanti: ",
    paste(setdiff(required_istat21, names(napoli21_raw)), collapse = ", ")
  )
)

napoli21 <- napoli21_raw |>
  transmute(
    SEZ21_ID = id_character_07(SEZ21_ID),
    COM_ASC1 = id_character_07(COM_ASC1),
    PF1 = as.numeric(PF1)
  )

expected_municipality_codes <- sprintf("630490%02d", 1:10)
assert_07(
  setequal(sort(unique(napoli21$COM_ASC1)), expected_municipality_codes),
  paste0(
    "COM_ASC1 non identifica esattamente le 10 Municipalita' attese. Valori trovati: ",
    paste(sort(unique(napoli21$COM_ASC1)), collapse = ", ")
  )
)
assert_07(!anyDuplicated(napoli21$SEZ21_ID),
          "SEZ21_ID duplicato nel file ISTAT 2021.")
assert_07(all(is.finite(napoli21$PF1) & napoli21$PF1 >= 0),
          "PF1 contiene valori mancanti, negativi o non finiti.")
assert_07(nrow(napoli21) == 4272L,
          paste0("Numero sezioni Napoli 2021 inatteso: ", nrow(napoli21), " (atteso 4272)."))
assert_07(sum(napoli21$PF1) == 374555,
          paste0("Totale famiglie PF1 inatteso: ", sum(napoli21$PF1), " (atteso 374555)."))

napoli21 <- napoli21 |>
  mutate(
    municipalita = match(COM_ASC1, expected_municipality_codes),
    municipalita_roman = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X")[municipalita]
  )

municipality_denominators <- napoli21 |>
  group_by(municipalita, municipalita_roman, COM_ASC1) |>
  summarise(
    famiglie_2021 = sum(PF1),
    n_sezioni_istat = n_distinct(SEZ21_ID),
    .groups = "drop"
  ) |>
  arrange(municipalita)

assert_07(nrow(municipality_denominators) == 10L,
          "Aggregazione ISTAT non produce 10 Municipalita'.")

# ------------------------------------------------------------------------------
# 3. PAL: nuclei familiari RdC osservati per Municipalita'
# ------------------------------------------------------------------------------
# Fonte: Comune di Napoli, Piano di Attuazione Locale (PAL), Annualita' 2020.
# Tabella: nuclei familiari beneficiari RdC suddivisi per Municipalita'.
# Periodo richiamato nel documento: ottobre 2020 - luglio 2021.
rdc_actual <- data.frame(
  municipalita = 1:10,
  municipalita_roman = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"),
  nuclei_rdc = c(1917, 5846, 5291, 5261, 1085, 5132, 4756, 4680, 3672, 2409),
  stringsAsFactors = FALSE
)

assert_07(sum(rdc_actual$nuclei_rdc) == 40049L,
          "I conteggi PAL trascritti non sommano a 40,049.")

actual_incidence <- municipality_denominators |>
  inner_join(rdc_actual, by = c("municipalita", "municipalita_roman")) |>
  mutate(
    rdc_incidence = safe_ratio_07(nuclei_rdc, famiglie_2021),
    rdc_incidence_pct = 100 * rdc_incidence
  )

assert_07(nrow(actual_incidence) == 10L && all(is.finite(actual_incidence$rdc_incidence)),
          "Incidenza RdC non costruita correttamente per tutte le Municipalita'.")

# ------------------------------------------------------------------------------
# 4. Geometrie sezioni 2021 e assegnazione robusta alle zone OMI
# ------------------------------------------------------------------------------
shape_dir <- tempfile("rdc07_shape21_")
dir.create(shape_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(R15_21_PATH, exdir = shape_dir, overwrite = TRUE)

shp21 <- list.files(
  shape_dir,
  pattern = "R15_21_WGS84\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
assert_07(
  length(shp21) == 1L,
  paste0("R15_21.zip: atteso un solo R15_21_WGS84.shp; trovati ", length(shp21), ".")
)

sections_all <- suppressMessages(sf::st_read(
  shp21,
  quiet = TRUE,
  stringsAsFactors = FALSE
))
assert_07("SEZ21_ID" %in% names(sections_all),
          "Shapefile 2021 privo di SEZ21_ID.")
assert_07(!is.na(sf::st_crs(sections_all)),
          "CRS mancante nello shapefile sezioni 2021.")

sections <- sections_all |>
  mutate(SEZ21_ID = id_character_07(SEZ21_ID)) |>
  select(SEZ21_ID) |>
  inner_join(napoli21, by = "SEZ21_ID")

assert_07(nrow(sections) == nrow(napoli21),
          paste0(
            "Match geometrie-dati ISTAT incompleto: ", nrow(sections),
            " sezioni con geometria su ", nrow(napoli21), "."
          ))
assert_07(!anyDuplicated(sections$SEZ21_ID),
          "Sezioni duplicate dopo il join dati-geometrie.")

if (any(!sf::st_is_valid(sections))) {
  sections <- suppressWarnings(sf::st_make_valid(sections))
}

omi_sf <- suppressMessages(sf::st_read(
  OMI_GPKG_PATH,
  quiet = TRUE,
  stringsAsFactors = FALSE
)) |>
  mutate(zona = as.character(zona)) |>
  select(zona)

assert_07(nrow(omi_sf) == 60L && !anyDuplicated(omi_sf$zona),
          "GPKG OMI deve contenere 60 zone univoche.")
assert_07(!is.na(sf::st_crs(omi_sf)), "CRS mancante nel GPKG OMI.")

if (any(!sf::st_is_valid(omi_sf))) {
  omi_sf <- suppressWarnings(sf::st_make_valid(omi_sf))
}

# Lavoriamo in EPSG:32633 per distanze/aree metriche.
sections_metric <- sf::st_transform(sections, 32633)
omi_metric <- sf::st_transform(omi_sf, 32633)

# Prima assegnazione: punto interno della sezione -> zona OMI.
section_points <- suppressWarnings(sf::st_point_on_surface(sections_metric))
assigned_raw <- suppressWarnings(sf::st_join(
  section_points |> select(SEZ21_ID),
  omi_metric |> select(zona),
  join = sf::st_intersects,
  left = TRUE
)) |>
  sf::st_drop_geometry()

# Se una sezione e' su un confine (piu' zone) o resta senza zona, scegliamo la
# zona con maggiore area di sovrapposizione della sezione. Non si usa il valore
# dell'exposure per risolvere l'ambiguita'.
assignment_counts <- assigned_raw |>
  group_by(SEZ21_ID) |>
  summarise(
    n_zone = sum(!is.na(zona)),
    .groups = "drop"
  )
problem_ids <- assignment_counts |>
  filter(n_zone != 1L) |>
  pull(SEZ21_ID)

assigned_unique <- assigned_raw |>
  filter(!SEZ21_ID %in% problem_ids, !is.na(zona)) |>
  select(SEZ21_ID, zona)

if (length(problem_ids)) {
  overlaps <- suppressWarnings(sf::st_intersection(
    sections_metric |>
      filter(SEZ21_ID %in% problem_ids) |>
      select(SEZ21_ID),
    omi_metric |> select(zona)
  )) |>
    mutate(overlap_m2 = as.numeric(sf::st_area(geometry))) |>
    sf::st_drop_geometry() |>
    filter(is.finite(overlap_m2) & overlap_m2 > 0) |>
    arrange(SEZ21_ID, desc(overlap_m2), zona)

  chosen_problem <- overlaps |>
    group_by(SEZ21_ID) |>
    slice(1L) |>
    ungroup() |>
    select(SEZ21_ID, zona)

  unresolved_ids <- setdiff(problem_ids, chosen_problem$SEZ21_ID)

  # IMPORTANTE: le zone OMI non sono obbligate a coprire il 100% della superficie
  # comunale. Il 02 originale accetta infatti un overlay se copre almeno il 99%
  # di popolazione e famiglie, senza forzare artificialmente le sezioni esterne
  # dentro la zona OMI piu' vicina. Manteniamo qui la stessa regola conservativa.
  section_to_omi <- bind_rows(assigned_unique, chosen_problem)
} else {
  unresolved_ids <- character()
  section_to_omi <- assigned_unique
}

assert_07(!anyDuplicated(section_to_omi$SEZ21_ID),
          "SEZ21_ID duplicato nel mapping finale sezione -> OMI.")
assert_07(all(section_to_omi$zona %in% exposure60$zona),
          "Il mapping contiene zone OMI non presenti nell'exposure 60-zone.")

# Audit trasparente delle eventuali sezioni fuori dalla copertura OMI.
unassigned_sections <- napoli21 |>
  filter(SEZ21_ID %in% unresolved_ids) |>
  transmute(
    SEZ21_ID,
    COM_ASC1,
    municipalita,
    municipalita_roman,
    PF1,
    motivo = "Nessuna intersezione geometrica con le 60 zone OMI"
  ) |>
  arrange(municipalita, SEZ21_ID)

write_csv_07(unassigned_sections, "74_rdc_validation_unassigned_sections.csv")

section_validation <- napoli21 |>
  left_join(section_to_omi, by = "SEZ21_ID") |>
  left_join(
    exposure60 |>
      select(zona, rdc_exposure_main) |>
      rename(rdc_exposure_60 = rdc_exposure_main),
    by = "zona"
  ) |>
  left_join(
    exposure59 |>
      select(zona, rdc_exposure_main) |>
      rename(rdc_exposure_59 = rdc_exposure_main),
    by = "zona"
  )

# Non imponiamo exposure finita per il 100% delle sezioni: alcune sezioni possono
# essere legittimamente esterne alla copertura OMI. La copertura viene verificata
# formalmente sotto, in totale e per ciascuna Municipalita'.

# ------------------------------------------------------------------------------
# 5. Aggregazione PF1-weighted alle 10 Municipalita'
# ------------------------------------------------------------------------------
predicted_municipality <- section_validation |>
  group_by(municipalita, municipalita_roman, COM_ASC1) |>
  summarise(
    predicted_exposure_60 = weighted_mean_07(rdc_exposure_60, PF1),
    predicted_exposure_59 = weighted_mean_07(rdc_exposure_59, PF1),
    famiglie_mappate_60 = sum(PF1[is.finite(rdc_exposure_60)], na.rm = TRUE),
    famiglie_mappate_59 = sum(PF1[is.finite(rdc_exposure_59)], na.rm = TRUE),
    n_sezioni_mappate_60 = n_distinct(SEZ21_ID[is.finite(rdc_exposure_60)]),
    n_sezioni_mappate_59 = n_distinct(SEZ21_ID[is.finite(rdc_exposure_59)]),
    .groups = "drop"
  )

validation_municipality <- actual_incidence |>
  left_join(
    predicted_municipality,
    by = c("municipalita", "municipalita_roman", "COM_ASC1")
  ) |>
  mutate(
    coverage_famiglie_60 = safe_ratio_07(famiglie_mappate_60, famiglie_2021),
    coverage_famiglie_59 = safe_ratio_07(famiglie_mappate_59, famiglie_2021),
    rank_actual_incidence = rank(-rdc_incidence, ties.method = "average"),
    rank_predicted_60 = rank(-predicted_exposure_60, ties.method = "average"),
    rank_predicted_59 = rank(-predicted_exposure_59, ties.method = "average")
  ) |>
  arrange(municipalita)

assert_07(nrow(validation_municipality) == 10L,
          "Tabella finale non contiene 10 Municipalita'.")
assert_07(all(is.finite(validation_municipality$predicted_exposure_60)),
          "Predicted exposure 60-zone mancante per almeno una Municipalita'.")
assert_07(all(is.finite(validation_municipality$predicted_exposure_59)),
          "Predicted exposure 59-zone mancante per almeno una Municipalita'.")
assert_07(min(validation_municipality$coverage_famiglie_60) >= 0.99,
          paste0(
            "Copertura famiglie 60-zone inferiore al 99% in almeno una Municipalita': min=",
            round(min(validation_municipality$coverage_famiglie_60), 4)
          ))

write_csv_07(validation_municipality, "70_rdc_validation_municipality.csv")

# ------------------------------------------------------------------------------
# 6. Pearson / Spearman: main 60-zone + sensitivity 59-zone
# ------------------------------------------------------------------------------
cor_test_row_07 <- function(data, xvar, method, specification) {
  x <- data[[xvar]]
  y <- data$rdc_incidence
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]

  assert_07(length(x) == 10L,
            paste0("Correlazione ", specification, ": attese 10 osservazioni."))

  if (method == "pearson") {
    test <- stats::cor.test(x, y, method = "pearson")
    ci_low <- unname(test$conf.int[1])
    ci_high <- unname(test$conf.int[2])
  } else if (method == "spearman") {
    # Con N=10 il p-value e' solo descrittivo; exact=FALSE evita fragilita' in
    # presenza di eventuali ties derivanti dalle aggregazioni territoriali.
    test <- suppressWarnings(stats::cor.test(x, y, method = "spearman", exact = FALSE))
    ci_low <- NA_real_
    ci_high <- NA_real_
  } else {
    stop("Metodo di correlazione non supportato.", call. = FALSE)
  }

  data.frame(
    specifica = specification,
    metodo = method,
    n_municipalita = length(x),
    correlazione = unname(test$estimate),
    statistica = unname(test$statistic),
    p_value = test$p.value,
    ci95_low = ci_low,
    ci95_high = ci_high,
    stringsAsFactors = FALSE
  )
}

correlations <- bind_rows(
  cor_test_row_07(validation_municipality, "predicted_exposure_60", "pearson", "MAIN_60_zone_city_coverage"),
  cor_test_row_07(validation_municipality, "predicted_exposure_60", "spearman", "MAIN_60_zone_city_coverage"),
  cor_test_row_07(validation_municipality, "predicted_exposure_59", "pearson", "SENSITIVITY_59_balanced_zones"),
  cor_test_row_07(validation_municipality, "predicted_exposure_59", "spearman", "SENSITIVITY_59_balanced_zones")
)

write_csv_07(correlations, "71_rdc_validation_correlations.csv")

# ------------------------------------------------------------------------------
# 7. Leave-one-Municipality-out: diagnostica di influenza con N=10
# ------------------------------------------------------------------------------
loo_rows <- lapply(seq_len(nrow(validation_municipality)), function(i) {
  d <- validation_municipality[-i, , drop = FALSE]
  omitted <- validation_municipality[i, , drop = FALSE]

  bind_rows(
    data.frame(
      specifica = "MAIN_60_zone_city_coverage",
      municipalita_esclusa = omitted$municipalita,
      municipalita_esclusa_roman = omitted$municipalita_roman,
      metodo = "pearson",
      correlazione = stats::cor(d$predicted_exposure_60, d$rdc_incidence, method = "pearson"),
      n = nrow(d),
      stringsAsFactors = FALSE
    ),
    data.frame(
      specifica = "MAIN_60_zone_city_coverage",
      municipalita_esclusa = omitted$municipalita,
      municipalita_esclusa_roman = omitted$municipalita_roman,
      metodo = "spearman",
      correlazione = stats::cor(d$predicted_exposure_60, d$rdc_incidence, method = "spearman"),
      n = nrow(d),
      stringsAsFactors = FALSE
    ),
    data.frame(
      specifica = "SENSITIVITY_59_balanced_zones",
      municipalita_esclusa = omitted$municipalita,
      municipalita_esclusa_roman = omitted$municipalita_roman,
      metodo = "pearson",
      correlazione = stats::cor(d$predicted_exposure_59, d$rdc_incidence, method = "pearson"),
      n = nrow(d),
      stringsAsFactors = FALSE
    ),
    data.frame(
      specifica = "SENSITIVITY_59_balanced_zones",
      municipalita_esclusa = omitted$municipalita,
      municipalita_esclusa_roman = omitted$municipalita_roman,
      metodo = "spearman",
      correlazione = stats::cor(d$predicted_exposure_59, d$rdc_incidence, method = "spearman"),
      n = nrow(d),
      stringsAsFactors = FALSE
    )
  )
})

leave_one_out <- bind_rows(loo_rows) |>
  arrange(specifica, metodo, municipalita_esclusa)
write_csv_07(leave_one_out, "73_rdc_validation_leave_one_out.csv")

# ------------------------------------------------------------------------------
# 8. Coverage / provenance audit
# ------------------------------------------------------------------------------
city_families <- sum(validation_municipality$famiglie_2021)
city_mapped60 <- sum(validation_municipality$famiglie_mappate_60)
city_mapped59 <- sum(validation_municipality$famiglie_mappate_59)

coverage_audit <- data.frame(
  controllo = c(
    "File PAL trovato",
    "Exposure 59 zone",
    "Exposure 60 zone",
    "Exposure 59=60 sulle zone comuni",
    "Sezioni ISTAT 2021",
    "Municipalita COM_ASC1",
    "Famiglie ISTAT 2021 totali",
    "Nuclei RdC PAL totali",
    "Copertura famiglie 60-zone citta",
    "Copertura famiglie 59-zone citta",
    "Min copertura famiglie 60-zone per Municipalita",
    "Min copertura famiglie 59-zone per Municipalita",
    "Sezioni problematiche risolte via overlap",
    "Sezioni fuori copertura OMI",
    "Famiglie fuori copertura OMI"
  ),
  valore = c(
    basename(PAL_PATH),
    nrow(exposure59),
    nrow(exposure60),
    max(abs(same59$exposure59 - same59$exposure60)),
    nrow(napoli21),
    nrow(municipality_denominators),
    city_families,
    sum(rdc_actual$nuclei_rdc),
    safe_ratio_07(city_mapped60, city_families),
    safe_ratio_07(city_mapped59, city_families),
    min(validation_municipality$coverage_famiglie_60),
    min(validation_municipality$coverage_famiglie_59),
    length(setdiff(problem_ids, unresolved_ids)),
    length(unresolved_ids),
    sum(unassigned_sections$PF1, na.rm = TRUE)
  ),
  esito = c(
    "PASS",
    ifelse(nrow(exposure59) == 59L, "PASS", "FAIL"),
    ifelse(nrow(exposure60) == 60L, "PASS", "FAIL"),
    ifelse(max(abs(same59$exposure59 - same59$exposure60)) < 1e-12, "PASS", "FAIL"),
    ifelse(nrow(napoli21) == 4272L, "PASS", "FAIL"),
    ifelse(nrow(municipality_denominators) == 10L, "PASS", "FAIL"),
    ifelse(city_families == 374555, "PASS", "FAIL"),
    ifelse(sum(rdc_actual$nuclei_rdc) == 40049L, "PASS", "FAIL"),
    ifelse(safe_ratio_07(city_mapped60, city_families) >= 0.99, "PASS", "FAIL"),
    ifelse(safe_ratio_07(city_mapped59, city_families) >= 0.90, "PASS", "WARN"),
    ifelse(min(validation_municipality$coverage_famiglie_60) >= 0.99, "PASS", "FAIL"),
    ifelse(min(validation_municipality$coverage_famiglie_59) >= 0.75, "PASS", "WARN"),
    "INFO",
    ifelse(length(unresolved_ids) == 0L, "PASS", "INFO"),
    ifelse(sum(unassigned_sections$PF1, na.rm = TRUE) / city_families <= 0.01, "PASS", "FAIL")
  ),
  stringsAsFactors = FALSE
)

write_csv_07(coverage_audit, "72_rdc_validation_coverage_audit.csv")
assert_07(!any(coverage_audit$esito == "FAIL"),
          "Almeno un audit della external validation e' FAIL.")

# ------------------------------------------------------------------------------
# 9. Figura main: predicted exposure 60-zone vs actual incidence
# ------------------------------------------------------------------------------
main_pearson <- correlations |>
  filter(
    specifica == "MAIN_60_zone_city_coverage",
    metodo == "pearson"
  )
main_spearman <- correlations |>
  filter(
    specifica == "MAIN_60_zone_city_coverage",
    metodo == "spearman"
  )

fig22 <- ggplot(
  validation_municipality,
  aes(x = predicted_exposure_60, y = rdc_incidence_pct)
) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.6) +
  geom_point(size = 2.6) +
  geom_text(
    aes(label = municipalita_roman),
    nudge_y = 0.35,
    size = 3.2,
    check_overlap = TRUE
  ) +
  labs(
    title = "External validation of pre-policy RdC exposure",
    subtitle = paste0(
      "10 Municipalities; PF1-weighted OMI exposure | Pearson r = ",
      sprintf("%.2f", main_pearson$correlazione),
      ", Spearman rho = ", sprintf("%.2f", main_spearman$correlazione)
    ),
    x = "Predicted RdC exposure (2011, municipality PF1-weighted)",
    y = "Observed RdC households / resident families 2021 (%)",
    caption = paste0(
      "RdC counts: Comune di Napoli PAL 2020 (Oct 2020-Jul 2021). ",
      "Families: ISTAT Census 2021. Descriptive external validation, not causal."
    )
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 7.5, hjust = 0)
  )

ggsave(
  file.path(OUTPUT_DIR, "fig22_rdc_exposure_vs_actual_incidence.png"),
  fig22,
  width = 8.5,
  height = 6.2,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 10. README / interpretazione consentita
# ------------------------------------------------------------------------------
loo_main_p <- leave_one_out |>
  filter(specifica == "MAIN_60_zone_city_coverage", metodo == "pearson") |>
  summarise(lo = min(correlazione), hi = max(correlazione))
loo_main_s <- leave_one_out |>
  filter(specifica == "MAIN_60_zone_city_coverage", metodo == "spearman") |>
  summarise(lo = min(correlazione), hi = max(correlazione))

readme_lines <- c(
  "V10 - EXTERNAL VALIDATION OF PRE-POLICY RdC EXPOSURE",
  "=====================================================",
  "",
  "SCOPO",
  "- Validare rdc_exposure_main usando una misura amministrativa osservata ex post.",
  "- Nessun outcome OMI entra nella validazione.",
  "- Nessuna definizione del 02 viene modificata o ri-stimata.",
  "",
  "DATI",
  "- Exposure: Censimento 2011 / 8milaCensus, gia' costruita nel 02.",
  "- Municipalita': COM_ASC1 nel file ISTAT Napoli 2021.",
  "- Denominatore: PF1 = famiglie residenti, Censimento 2021.",
  "- Numeratore: nuclei familiari RdC per Municipalita', Comune di Napoli PAL 2020.",
  "- Il PAL richiama il periodo ottobre 2020 - luglio 2021 e un totale di 40,049 nuclei.",
  "",
  "SPECIFICA MAIN",
  "- Aggregazione exposure alle Municipalita' ponderata per PF1 delle sezioni 2021 coperte da OMI.",
  "- Le sezioni fuori dalla copertura geometrica OMI non vengono imputate alla zona piu' vicina.",
  "- La copertura main deve essere >=99% delle famiglie in ciascuna Municipalita'.",
  "- Main: 60 zone OMI per massimizzare la copertura della citta'.",
  "- Sensitivity: sole 59 zone del pannello econometrico.",
  "- La 60a zona NON cambia la scala dell'indice: il 02 standardizza usando le 59 zone main.",
  "",
  "RISULTATI MAIN (60 zone)",
  paste0("- Pearson r = ", sprintf("%.4f", main_pearson$correlazione),
         "; p = ", sprintf("%.4f", main_pearson$p_value), "."),
  paste0("- Spearman rho = ", sprintf("%.4f", main_spearman$correlazione),
         "; p = ", sprintf("%.4f", main_spearman$p_value), "."),
  paste0("- Leave-one-out Pearson range = [", sprintf("%.4f", loo_main_p$lo),
         ", ", sprintf("%.4f", loo_main_p$hi), "]."),
  paste0("- Leave-one-out Spearman range = [", sprintf("%.4f", loo_main_s$lo),
         ", ", sprintf("%.4f", loo_main_s$hi), "]."),
  "",
  "INTERPRETAZIONE",
  "- Una correlazione positiva supporta la validita' esterna dell'ordinamento territoriale dell'exposure.",
  "- Con sole 10 Municipalita', magnitudine e p-value devono essere letti con cautela.",
  "- La correlazione NON dimostra che il RdC abbia causato variazioni negli affitti.",
  "- Il PAL misura nuclei RdC amministrativamente osservati/gestiti nel periodo indicato;",
  "  il denominatore PF1 e' 2021 e serve solo a trasformare i conteggi in incidenze comparabili.",
  "",
  "OUTPUT",
  "- 70_rdc_validation_municipality.csv",
  "- 71_rdc_validation_correlations.csv",
  "- 72_rdc_validation_coverage_audit.csv",
  "- 73_rdc_validation_leave_one_out.csv",
  "- 74_rdc_validation_unassigned_sections.csv",
  "- fig22_rdc_exposure_vs_actual_incidence.png"
)

writeLines(
  readme_lines,
  con = file.path(OUTPUT_DIR, "README_07_RDC_EXTERNAL_VALIDATION.txt"),
  useBytes = TRUE
)

# Pulizia dei file temporanei creati dal 07.
unlink(tmp21, recursive = TRUE, force = TRUE)
unlink(shape_dir, recursive = TRUE, force = TRUE)

# ------------------------------------------------------------------------------
# 11. Console summary
# ------------------------------------------------------------------------------
message("07 external validation completata.")
message("PAL documentato: ", PAL_PATH)
message("Famiglie ISTAT 2021: ", format(city_families, big.mark = ","))
message("Nuclei RdC PAL: ", format(sum(rdc_actual$nuclei_rdc), big.mark = ","))
message(
  "Sezioni fuori copertura OMI: ", length(unresolved_ids),
  " | famiglie: ", sum(unassigned_sections$PF1, na.rm = TRUE)
)
message(
  "MAIN 60-zone | Pearson r=", sprintf("%.3f", main_pearson$correlazione),
  " | Spearman rho=", sprintf("%.3f", main_spearman$correlazione)
)
message("Output: ", normalizePath(OUTPUT_DIR, winslash = "/", mustWork = TRUE))
