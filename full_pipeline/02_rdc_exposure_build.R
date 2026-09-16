# ==============================================================================
# 02_rdc_exposure_build.R
# V10 - PRE-POLICY EXPOSURE TO REDDITO DI CITTADINANZA (RdC)
# ==============================================================================
#
# OBIETTIVO
# ---------
# Costruire UNA misura principale di esposizione territoriale al RdC usando
# esclusivamente informazioni del 2011, quindi precedenti alla policy del 2019.
#
# IMPORTANTE:
# - Questo script NON stima alcun effetto sugli affitti.
# - I canoni e i prezzi OMI NON entrano nella costruzione dell'indice.
# - La definizione MAIN viene fissata ex ante e non cambia in funzione dei risultati.
#
# DEFINIZIONE MAIN (59 zone del pannello bilanciato come popolazione di riferimento)
# ------------------------------------------------------------------------------
# 1) renter_share      = quota famiglie in affitto (Censimento 2011, sezione -> OMI)
# 2) low_employment    = 1 - quota occupati 15+ (Censimento 2011, sezione -> OMI)
# 3) economic_distress = V6 8milaCensus 2011:
#    "Incidenza delle famiglie con potenziale disagio economico"
#    V6 è pubblicato a livello ACE; viene riportato alle zone OMI assegnando
#    le sezioni alle zone e ponderando V6 con il numero di famiglie PF1.
#
# Tutte le componenti sono standardizzate sulle 59 zone del pannello bilanciato.
#
#   RdCExposure_main_raw =
#       (z_renter + z_low_employment + z_economic_distress) / 3
#
#   RdCExposure_main = z(RdCExposure_main_raw)
#
# Il /3 è soltanto una media a pesi uguali; dopo la standardizzazione finale
# non cambia l'ordinamento dell'indice.
#
# ROBUSTEZZE COSTRUITE ORA (senza usare outcome OMI)
# --------------------------------------------------
# - RdCExposure_2comp: renter + low employment
# - RdCExposure_Anderson: inverse-covariance weights
# - RdCExposure_PCA1: prima componente principale
# - RdCExposure_NEET: renter + low employment + V8 (robustezza alternativa)
#
# Queste misure NON sostituiscono la definizione MAIN.
#
# INPUT
# -----
# output_v10/master_naples_housing_panel.csv
# output_v10/istat2011_zone_characteristics.csv
# output_v10/zones_omi_istat2011.gpkg
# input/dati-cpa_2011.zip
# input/R15_11_WGS84.zip
# input/subcomunali_063_063049.xlsx
#
# OUTPUT principali in output_v10/
# ---------------------------------
# 12_rdc_exposure_zone_59.csv
# 12b_rdc_exposure_zone_60.csv
# 12c_rdc_exposure_definition.csv
# 12d_rdc_exposure_component_summary.csv
# 12e_rdc_exposure_correlations.csv
# 12f_rdc_exposure_by_band.csv
# 12g_rdc_exposure_weights_diagnostics.csv
# 12h_rdc_ace_to_omi_composition.csv
# 12i_rdc_exposure_top_bottom.csv
# 12j_audit_rdc_exposure.csv
# master_naples_housing_panel_rdc.csv
# zones_omi_rdc_exposure.gpkg
# fig09_rdc_exposure_map.png
# fig10_rdc_components_by_band.png
# README_02_RDC_EXPOSURE.txt
#
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

# ------------------------------------------------------------------------------
# 0. Pacchetti e cartelle
# ------------------------------------------------------------------------------
required_packages <- c("dplyr", "tidyr", "stringr", "sf", "readxl", "ggplot2")
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
PROCESSED_DIR <- file.path(PROJECT_DIR, "data_processed_v10")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

assert_02 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

find_file_02 <- function(filename, extra_dirs = character()) {
  candidates <- unique(c(
    file.path(INPUT_DIR, filename),
    file.path(PROJECT_DIR, filename),
    file.path(OUTPUT_DIR, filename),
    file.path(PROCESSED_DIR, filename),
    unlist(lapply(extra_dirs, function(d) file.path(d, filename)))
  ))
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop(
      "File richiesto non trovato: ", filename,
      "\nPercorsi controllati:\n- ", paste(candidates, collapse = "\n- "),
      call. = FALSE
    )
  }
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

write_csv_02 <- function(x, filename) {
  utils::write.csv(
    x,
    file.path(OUTPUT_DIR, filename),
    row.names = FALSE,
    na = ""
  )
}

safe_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- str_trim(as.character(x))
  x[x %in% c("", "-", "....", "…", "NA", "NaN")] <- NA_character_
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

id_character_02 <- function(x) {
  if (is.numeric(x)) {
    out <- format(x, scientific = FALSE, trim = TRUE, digits = 22)
  } else {
    out <- as.character(x)
  }
  out <- str_trim(out)
  out <- sub("\\.0+$", "", out)
  out
}

zscore_ref_02 <- function(x, ref) {
  mu <- mean(x[ref], na.rm = TRUE)
  ss <- stats::sd(x[ref], na.rm = TRUE)
  assert_02(is.finite(mu) && is.finite(ss) && ss > 0,
            "Standardizzazione impossibile: media/sd non valide.")
  (x - mu) / ss
}

weighted_mean_02 <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

normalize_utf8_02 <- function(x) {
  if (!is.character(x)) return(x)
  converted <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = NA_character_))
  bad <- is.na(converted) & !is.na(x)
  if (any(bad)) {
    converted[bad] <- suppressWarnings(iconv(
      x[bad], from = "Windows-1252", to = "UTF-8", sub = ""
    ))
  }
  converted
}

read_semicolon_zip_02 <- function(zip_path, member) {
  extract_dir <- tempfile(pattern = "rdc_zip_member_")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(extract_dir, recursive = TRUE, force = TRUE), add = TRUE)

  utils::unzip(
    zip_path,
    files = member,
    exdir = extract_dir,
    junkpaths = TRUE,
    overwrite = TRUE
  )

  extracted <- file.path(extract_dir, basename(member))
  assert_02(file.exists(extracted),
            paste0("Impossibile estrarre ", member, " da ", basename(zip_path), "."))

  out <- utils::read.csv2(
    extracted,
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "Windows-1252",
    na.strings = c("", "NA"),
    quote = "\"",
    comment.char = "",
    fill = TRUE
  )

  out[] <- lapply(out, normalize_utf8_02)
  nm <- normalize_utf8_02(names(out))
  nm <- sub("^\\ufeff", "", str_trim(nm))
  names(out) <- nm

  empty_col <- is.na(names(out)) | !nzchar(names(out))
  if (any(empty_col)) {
    droppable <- vapply(
      out[empty_col],
      function(z) all(is.na(z) | str_trim(as.character(z)) == ""),
      logical(1)
    )
    if (!all(droppable)) {
      stop("CSV con colonne senza nome ma non vuote.", call. = FALSE)
    }
    out <- out[, !empty_col, drop = FALSE]
  }

  assert_02(!any(is.na(names(out)) | names(out) == ""),
            "Restano colonne senza nome nel CSV.")
  assert_02(!anyDuplicated(names(out)),
            "Nomi colonna duplicati nel CSV.")
  out
}

# ------------------------------------------------------------------------------
# 1. Leggiamo SOLO la base pre-policy necessaria
# ------------------------------------------------------------------------------
# Gli output V10 canonici devono essere letti ESPLICITAMENTE da output_v10.
# In questo modo eventuali copie omonime nella cartella principale non possono
# essere selezionate per errore.
master_path <- file.path(OUTPUT_DIR, "master_naples_housing_panel.csv")
istat_zone_path <- file.path(OUTPUT_DIR, "istat2011_zone_characteristics.csv")
zones_gpkg_path <- file.path(OUTPUT_DIR, "zones_omi_istat2011.gpkg")

assert_02(file.exists(master_path),
          "Manca output_v10/master_naples_housing_panel.csv")
assert_02(file.exists(istat_zone_path),
          "Manca output_v10/istat2011_zone_characteristics.csv")
assert_02(file.exists(zones_gpkg_path),
          "Manca output_v10/zones_omi_istat2011.gpkg")

# Gli input grezzi possono invece essere cercati nelle cartelle previste.
cpa_zip_path <- find_file_02("dati-cpa_2011.zip")
shape_zip_path <- find_file_02("R15_11_WGS84.zip")
ace_xlsx_path <- find_file_02("subcomunali_063_063049.xlsx")

master <- utils::read.csv(
  master_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

required_master <- c("zona", "fascia")
assert_02(all(required_master %in% names(master)),
          "Il master non contiene zona/fascia.")
assert_02(nrow(master) == 1003L,
          paste0("Master inatteso: ", nrow(master), " righe invece di 1003."))

balanced_zones <- sort(unique(as.character(master$zona)))
assert_02(length(balanced_zones) == 59L,
          paste0("Attese 59 zone bilanciate, trovate ", length(balanced_zones), "."))

zone_band <- master |>
  transmute(zona = as.character(zona), fascia = as.character(fascia)) |>
  distinct()

assert_02(nrow(zone_band) == 59L && !anyDuplicated(zone_band$zona),
          "Relazione zona-fascia del master non univoca.")

istat_zone <- utils::read.csv(
  istat_zone_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8"
)

needed_zone_vars <- c(
  "zona", "quota_affitto_2011", "quota_occupati_15plus_2011",
  "PF1", "P1"
)
assert_02(all(needed_zone_vars %in% names(istat_zone)),
          "Mancano variabili 2011 necessarie in istat2011_zone_characteristics.csv.")
assert_02(nrow(istat_zone) == 60L && !anyDuplicated(istat_zone$zona),
          "Il file ISTAT-zona deve contenere 60 zone univoche.")

istat_zone <- istat_zone |>
  mutate(
    zona = as.character(zona),
    quota_affitto_2011 = as.numeric(quota_affitto_2011),
    quota_occupati_15plus_2011 = as.numeric(quota_occupati_15plus_2011),
    PF1 = as.numeric(PF1),
    P1 = as.numeric(P1)
  )

assert_02(all(is.finite(istat_zone$quota_affitto_2011)),
          "Quota affitto 2011 mancante/non finita.")
assert_02(all(is.finite(istat_zone$quota_occupati_15plus_2011)),
          "Quota occupati 2011 mancante/non finita.")

# ------------------------------------------------------------------------------
# 2. 8milaCensus 2011: ACE -> V6 (e V8/L12 per diagnostica)
# ------------------------------------------------------------------------------
ace_raw <- suppressMessages(readxl::read_excel(
  ace_xlsx_path,
  sheet = "Napoli",
  skip = 2,
  .name_repair = "unique"
))

names(ace_raw) <- str_squish(names(ace_raw))

find_col_code_02 <- function(df, code) {
  hit <- names(df)[str_detect(names(df), paste0("^", code, "\\s*-"))]
  assert_02(length(hit) == 1L,
            paste0("Colonna ", code, " non trovata univocamente in 8milaCensus."))
  hit
}

territorio_col <- names(ace_raw)[str_to_lower(names(ace_raw)) == "territorio"]
assert_02(length(territorio_col) == 1L,
          "Colonna Territorio non trovata univocamente in 8milaCensus.")

v6_col <- find_col_code_02(ace_raw, "V6")
v8_col <- find_col_code_02(ace_raw, "V8")
l12_col <- find_col_code_02(ace_raw, "L12")

ace_table <- data.frame(
  territorio = as.character(ace_raw[[territorio_col]]),
  V6_pct = safe_num(ace_raw[[v6_col]]),
  V8_pct = safe_num(ace_raw[[v8_col]]),
  L12_pct = safe_num(ace_raw[[l12_col]]),
  stringsAsFactors = FALSE
) |>
  filter(str_detect(str_to_lower(str_trim(territorio)), "^ace\\s*[0-9]+$")) |>
  mutate(
    ACE = as.integer(str_extract(territorio, "[0-9]+")),
    v6_disagio_economico_ace = V6_pct / 100,
    v8_neet_ace = V8_pct / 100,
    l12_occupazione_ace = L12_pct / 100
  ) |>
  select(
    ACE, v6_disagio_economico_ace,
    v8_neet_ace, l12_occupazione_ace
  ) |>
  arrange(ACE)

assert_02(nrow(ace_table) == 69L,
          paste0("Attese 69 ACE di Napoli in 8milaCensus, trovate ", nrow(ace_table), "."))
assert_02(!anyDuplicated(ace_table$ACE),
          "ACE duplicate in 8milaCensus.")
assert_02(all(is.finite(ace_table$v6_disagio_economico_ace)),
          "V6 mancante/non finito per almeno una ACE.")

# ------------------------------------------------------------------------------
# 3. Ricostruiamo sezione 2011 -> zona OMI per riportare V6 alle zone
# ------------------------------------------------------------------------------
members <- utils::unzip(cpa_zip_path, list = TRUE)$Name
cpa_member <- members[basename(members) == "R15_indicatori_2011_sezioni.csv"]
assert_02(length(cpa_member) == 1L,
          "R15_indicatori_2011_sezioni.csv non trovato univocamente.")

section_data_raw <- read_semicolon_zip_02(cpa_zip_path, cpa_member)
needed_section_vars <- c("PROCOM", "SEZ2011", "ACE", "P1", "PF1")
assert_02(all(needed_section_vars %in% names(section_data_raw)),
          "Variabili sezione/ACE/P1/PF1 mancanti nel Censimento 2011.")

section_data <- section_data_raw |>
  filter(as.numeric(PROCOM) == 63049) |>
  transmute(
    SEZ2011 = id_character_02(SEZ2011),
    ACE = as.integer(ACE),
    P1 = as.numeric(P1),
    PF1 = as.numeric(PF1)
  )

assert_02(nrow(section_data) == 4050L,
          paste0("Attese 4050 sezioni con dati per Napoli, trovate ", nrow(section_data), "."))
assert_02(!anyDuplicated(section_data$SEZ2011),
          "SEZ2011 duplicate nei dati censuari.")
assert_02(setequal(sort(unique(section_data$ACE)), sort(ace_table$ACE)),
          "Le ACE dei dati di sezione non coincidono con le ACE di 8milaCensus.")

shape_dir <- tempfile(pattern = "rdc_shape_2011_")
dir.create(shape_dir, recursive = TRUE, showWarnings = FALSE)

utils::unzip(shape_zip_path, exdir = shape_dir, overwrite = TRUE)
shp <- list.files(
  shape_dir,
  pattern = "R15_11_WGS84\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)
assert_02(length(shp) == 1L,
          "Shapefile R15_11_WGS84.shp non trovato univocamente.")

sections <- suppressMessages(sf::st_read(
  shp,
  quiet = TRUE,
  stringsAsFactors = FALSE
)) |>
  filter(as.numeric(PRO_COM) == 63049) |>
  mutate(SEZ2011 = id_character_02(SEZ2011)) |>
  select(SEZ2011) |>
  left_join(section_data, by = "SEZ2011") |>
  filter(!is.na(P1))

sections <- suppressWarnings(sf::st_make_valid(sections))
assert_02(nrow(sections) == 4050L,
          paste0("Dopo il merge geometrico attese 4050 sezioni, trovate ", nrow(sections), "."))

zones_omi <- suppressMessages(sf::st_read(
  zones_gpkg_path,
  quiet = TRUE,
  stringsAsFactors = FALSE
))

assert_02("zona" %in% names(zones_omi),
          "Il GPKG OMI non contiene la colonna zona.")

# Con oggetti sf NON assumiamo che la colonna geometrica si chiami "geometry":
# GDAL/sf può conservarla con un nome diverso su sistemi differenti.
# dplyr::select() su un oggetto sf mantiene automaticamente la geometria attiva.
zones_omi <- zones_omi |>
  mutate(zona = as.character(zona)) |>
  select(zona)

assert_02(
  inherits(zones_omi, "sf") &&
    !is.null(sf::st_geometry(zones_omi)) &&
    !any(sf::st_is_empty(zones_omi)),
  "Geometria OMI assente o vuota dopo la lettura del GPKG."
)

assert_02(nrow(zones_omi) == 60L && !anyDuplicated(zones_omi$zona),
          "Il GPKG deve contenere 60 zone OMI univoche.")

sections_metric <- sf::st_transform(sections, 32633)
zones_metric <- sf::st_transform(zones_omi, 32633)

section_points <- suppressWarnings(sf::st_point_on_surface(sections_metric))
assigned_raw <- suppressWarnings(sf::st_join(
  section_points,
  zones_metric,
  join = sf::st_within,
  left = FALSE
))

dup_ids <- assigned_raw |>
  sf::st_drop_geometry() |>
  count(SEZ2011, name = "n_zone") |>
  filter(n_zone > 1L) |>
  pull(SEZ2011)

if (length(dup_ids)) {
  overlaps_sf <- suppressWarnings(sf::st_intersection(
    sections_metric |>
      filter(SEZ2011 %in% dup_ids) |>
      select(SEZ2011),
    zones_metric |>
      select(zona)
  ))

  # Anche qui usiamo st_area() sull'intero oggetto sf, senza riferirci
  # al nome fisico della colonna geometrica.
  overlaps_sf$overlap_m2 <- as.numeric(sf::st_area(overlaps_sf))

  overlaps <- overlaps_sf |>
    sf::st_drop_geometry() |>
    arrange(SEZ2011, desc(overlap_m2), zona)

  chosen <- overlaps |>
    group_by(SEZ2011) |>
    slice(1L) |>
    ungroup() |>
    select(SEZ2011, zona)

  assigned_unique <- assigned_raw |>
    sf::st_drop_geometry() |>
    filter(!SEZ2011 %in% dup_ids)

  assigned_dup <- sections |>
    sf::st_drop_geometry() |>
    filter(SEZ2011 %in% dup_ids) |>
    left_join(chosen, by = "SEZ2011")

  assigned <- bind_rows(assigned_unique, assigned_dup)
} else {
  assigned <- sf::st_drop_geometry(assigned_raw)
}

assert_02(!anyDuplicated(assigned$SEZ2011),
          "Sezioni duplicate dopo la risoluzione dell'overlay.")

coverage_pop <- sum(assigned$P1, na.rm = TRUE) / sum(section_data$P1, na.rm = TRUE)
coverage_fam <- sum(assigned$PF1, na.rm = TRUE) / sum(section_data$PF1, na.rm = TRUE)
zone_coverage <- mean(unique(zones_omi$zona) %in% unique(assigned$zona))

assert_02(coverage_pop >= 0.99,
          paste0("Copertura popolazione overlay insufficiente: ", round(coverage_pop, 4)))
assert_02(coverage_fam >= 0.99,
          paste0("Copertura famiglie overlay insufficiente: ", round(coverage_fam, 4)))
assert_02(zone_coverage == 1,
          "Non tutte le 60 zone OMI ricevono sezioni censuarie.")

assigned <- assigned |>
  left_join(ace_table, by = "ACE")

assert_02(all(is.finite(assigned$v6_disagio_economico_ace)),
          "Alcune sezioni assegnate non trovano V6 della propria ACE.")

# Composizione ACE -> OMI (utile per audit e riproducibilità)
ace_to_omi <- assigned |>
  group_by(zona, ACE) |>
  summarise(
    popolazione = sum(P1, na.rm = TRUE),
    famiglie = sum(PF1, na.rm = TRUE),
    v6_disagio_economico_ace = first(v6_disagio_economico_ace),
    v8_neet_ace = first(v8_neet_ace),
    l12_occupazione_ace = first(l12_occupazione_ace),
    n_sezioni = n_distinct(SEZ2011),
    .groups = "drop"
  ) |>
  group_by(zona) |>
  mutate(
    # Usare if(), non ifelse(): il test è scalare per gruppo mentre il
    # numeratore è un vettore. ifelse() restituirebbe il primo valore riciclato.
    quota_famiglie_nella_zona = if (sum(famiglie, na.rm = TRUE) > 0) {
      famiglie / sum(famiglie, na.rm = TRUE)
    } else {
      rep(NA_real_, dplyr::n())
    },
    quota_popolazione_nella_zona = if (sum(popolazione, na.rm = TRUE) > 0) {
      popolazione / sum(popolazione, na.rm = TRUE)
    } else {
      rep(NA_real_, dplyr::n())
    }
  ) |>
  ungroup() |>
  arrange(zona, desc(quota_famiglie_nella_zona), ACE)

# Audit delle quote di composizione: per ogni zona devono sommare a 1.
share_audit <- ace_to_omi |>
  group_by(zona) |>
  summarise(
    sum_quota_famiglie = sum(quota_famiglie_nella_zona, na.rm = TRUE),
    sum_quota_popolazione = sum(quota_popolazione_nella_zona, na.rm = TRUE),
    .groups = "drop"
  )

assert_02(
  max(abs(share_audit$sum_quota_famiglie - 1), na.rm = TRUE) < 1e-10,
  "Le quote famiglie ACE->OMI non sommano a 1 in almeno una zona."
)
assert_02(
  max(abs(share_audit$sum_quota_popolazione - 1), na.rm = TRUE) < 1e-10,
  "Le quote popolazione ACE->OMI non sommano a 1 in almeno una zona."
)

ace_zone <- assigned |>
  group_by(zona) |>
  summarise(
    # V6 ha come denominatore le famiglie -> peso PF1.
    economic_distress_v6_2011 = weighted_mean_02(
      v6_disagio_economico_ace, PF1
    ),
    # V8 e L12 sono usati solo come diagnostica/robustezza -> peso popolazione.
    neet_v8_2011 = weighted_mean_02(v8_neet_ace, P1),
    employment_l12_ace_2011 = weighted_mean_02(l12_occupazione_ace, P1),
    famiglie_assegnate = sum(PF1, na.rm = TRUE),
    popolazione_assegnata = sum(P1, na.rm = TRUE),
    n_ace_intercettate = n_distinct(ACE),
    n_sezioni_assegnate = n_distinct(SEZ2011),
    .groups = "drop"
  )

assert_02(nrow(ace_zone) == 60L && !anyDuplicated(ace_zone$zona),
          "Aggregazione ACE -> OMI non produce 60 zone univoche.")
assert_02(all(is.finite(ace_zone$economic_distress_v6_2011)),
          "V6 aggregato mancante in almeno una zona.")

# I file dello shapefile sono ormai caricati in memoria: pulizia cartella temporanea.
unlink(shape_dir, recursive = TRUE, force = TRUE)

# ------------------------------------------------------------------------------
# 4. Costruzione DEFINITIVA dell'esposizione RdC 2011
# ------------------------------------------------------------------------------
zone_fascia_60 <- suppressMessages(sf::st_read(
  zones_gpkg_path,
  quiet = TRUE,
  stringsAsFactors = FALSE
)) |>
  sf::st_drop_geometry() |>
  transmute(
    zona = as.character(zona),
    fascia = as.character(fascia)
  ) |>
  distinct()

exposure60 <- istat_zone |>
  select(
    zona, P1, PF1,
    quota_affitto_2011,
    quota_occupati_15plus_2011
  ) |>
  left_join(zone_fascia_60, by = "zona") |>
  left_join(ace_zone, by = "zona") |>
  mutate(
    balanced_panel = zona %in% balanced_zones,
    renter_share_2011 = quota_affitto_2011,
    low_employment_2011 = 1 - quota_occupati_15plus_2011,
    economic_distress_2011 = economic_distress_v6_2011
  )

assert_02(nrow(exposure60) == 60L,
          "Exposure base deve contenere 60 zone.")
assert_02(sum(exposure60$balanced_panel) == 59L,
          "Flag balanced_panel non identifica 59 zone.")

ref <- exposure60$balanced_panel

exposure60 <- exposure60 |>
  mutate(
    z_renter_share_2011 = zscore_ref_02(renter_share_2011, ref),
    z_low_employment_2011 = zscore_ref_02(low_employment_2011, ref),
    z_economic_distress_2011 = zscore_ref_02(economic_distress_2011, ref),
    z_neet_v8_2011 = zscore_ref_02(neet_v8_2011, ref),
    rdc_exposure_main_raw = (
      z_renter_share_2011 +
      z_low_employment_2011 +
      z_economic_distress_2011
    ) / 3,
    rdc_exposure_main = zscore_ref_02(rdc_exposure_main_raw, ref),
    rdc_exposure_2comp_raw = (
      z_renter_share_2011 +
      z_low_employment_2011
    ) / 2,
    rdc_exposure_2comp = zscore_ref_02(rdc_exposure_2comp_raw, ref),
    rdc_exposure_neet_raw = (
      z_renter_share_2011 +
      z_low_employment_2011 +
      z_neet_v8_2011
    ) / 3,
    rdc_exposure_neet = zscore_ref_02(rdc_exposure_neet_raw, ref)
  )

# Anderson inverse-covariance weighting: robustness, NON main.
component_names <- c(
  "z_renter_share_2011",
  "z_low_employment_2011",
  "z_economic_distress_2011"
)

X_ref <- as.matrix(exposure60[ref, component_names])
X_all <- as.matrix(exposure60[, component_names])

cov_ref <- stats::cov(X_ref, use = "complete.obs")
one_vec <- rep(1, ncol(cov_ref))
anderson_num <- tryCatch(
  qr.solve(cov_ref, one_vec),
  error = function(e) rep(1, length(one_vec))
)
anderson_weights <- as.numeric(anderson_num / sum(anderson_num))
names(anderson_weights) <- component_names

anderson_raw <- as.numeric(X_all %*% anderson_weights)
exposure60$rdc_exposure_anderson <- zscore_ref_02(anderson_raw, ref)

# PCA1: robustness, NON main.
pca_fit <- stats::prcomp(X_ref, center = FALSE, scale. = FALSE)
pca_loading <- pca_fit$rotation[, 1]
if (sum(pca_loading) < 0) pca_loading <- -pca_loading
pca_raw <- as.numeric(X_all %*% pca_loading)
exposure60$rdc_exposure_pca1 <- zscore_ref_02(pca_raw, ref)
pca_explained <- summary(pca_fit)$importance[2, 1]

# Ordine finale delle colonne.
exposure60 <- exposure60 |>
  select(
    zona, fascia, balanced_panel,
    renter_share_2011,
    low_employment_2011,
    economic_distress_2011,
    neet_v8_2011,
    employment_l12_ace_2011,
    z_renter_share_2011,
    z_low_employment_2011,
    z_economic_distress_2011,
    z_neet_v8_2011,
    rdc_exposure_main,
    rdc_exposure_2comp,
    rdc_exposure_anderson,
    rdc_exposure_pca1,
    rdc_exposure_neet,
    famiglie_assegnate,
    popolazione_assegnata,
    n_ace_intercettate,
    n_sezioni_assegnate
  ) |>
  arrange(fascia, zona)

exposure59 <- exposure60 |>
  filter(balanced_panel) |>
  arrange(fascia, zona)

assert_02(nrow(exposure59) == 59L,
          "Exposure main deve contenere 59 zone.")
assert_02(all(is.finite(exposure59$rdc_exposure_main)),
          "RdCExposure main contiene valori mancanti/non finiti.")
assert_02(abs(mean(exposure59$rdc_exposure_main)) < 1e-10,
          "RdCExposure main non ha media circa zero sulle 59 zone.")
assert_02(abs(stats::sd(exposure59$rdc_exposure_main) - 1) < 1e-10,
          "RdCExposure main non ha sd circa uno sulle 59 zone.")

# ------------------------------------------------------------------------------
# 5. Diagnostica dell'indice (ANCORA senza outcome OMI)
# ------------------------------------------------------------------------------
component_summary <- exposure59 |>
  summarise(
    across(
      c(
        renter_share_2011,
        low_employment_2011,
        economic_distress_2011,
        neet_v8_2011,
        rdc_exposure_main
      ),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        min = ~min(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        max = ~max(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    everything(),
    names_to = c("variabile", ".value"),
    names_pattern = "(.+)_(mean|sd|min|median|max)$"
  )

corr_vars <- c(
  "renter_share_2011",
  "low_employment_2011",
  "economic_distress_2011",
  "neet_v8_2011",
  "rdc_exposure_main",
  "rdc_exposure_2comp",
  "rdc_exposure_anderson",
  "rdc_exposure_pca1",
  "rdc_exposure_neet"
)

corr_matrix <- stats::cor(
  exposure59[, corr_vars],
  use = "pairwise.complete.obs"
)

correlations_long <- as.data.frame(as.table(corr_matrix)) |>
  rename(variabile_1 = Var1, variabile_2 = Var2, correlazione = Freq)

band_summary <- exposure59 |>
  group_by(fascia) |>
  summarise(
    n_zone = n(),
    renter_share_mean = mean(renter_share_2011),
    low_employment_mean = mean(low_employment_2011),
    economic_distress_mean = mean(economic_distress_2011),
    rdc_exposure_mean = mean(rdc_exposure_main),
    rdc_exposure_sd = sd(rdc_exposure_main),
    rdc_exposure_min = min(rdc_exposure_main),
    rdc_exposure_max = max(rdc_exposure_main),
    .groups = "drop"
  )

# Diagnostica dei pesi alternativi.
weights_diag <- data.frame(
  componente = c("renter_share", "low_employment", "economic_distress"),
  peso_main_equal = rep(1/3, 3),
  peso_anderson = unname(anderson_weights),
  loading_pca1 = unname(pca_loading),
  stringsAsFactors = FALSE
)

alternative_correlations <- data.frame(
  confronto = c(
    "main_vs_2comp",
    "main_vs_anderson",
    "main_vs_pca1",
    "main_vs_neet"
  ),
  correlazione = c(
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_2comp),
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_anderson),
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_pca1),
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_neet)
  )
)

# Controllo di coerenza spaziale dell'occupazione:
# L12 ACE è solo diagnostica, perché la misura MAIN usa il conteggio sezione -> OMI.
employment_mapping_corr <- cor(
  exposure59$low_employment_2011,
  1 - exposure59$employment_l12_ace_2011,
  use = "complete.obs"
)

top_bottom <- bind_rows(
  exposure59 |>
    arrange(rdc_exposure_main) |>
    slice_head(n = 10) |>
    mutate(gruppo = "10 meno esposte"),
  exposure59 |>
    arrange(desc(rdc_exposure_main)) |>
    slice_head(n = 10) |>
    mutate(gruppo = "10 più esposte")
) |>
  select(
    gruppo, zona, fascia, rdc_exposure_main,
    renter_share_2011, low_employment_2011, economic_distress_2011
  )

definition <- data.frame(
  componente = c(
    "renter_share_2011",
    "low_employment_2011",
    "economic_distress_2011",
    "rdc_exposure_main"
  ),
  fonte = c(
    "ISTAT Censimento 2011 - sezioni",
    "ISTAT Censimento 2011 - sezioni",
    "ISTAT 8milaCensus 2011 - ACE, riportato a OMI con pesi PF1",
    "Indice standardizzato sulle 59 zone bilanciate"
  ),
  definizione = c(
    "Famiglie in affitto / famiglie residenti",
    "1 - quota occupati 15+",
    "V6: famiglie con potenziale disagio economico / totale famiglie",
    "Media a pesi uguali dei tre z-score, poi nuovamente standardizzata"
  ),
  segno_atteso_esposizione = c("+", "+", "+", "+"),
  peso_main = c(1/3, 1/3, 1/3, NA_real_),
  incluso_nel_main = c(TRUE, TRUE, TRUE, NA),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 6. Audit formale
# ------------------------------------------------------------------------------
audit <- data.frame(
  controllo = c(
    "Master zona-semestre",
    "Zone pannello bilanciato",
    "Zone ISTAT complessive",
    "ACE 8milaCensus Napoli",
    "Sezioni ISTAT Napoli con dati",
    "Copertura popolazione sezione->OMI",
    "Copertura famiglie sezione->OMI",
    "Zone OMI coperte dall'overlay",
    "Zone con V6 disponibile",
    "Exposure main finita su 59 zone",
    "Exposure main media",
    "Exposure main sd",
    "Outcome OMI usati per costruire l'indice?",
    "Pesi MAIN",
    "Varianza spiegata PCA1",
    "Correlazione MAIN-Anderson",
    "Correlazione MAIN-PCA1",
    "Correlazione MAIN-2comp",
    "Correlazione low employment vs L12-ACE"
  ),
  valore = c(
    nrow(master),
    length(balanced_zones),
    nrow(exposure60),
    nrow(ace_table),
    nrow(section_data),
    coverage_pop,
    coverage_fam,
    zone_coverage,
    sum(is.finite(exposure60$economic_distress_2011)),
    sum(is.finite(exposure59$rdc_exposure_main)),
    mean(exposure59$rdc_exposure_main),
    sd(exposure59$rdc_exposure_main),
    "NO",
    "1/3 renter + 1/3 low employment + 1/3 V6",
    pca_explained,
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_anderson),
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_pca1),
    cor(exposure59$rdc_exposure_main, exposure59$rdc_exposure_2comp),
    employment_mapping_corr
  ),
  esito = c(
    ifelse(nrow(master) == 1003L, "PASS", "FAIL"),
    ifelse(length(balanced_zones) == 59L, "PASS", "FAIL"),
    ifelse(nrow(exposure60) == 60L, "PASS", "FAIL"),
    ifelse(nrow(ace_table) == 69L, "PASS", "FAIL"),
    ifelse(nrow(section_data) == 4050L, "PASS", "FAIL"),
    ifelse(coverage_pop >= 0.99, "PASS", "FAIL"),
    ifelse(coverage_fam >= 0.99, "PASS", "FAIL"),
    ifelse(zone_coverage == 1, "PASS", "FAIL"),
    ifelse(sum(is.finite(exposure60$economic_distress_2011)) == 60L, "PASS", "FAIL"),
    ifelse(sum(is.finite(exposure59$rdc_exposure_main)) == 59L, "PASS", "FAIL"),
    ifelse(abs(mean(exposure59$rdc_exposure_main)) < 1e-8, "PASS", "FAIL"),
    ifelse(abs(sd(exposure59$rdc_exposure_main) - 1) < 1e-8, "PASS", "FAIL"),
    "PASS",
    "FROZEN",
    "INFO",
    "INFO",
    "INFO",
    "INFO",
    "INFO"
  ),
  stringsAsFactors = FALSE
)

assert_02(!any(audit$esito == "FAIL"),
          "Almeno un audit dell'esposizione RdC è FAIL.")

# ------------------------------------------------------------------------------
# 7. Output tabellari e master arricchito
# ------------------------------------------------------------------------------
write_csv_02(exposure59, "12_rdc_exposure_zone_59.csv")
write_csv_02(exposure60, "12b_rdc_exposure_zone_60.csv")
write_csv_02(definition, "12c_rdc_exposure_definition.csv")
write_csv_02(component_summary, "12d_rdc_exposure_component_summary.csv")
write_csv_02(correlations_long, "12e_rdc_exposure_correlations.csv")
write_csv_02(band_summary, "12f_rdc_exposure_by_band.csv")

weights_diag_out <- bind_rows(
  weights_diag |>
    mutate(tipo = "pesi_componenti"),
  data.frame(
    componente = c(
      "PCA1_variance_explained",
      alternative_correlations$confronto
    ),
    peso_main_equal = NA_real_,
    peso_anderson = c(
      pca_explained,
      alternative_correlations$correlazione
    ),
    loading_pca1 = NA_real_,
    tipo = c(
      "diagnostica",
      rep("correlazione_indici", nrow(alternative_correlations))
    ),
    stringsAsFactors = FALSE
  )
)
write_csv_02(weights_diag_out, "12g_rdc_exposure_weights_diagnostics.csv")
write_csv_02(ace_to_omi, "12h_rdc_ace_to_omi_composition.csv")
write_csv_02(top_bottom, "12i_rdc_exposure_top_bottom.csv")
write_csv_02(audit, "12j_audit_rdc_exposure.csv")
write_csv_02(ace_table, "12k_8milacensus_ace2011_source.csv")

exposure_for_master <- exposure60 |>
  select(
    zona,
    renter_share_2011,
    low_employment_2011,
    economic_distress_2011,
    neet_v8_2011,
    rdc_exposure_main,
    rdc_exposure_2comp,
    rdc_exposure_anderson,
    rdc_exposure_pca1,
    rdc_exposure_neet
  )

master_rdc <- master |>
  left_join(exposure_for_master, by = "zona")

assert_02(nrow(master_rdc) == nrow(master),
          "Il join RdC ha modificato il numero di righe del master.")
assert_02(all(names(master) %in% names(master_rdc)),
          "Il master arricchito ha perso una o più colonne del master V10 originale.")
assert_02(ncol(master_rdc) == ncol(master) + ncol(exposure_for_master) - 1L,
          "Numero di colonne inatteso nel master arricchito.")
assert_02(all(is.finite(master_rdc$rdc_exposure_main)),
          "RdCExposure mancante nel master arricchito.")

write_csv_02(master_rdc, "master_naples_housing_panel_rdc.csv")

# GPKG per mappe/robustezze future.
zones_rdc <- suppressMessages(sf::st_read(
  zones_gpkg_path,
  quiet = TRUE,
  stringsAsFactors = FALSE
)) |>
  mutate(
    zona = as.character(zona),
    fascia = as.character(fascia)
  ) |>
  # select() mantiene automaticamente la geometria attiva dell'oggetto sf.
  select(zona, fascia) |>
  left_join(exposure60, by = c("zona", "fascia"))

assert_02(
  inherits(zones_rdc, "sf") &&
    !is.null(sf::st_geometry(zones_rdc)) &&
    !any(sf::st_is_empty(zones_rdc)),
  "Geometria mancante nel GPKG finale RdC."
)

gpkg_out <- file.path(OUTPUT_DIR, "zones_omi_rdc_exposure.gpkg")
if (file.exists(gpkg_out)) unlink(gpkg_out)
suppressMessages(sf::st_write(
  zones_rdc,
  gpkg_out,
  quiet = TRUE
))

# ------------------------------------------------------------------------------
# 8. Figure SOLO descrittive dell'esposizione (nessun outcome OMI)
# ------------------------------------------------------------------------------
fig_map <- ggplot(zones_rdc |> filter(balanced_panel)) +
  geom_sf(aes(fill = rdc_exposure_main), linewidth = 0.15) +
  scale_fill_viridis_c(
    name = "RdC exposure\n(z-score)",
    option = "C"
  ) +
  labs(
    title = "Pre-policy exposure to the Citizenship Income",
    subtitle = "Pre-policy index built only from 2011 characteristics",
    caption = "Main index: renter share + low employment + economic distress (equal weights)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  file.path(OUTPUT_DIR, "fig09_rdc_exposure_map.png"),
  fig_map,
  width = 8,
  height = 7,
  dpi = 300
)

components_long <- exposure59 |>
  select(
    zona, fascia,
    z_renter_share_2011,
    z_low_employment_2011,
    z_economic_distress_2011
  ) |>
  pivot_longer(
    cols = starts_with("z_"),
    names_to = "componente",
    values_to = "zscore"
  ) |>
  mutate(
    componente = recode(
      componente,
      z_renter_share_2011 = "Renter share",
      z_low_employment_2011 = "Low employment",
      z_economic_distress_2011 = "Economic distress (V6)"
    )
  ) |>
  group_by(fascia, componente) |>
  summarise(media_z = mean(zscore), .groups = "drop")

fig_components <- ggplot(
  components_long,
  aes(x = fascia, y = media_z, group = componente, linetype = componente)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  labs(
    title = "Components of the pre-policy RdC exposure index",
    subtitle = "Average standardized component by OMI band",
    x = "OMI band",
    y = "Average z-score",
    linetype = "Component"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(OUTPUT_DIR, "fig10_rdc_components_by_band.png"),
  fig_components,
  width = 8,
  height = 5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 9. README metodologico: congeliamo la definizione
# ------------------------------------------------------------------------------
readme <- c(
  "V10 - 02 RDC EXPOSURE BUILD",
  "============================",
  "",
  "DEFINIZIONE MAIN CONGELATA",
  "RdCExposure_main = z[(z renter share + z low employment + z V6 economic distress)/3].",
  "La standardizzazione usa come riferimento le 59 zone del pannello bilanciato.",
  "",
  "INTERPRETAZIONE",
  "L'indice NON è una probabilità di eleggibilità e NON è il take-up osservato.",
  "È una misura relativa, pre-policy, della presenza territoriale di caratteristiche",
  "associate alla potenziale esposizione al Reddito di Cittadinanza.",
  "",
  "FONTI",
  "- Renter share: ISTAT Censimento 2011, sezioni -> OMI.",
  "- Low employment: ISTAT Censimento 2011, sezioni -> OMI.",
  "- Economic distress V6: ISTAT 8milaCensus 2011 a livello ACE.",
  "  V6 viene riportato alle zone OMI tramite le sezioni e ponderazione PF1 (famiglie).",
  "",
  "SCELTE PRE-SPECIFICATE",
  "- Nessun canone/prezzo OMI entra nella costruzione.",
  "- Nessuna fascia/distanza OMI entra nella costruzione.",
  "- Pesi MAIN uguali: 1/3, 1/3, 1/3.",
  "- Anderson/PCA/2-component/NEET sono robustness e NON possono sostituire il main",
  "  sulla base dei risultati futuri sugli affitti.",
  "",
  "CAUTELE",
  "- V6 è disponibile a livello ACE, non di singola sezione; il passaggio ACE->OMI",
  "  è una interpolazione territoriale ponderata per il numero di famiglie.",
  "- L'indice misura esposizione prevista, non beneficiari effettivi.",
  "- IDISE 2021 non viene utilizzato per costruire l'esposizione al 2019.",
  "",
  paste0("Copertura popolazione overlay: ", sprintf("%.4f%%", 100 * coverage_pop)),
  paste0("Copertura famiglie overlay: ", sprintf("%.4f%%", 100 * coverage_fam)),
  paste0("PCA1 varianza spiegata (diagnostica): ", sprintf("%.4f", pca_explained)),
  paste0(
    "Pesi Anderson (diagnostica): ",
    paste(sprintf("%s=%.4f", names(anderson_weights), anderson_weights), collapse = "; ")
  ),
  "",
  "NEXT STEP",
  "03_main_models.R userà RdCExposure_main in un modello dinamico Exposure x semestre.",
  "Solo lì verranno riaperti gli outcome OMI."
)

writeLines(
  readme,
  con = file.path(OUTPUT_DIR, "README_02_RDC_EXPOSURE.txt"),
  useBytes = TRUE
)

message("")
message("==============================================================")
message("02_rdc_exposure_build.R COMPLETATO")
message("==============================================================")
message("Zone main: ", nrow(exposure59))
message("Copertura popolazione: ", sprintf("%.4f%%", 100 * coverage_pop))
message("Copertura famiglie: ", sprintf("%.4f%%", 100 * coverage_fam))
message("PCA1 varianza spiegata: ", sprintf("%.4f", pca_explained))
message("Output: ", normalizePath(OUTPUT_DIR, winslash = "/", mustWork = TRUE))
message("La definizione MAIN è ora CONGELATA.")
