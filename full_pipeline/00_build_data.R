# ==============================================================================
# V10 - NAPLES RENT REPRICING
# 00_build_data.R
# Costruzione riproducibile del panel OMI + caratteristiche ISTAT 2011
# Periodo fissato: 2016-1 / 2024-1. 2024-2 ESCLUSO PER DISEGNO.
# ==============================================================================

options(stringsAsFactors = FALSE, scipen = 999)

required_packages <- c("dplyr", "tidyr", "stringr", "sf")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Pacchetti R mancanti: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(sf)
})

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
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

EXPECTED_SEMESTERS <- c(
  as.vector(rbind(paste0(2016:2023, "-1"), paste0(2016:2023, "-2"))),
  "2024-1"
)

assert_v10 <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
  invisible(TRUE)
}

parse_number_it <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N.D.", "ND", "-")] <- NA_character_
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

id_character <- function(x) {
  out <- format(x, scientific = FALSE, trim = TRUE)
  sub("\\.0+$", "", out)
}

zscore_ref <- function(x, reference) {
  reference <- as.logical(reference)
  mu <- mean(x[reference], na.rm = TRUE)
  sig <- stats::sd(x[reference], na.rm = TRUE)
  assert_v10(is.finite(sig) && sig > 0, "Deviazione standard nulla/non valida.")
  (x - mu) / sig
}

safe_ratio <- function(num, den) {
  ifelse(is.finite(num) & is.finite(den) & den != 0, num / den, NA_real_)
}

write_csv_v10 <- function(x, filename) {
  utils::write.csv(x, file.path(OUTPUT_DIR, filename), row.names = FALSE, na = "")
}

read_semicolon_zip <- function(zip_path, member, skip = 0L) {
  exdir <- tempfile("v10_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(zip_path, files = member, exdir = exdir, junkpaths = TRUE, overwrite = TRUE)
  p <- file.path(exdir, basename(member))

  out <- utils::read.csv2(
    p, skip = skip, header = TRUE, stringsAsFactors = FALSE,
    check.names = FALSE, fileEncoding = "Windows-1252",
    na.strings = c("", "NA"), quote = "\"", comment.char = "", fill = TRUE
  )

  # I CSV OMI/NTN possono terminare ogni riga con un ';'. Con check.names=FALSE
  # R crea allora una colonna finale con nome vuoto. dplyr::mutate() rifiuta
  # data frame con nomi NA/"", quindi puliamo i nomi e rimuoviamo soltanto
  # colonne senza nome che siano interamente vuote.
  normalize_utf8_v10 <- function(x) {
    if (!is.character(x)) return(x)
    y <- suppressWarnings(iconv(x, from = "", to = "UTF-8", sub = NA_character_))
    bad <- is.na(y) & !is.na(x)
    if (any(bad)) {
      y[bad] <- suppressWarnings(iconv(x[bad], from = "Windows-1252", to = "UTF-8", sub = ""))
    }
    y
  }

  out[] <- lapply(out, normalize_utf8_v10)
  clean_names <- normalize_utf8_v10(names(out))
  clean_names <- sub("^\\ufeff", "", trimws(clean_names))
  names(out) <- clean_names

  unnamed <- is.na(names(out)) | !nzchar(names(out))
  empty_col <- vapply(
    out,
    function(z) all(is.na(z) | trimws(as.character(z)) == ""),
    logical(1)
  )
  drop_cols <- unnamed & empty_col
  if (any(drop_cols)) out <- out[, !drop_cols, drop = FALSE]

  assert_v10(
    !any(is.na(names(out)) | !nzchar(names(out))),
    paste0("Colonna senza nome ma non vuota in ", basename(member), ".")
  )
  assert_v10(
    !anyDuplicated(names(out)),
    paste0("Nomi di colonna duplicati in ", basename(member), ".")
  )

  out
}

# ------------------------------------------------------------------------------
# 0. Risoluzione degli input OMI: cartelle già estratte oppure archivi aggregati
# ------------------------------------------------------------------------------
resolve_zip_collection <- function(folder_name, aggregate_name) {
  candidates <- c(file.path(INPUT_DIR, folder_name), file.path(PROJECT_DIR, folder_name))
  existing_dir <- candidates[dir.exists(candidates)]
  if (length(existing_dir)) {
    z <- list.files(existing_dir[1], pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
    if (length(z)) return(normalizePath(z, winslash = "/", mustWork = TRUE))
  }
  aggregate_candidates <- c(file.path(INPUT_DIR, aggregate_name), file.path(PROJECT_DIR, aggregate_name))
  aggregate <- aggregate_candidates[file.exists(aggregate_candidates)]
  if (!length(aggregate)) {
    stop("Input non trovato: ", folder_name, " oppure ", aggregate_name, call. = FALSE)
  }
  exdir <- file.path(PROCESSED_DIR, paste0("unpacked_", folder_name))
  if (dir.exists(exdir)) unlink(exdir, recursive = TRUE, force = TRUE)
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(aggregate[1], exdir = exdir)
  z <- list.files(exdir, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  assert_v10(length(z) > 0, paste0("Nessuno ZIP interno trovato in ", aggregate_name, "."))
  normalizePath(z, winslash = "/", mustWork = TRUE)
}

OMI_CANDIDATE_ZIPS <- resolve_zip_collection("omi_raw", "omi_raw.zip")
NTN_CANDIDATE_ZIPS <- resolve_zip_collection("omi_ntn", "omi_ntn.zip")

period_from_omi_zip <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  value_member <- members[grepl("_VALORI\\.csv$", members, ignore.case = TRUE)]
  if (length(value_member) != 1L) return(NA_character_)
  m <- stringr::str_match(basename(value_member), "_(20[0-9]{2})([12])_VALORI\\.csv$")
  if (is.na(m[1, 1])) return(NA_character_)
  paste0(m[1, 2], "-", m[1, 3])
}

omi_period_index <- data.frame(
  zip_path = OMI_CANDIDATE_ZIPS,
  semestre = vapply(OMI_CANDIDATE_ZIPS, period_from_omi_zip, character(1)),
  stringsAsFactors = FALSE
) |>
  filter(!is.na(semestre))

# Il 2024-2 e ogni periodo successivo vengono esclusi esplicitamente.
omi_period_index <- omi_period_index |>
  filter(semestre %in% EXPECTED_SEMESTERS)

assert_v10(
  setequal(omi_period_index$semestre, EXPECTED_SEMESTERS) && nrow(omi_period_index) == 17L,
  paste0("OMI incompleto: servono esattamente i 17 semestri 2016-1 / 2024-1. Trovati: ",
         paste(sort(omi_period_index$semestre), collapse = ", "))
)

# ------------------------------------------------------------------------------
# 1. OMI: tipologia 20, Abitazioni civili, stato NORMALE
# ------------------------------------------------------------------------------
read_omi_main <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  value_member <- members[grepl("_VALORI\\.csv$", members, ignore.case = TRUE)]
  assert_v10(length(value_member) == 1L, paste0("File VALORI non univoco in ", basename(zip_path)))
  m <- str_match(basename(value_member), "_(20[0-9]{2})([12])_VALORI\\.csv$")
  assert_v10(!is.na(m[1, 1]), "Periodo OMI non riconosciuto.")
  year <- as.integer(m[1, 2]); half <- as.integer(m[1, 3]); semester <- paste0(year, "-", half)

  raw <- read_semicolon_zip(zip_path, value_member, skip = 1L)
  required <- c(
    "Comune_amm", "Fascia", "Zona", "Cod_Tip", "Descr_Tipologia", "Stato",
    "Compr_min", "Compr_max", "Sup_NL_compr", "Loc_min", "Loc_max", "Sup_NL_loc"
  )
  assert_v10(all(required %in% names(raw)), paste0("Colonne OMI mancanti in ", basename(zip_path)))

  napoli <- raw |>
    mutate(
      Comune_amm = str_to_upper(str_squish(as.character(Comune_amm))),
      Zona = str_to_upper(str_squish(as.character(Zona))),
      Fascia = str_to_upper(str_squish(as.character(Fascia))),
      Cod_Tip = str_squish(as.character(Cod_Tip)),
      Stato = str_to_upper(str_squish(as.character(Stato)))
    ) |>
    filter(Comune_amm == "F839", nzchar(Zona))

  selected <- napoli |>
    filter(Cod_Tip == "20", Stato == "NORMALE") |>
    transmute(
      zona = Zona, fascia = Fascia,
      anno = year, semestre_num = half, semestre = semester,
      cod_tip = Cod_Tip, descr_tipologia = str_squish(as.character(Descr_Tipologia)),
      stato = Stato,
      compr_min = parse_number_it(Compr_min), compr_max = parse_number_it(Compr_max),
      superficie_compr = str_to_upper(str_squish(as.character(Sup_NL_compr))),
      loc_min = parse_number_it(Loc_min), loc_max = parse_number_it(Loc_max),
      superficie_loc = str_to_upper(str_squish(as.character(Sup_NL_loc))),
      file_sorgente = basename(zip_path)
    )

  audit <- data.frame(
    semestre = semester, anno = year, semestre_num = half,
    file_zip = basename(zip_path),
    righe_napoli = nrow(napoli),
    zone_totali_file = n_distinct(napoli$Zona),
    zone_tipologia20 = n_distinct(selected$zona),
    duplicati_tipologia20 = paste(unique(selected$zona[duplicated(selected$zona)]), collapse = ","),
    valori_mancanti = paste(selected$zona[!complete.cases(selected[c("compr_min", "compr_max", "loc_min", "loc_max")])], collapse = ","),
    zone_senza_tipologia20 = paste(sort(setdiff(unique(napoli$Zona), unique(selected$zona))), collapse = ","),
    superficie_compravendita = paste(sort(unique(selected$superficie_compr)), collapse = ","),
    superficie_locazione = paste(sort(unique(selected$superficie_loc)), collapse = ","),
    stringsAsFactors = FALSE
  )

  list(data = selected, audit = audit, zip_path = zip_path)
}

omi_parts <- lapply(omi_period_index$zip_path, read_omi_main)
omi_audit <- bind_rows(lapply(omi_parts, `[[`, "audit")) |>
  mutate(semestre = factor(semestre, levels = EXPECTED_SEMESTERS)) |>
  arrange(semestre) |>
  mutate(semestre = as.character(semestre))

omi <- bind_rows(lapply(omi_parts, `[[`, "data")) |>
  mutate(period_index = match(semestre, EXPECTED_SEMESTERS)) |>
  arrange(zona, period_index)

assert_v10(!anyDuplicated(omi[c("zona", "semestre")]), "Duplicati zona-semestre OMI.")
assert_v10(all(omi$compr_min > 0 & omi$compr_max > 0 & omi$loc_min > 0 & omi$loc_max > 0),
           "Valori OMI nulli/negativi.")
assert_v10(all(omi$compr_min <= omi$compr_max & omi$loc_min <= omi$loc_max),
           "Intervalli OMI non validi.")

omi <- omi |>
  mutate(
    compr_medio = (compr_min + compr_max) / 2,
    loc_medio = (loc_min + loc_max) / 2,
    log_sale = log(compr_medio),
    log_rent = log(loc_medio),
    rent_sale_ratio_annualized = 12 * loc_medio / compr_medio,
    log_rent_sale_gap = log(12 * loc_medio) - log(compr_medio),
    sample_levels_2018plus = as.integer(anno >= 2018L),
    DE_peripheral = as.integer(fascia %in% c("D", "E")),
    break_2019_2 = as.integer(semestre == "2019-2"),
    pandemic_2020_2021 = as.integer(anno %in% c(2020L, 2021L)),
    post_2022 = as.integer(anno >= 2022L),
    catchup_2023_2 = as.integer(semestre == "2023-2")
  ) |>
  group_by(zona) |>
  arrange(period_index, .by_group = TRUE) |>
  mutate(
    d_log_sale = log_sale - lag(log_sale),
    d_log_rent = log_rent - lag(log_rent),
    d_log_rent_sale_gap = log_rent_sale_gap - lag(log_rent_sale_gap)
  ) |>
  ungroup()

# La differenza 2018-1 attraversa il cambio N -> L della superficie locativa.
omi <- omi |>
  mutate(
    d_log_rent = ifelse(semestre == "2018-1", NA_real_, d_log_rent),
    d_log_rent_sale_gap = ifelse(semestre == "2018-1", NA_real_, d_log_rent_sale_gap),
    valid_rent_difference = as.integer(is.finite(d_log_rent)),
    valid_sale_difference = as.integer(is.finite(d_log_sale)),
    valid_gap_difference = as.integer(is.finite(d_log_rent_sale_gap))
  )

zone_counts <- omi |> count(zona, name = "n_semestri")
balanced_zones <- zone_counts |> filter(n_semestri == length(EXPECTED_SEMESTERS)) |> pull(zona)
excluded_zones <- zone_counts |>
  filter(!zona %in% balanced_zones) |>
  mutate(motivo = "Tipologia 20 assente in almeno un semestre; nessuna imputazione")

assert_v10(n_distinct(omi$zona) == 60L, "L'unione OMI deve contenere 60 zone con tipologia 20.")
assert_v10(length(balanced_zones) == 59L, "Il pannello bilanciato deve contenere 59 zone.")
assert_v10(identical(excluded_zones$zona, "D32"), "La zona non bilanciata attesa e D32.")

omi_bal <- omi |> filter(zona %in% balanced_zones)
assert_v10(nrow(omi_bal) == 59L * 17L, "Il panel bilanciato non contiene 59 x 17 righe.")
assert_v10(all(omi_bal$superficie_compr[omi_bal$anno >= 2018] == "L"), "Vendita 2018+ non tutta L.")
assert_v10(all(omi_bal$superficie_loc[omi_bal$anno >= 2018] == "L"), "Locazione 2018+ non tutta L.")

# ------------------------------------------------------------------------------
# 2. Geografia OMI 2024-1: scelta fissa per overlay e robustness spaziali
# ------------------------------------------------------------------------------
latest_idx <- which(vapply(omi_parts, function(x) x$audit$semestre == "2024-1", logical(1)))
assert_v10(length(latest_idx) == 1L, "ZIP OMI 2024-1 non individuato.")
latest_zip <- omi_parts[[latest_idx]]$zip_path
kml_members <- utils::unzip(latest_zip, list = TRUE)$Name
kml_member <- kml_members[grepl("\\.kml$", kml_members, ignore.case = TRUE)]
assert_v10(length(kml_member) == 1L, "KML 2024-1 non univoco.")
kml_dir <- file.path(PROCESSED_DIR, "omi_kml_2024_1")
dir.create(kml_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(latest_zip, files = kml_member, exdir = kml_dir, junkpaths = TRUE, overwrite = TRUE)
kml_path <- file.path(kml_dir, basename(kml_member))

omi_sf <- suppressMessages(sf::st_read(kml_path, quiet = TRUE, stringsAsFactors = FALSE))
omi_sf <- suppressWarnings(sf::st_make_valid(omi_sf))
omi_sf <- suppressWarnings(sf::st_zm(omi_sf, drop = TRUE, what = "ZM"))

# I driver KML/GDAL non espongono sempre i campi ExtendedData nello stesso modo.
# Nel file OMI il codice zona e CODZONA, ma in alcune installazioni di sf viene
# importato soltanto il campo standard Name (es. "NAPOLI - Zona OMI D31").
zone_field_candidates <- c(
  "CODZONA", "codzona", "COD_ZONA", "cod_zona", "ZONA", "zona"
)
zone_field <- zone_field_candidates[zone_field_candidates %in% names(omi_sf)]

if (length(zone_field) >= 1L) {
  zone_field <- zone_field[1L]
  omi_sf$zona <- str_to_upper(str_squish(as.character(omi_sf[[zone_field]])))
} else if ("Name" %in% names(omi_sf)) {
  omi_sf$zona <- stringr::str_match(
    as.character(omi_sf$Name),
    "(?i)Zona\\s+OMI\\s+([A-Z][0-9]+)"
  )[, 2]
  omi_sf$zona <- str_to_upper(str_squish(omi_sf$zona))
} else if ("name" %in% names(omi_sf)) {
  omi_sf$zona <- stringr::str_match(
    as.character(omi_sf$name),
    "(?i)Zona\\s+OMI\\s+([A-Z][0-9]+)"
  )[, 2]
  omi_sf$zona <- str_to_upper(str_squish(omi_sf$zona))
} else {
  stop(
    paste0(
      "Impossibile ricavare il codice zona dal KML. Colonne lette da sf: ",
      paste(names(omi_sf), collapse = ", ")
    ),
    call. = FALSE
  )
}

assert_v10(
  sum(!is.na(omi_sf$zona)) >= 60L,
  paste0(
    "Dal KML sono stati ricavati meno di 60 codici zona. Colonne lette: ",
    paste(names(omi_sf), collapse = ", ")
  )
)

omi_sf <- omi_sf |>
  filter(zona %in% unique(omi$zona)) |>
  select(zona, geometry) |>
  left_join(omi |> distinct(zona, fascia), by = "zona")
assert_v10(nrow(omi_sf) == 60L && !anyDuplicated(omi_sf$zona), "KML filtrato non contiene 60 zone uniche.")

omi_metric <- st_transform(omi_sf, 32633)
rep_points <- suppressWarnings(st_point_on_surface(omi_metric))
b_core <- omi_metric |> filter(fascia == "B") |> summarise(geometry = st_union(geometry))
b_core_center <- st_centroid(b_core)

zone_geography <- omi_metric |>
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    distance_to_B_core_km = as.numeric(st_distance(rep_points, b_core_center)) / 1000
  ) |>
  st_drop_geometry() |>
  select(zona, fascia, area_km2, distance_to_B_core_km)

rep_wgs <- st_transform(rep_points, 4326)
coords <- st_coordinates(rep_wgs)
zone_geography$rep_lon <- coords[, 1]
zone_geography$rep_lat <- coords[, 2]

# Lista di vicinato per il panel bilanciato. Una tolleranza di 1 metro evita
# che microscopici gap di digitalizzazione del KML spezzino adiacenze reali.
omi_metric_bal <- omi_metric |> filter(zona %in% balanced_zones)
buffered_bal <- suppressWarnings(st_buffer(omi_metric_bal, dist = 0.5))
touches <- st_intersects(buffered_bal)
zone_neighbors <- bind_rows(lapply(seq_along(touches), function(i) {
  js <- touches[[i]]
  js <- js[js > i]
  if (!length(js)) return(NULL)
  data.frame(zona_1 = omi_metric_bal$zona[i], zona_2 = omi_metric_bal$zona[js], stringsAsFactors = FALSE)
}))

# ------------------------------------------------------------------------------
# 3. ISTAT 2011 -> caratteristiche preesistenti per zona OMI
# ------------------------------------------------------------------------------
find_input_file <- function(filename) {
  candidates <- c(
    file.path(INPUT_DIR, filename), file.path(PROJECT_DIR, filename),
    file.path(INPUT_DIR, "istat2011", filename), file.path(PROJECT_DIR, "istat2011", filename)
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("File ISTAT mancante: ", filename, call. = FALSE)
  normalizePath(hit[1], winslash = "/", mustWork = TRUE)
}

istat_data_zip <- find_input_file("dati-cpa_2011.zip")
istat_shape_zip <- find_input_file("R15_11_WGS84.zip")

members <- utils::unzip(istat_data_zip, list = TRUE)$Name
istat_member <- members[basename(members) == "R15_indicatori_2011_sezioni.csv"]
assert_v10(length(istat_member) == 1L, "R15_indicatori_2011_sezioni.csv non trovato univocamente.")
istat11 <- read_semicolon_zip(istat_data_zip, istat_member, skip = 0L)

istat_required <- c(
  "PROCOM", "SEZ2011", "P1", "P46", "P47", "P60", "P61", "P128",
  "A2", "A3", "A6", "A44", "A46", "A47", "A48", "PF1", "PF2", "PF3"
)
assert_v10(all(istat_required %in% names(istat11)), "Variabili ISTAT 2011 mancanti.")
count_vars <- setdiff(istat_required, c("PROCOM", "SEZ2011"))
istat11 <- istat11 |>
  filter(as.numeric(PROCOM) == 63049) |>
  transmute(SEZ2011 = id_character(SEZ2011), across(all_of(count_vars), as.numeric))
assert_v10(nrow(istat11) > 3000L && !anyDuplicated(istat11$SEZ2011), "Sezioni ISTAT Napoli 2011 non plausibili.")

shape_dir <- file.path(PROCESSED_DIR, "istat2011_shape")
dir.create(shape_dir, recursive = TRUE, showWarnings = FALSE)
utils::unzip(istat_shape_zip, exdir = shape_dir, overwrite = TRUE)
shp <- list.files(shape_dir, pattern = "R15_11_WGS84\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
assert_v10(length(shp) == 1L, "Shapefile ISTAT 2011 non univoco.")
sections <- suppressMessages(st_read(shp, quiet = TRUE, stringsAsFactors = FALSE)) |>
  filter(as.numeric(PRO_COM) == 63049) |>
  mutate(SEZ2011 = id_character(SEZ2011)) |>
  select(SEZ2011) |>
  left_join(istat11, by = "SEZ2011") |>
  filter(!is.na(P1))
sections <- suppressWarnings(st_make_valid(sections))

sections_metric <- st_transform(sections, 32633)
section_points <- suppressWarnings(st_point_on_surface(sections_metric))
assigned_raw <- suppressWarnings(st_join(section_points, omi_metric |> select(zona), join = st_within, left = FALSE))

dup_ids <- assigned_raw |> st_drop_geometry() |> count(SEZ2011, name = "n") |> filter(n > 1L) |> pull(SEZ2011)
if (length(dup_ids)) {
  overlaps <- suppressWarnings(st_intersection(
    sections_metric |> filter(SEZ2011 %in% dup_ids) |> select(SEZ2011),
    omi_metric |> select(zona)
  )) |>
    mutate(overlap_m2 = as.numeric(st_area(geometry))) |>
    st_drop_geometry() |>
    arrange(SEZ2011, desc(overlap_m2), zona)
  chosen <- overlaps |> group_by(SEZ2011) |> slice(1L) |> ungroup() |> select(SEZ2011, zona)
  assigned_unique <- assigned_raw |> st_drop_geometry() |> filter(!SEZ2011 %in% dup_ids)
  assigned_dup <- sections |> st_drop_geometry() |> filter(SEZ2011 %in% dup_ids) |> left_join(chosen, by = "SEZ2011")
  assigned <- bind_rows(assigned_unique, assigned_dup)
} else {
  assigned <- st_drop_geometry(assigned_raw)
}

assert_v10(!anyDuplicated(assigned$SEZ2011), "Sezioni ISTAT duplicate dopo overlay.")
coverage_pop <- sum(assigned$P1, na.rm = TRUE) / sum(istat11$P1, na.rm = TRUE)
coverage_fam <- sum(assigned$PF1, na.rm = TRUE) / sum(istat11$PF1, na.rm = TRUE)
zone_share <- mean(unique(omi$zona) %in% unique(assigned$zona))
assert_v10(coverage_pop >= 0.90 && coverage_fam >= 0.90 && zone_share == 1,
           "Copertura overlay ISTAT-OMI insufficiente.")

istat_zone <- assigned |>
  group_by(zona) |>
  summarise(across(all_of(count_vars), ~sum(.x, na.rm = TRUE)), n_sezioni = n_distinct(SEZ2011), .groups = "drop") |>
  mutate(
    quota_affitto_2011 = safe_ratio(A46, PF1),
    quota_proprieta_2011 = safe_ratio(A47, PF1),
    quota_altro_titolo_2011 = safe_ratio(A48, PF1),
    # A44 = superficie abitazioni occupate; PF2 = componenti delle famiglie residenti.
    spazio_mq_procapite_2011 = safe_ratio(A44, PF2),
    persone_per_abitazione_2011 = safe_ratio(PF2, A2),
    dimensione_familiare_2011 = safe_ratio(PF2, PF1),
    quota_unipersonali_2011 = safe_ratio(PF3, PF1),
    quota_vuote_2011 = safe_ratio(A6, A2 + A6),
    quota_terziaria_2011 = safe_ratio(P47, P46),
    quota_occupati_15plus_2011 = safe_ratio(P61, P60 + P128)
  )

ref_bal <- istat_zone$zona %in% balanced_zones
istat_zone <- istat_zone |>
  mutate(
    z_quota_affitto_2011 = zscore_ref(quota_affitto_2011, ref_bal),
    z_basso_spazio_2011 = zscore_ref(-spazio_mq_procapite_2011, ref_bal),
    z_quota_vuote_2011 = zscore_ref(quota_vuote_2011, ref_bal),
    indice_pressione_abitativa_2011 = (z_quota_affitto_2011 + z_basso_spazio_2011) / 2
  )

tenure_gap_rate <- sum(abs(istat_zone$PF1 - (istat_zone$A46 + istat_zone$A47 + istat_zone$A48)), na.rm = TRUE) /
  sum(istat_zone$PF1, na.rm = TRUE)
assert_v10(tenure_gap_rate < 0.001, "Titoli di godimento ISTAT non coerenti con PF1.")

# ------------------------------------------------------------------------------
# 4. Master definitivo: 59 zone x 17 semestri = 1.003 righe
# ------------------------------------------------------------------------------
master <- omi_bal |>
  left_join(zone_geography, by = c("zona", "fascia")) |>
  left_join(istat_zone, by = "zona") |>
  arrange(zona, period_index)

assert_v10(nrow(master) == 1003L, "Master finale diverso da 59 x 17.")
assert_v10(all(is.finite(master$quota_affitto_2011)), "ISTAT 2011 mancante nel master.")

# ------------------------------------------------------------------------------
# 5. NTN comunale: contesto, non identificazione zonale
# ------------------------------------------------------------------------------
period_from_ntn_zip <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  member <- members[grepl("_VALORI-RES\\.csv$", members, ignore.case = TRUE)]
  if (length(member) != 1L) return(NA_integer_)
  m <- str_match(basename(member), "_(20[0-9]{2})_VALORI-RES\\.csv$")
  if (is.na(m[1, 1])) return(NA_integer_)
  as.integer(m[1, 2])
}
ntn_index <- data.frame(zip_path = NTN_CANDIDATE_ZIPS,
                        anno = vapply(NTN_CANDIDATE_ZIPS, period_from_ntn_zip, integer(1))) |>
  filter(anno %in% 2016:2024)
assert_v10(setequal(ntn_index$anno, 2016:2024) && nrow(ntn_index) == 9L, "NTN 2016-2024 incompleto.")

read_ntn <- function(zip_path) {
  members <- utils::unzip(zip_path, list = TRUE)$Name
  member <- members[grepl("_VALORI-RES\\.csv$", members, ignore.case = TRUE)]
  year <- period_from_ntn_zip(zip_path)
  raw <- read_semicolon_zip(zip_path, member, skip = 0L)
  code_col <- names(raw)[grepl("COD_?COM|CODCOM", names(raw), ignore.case = TRUE)]
  ntn_cols <- names(raw)[grepl("^NTN", trimws(names(raw)), ignore.case = TRUE)]
  assert_v10(length(code_col) == 1L && length(ntn_cols) == 6L, "Struttura file NTN inattesa.")
  row <- raw[str_to_upper(str_squish(as.character(raw[[code_col]]))) == "F839", , drop = FALSE]
  assert_v10(nrow(row) == 1L, paste0("Napoli non univoca nel NTN ", year))
  vals <- vapply(ntn_cols, function(v) parse_number_it(row[[v]][1]), numeric(1))
  data.frame(
    anno = year, ntn_fino_50 = vals[1], ntn_50_85 = vals[2], ntn_85_115 = vals[3],
    ntn_115_145 = vals[4], ntn_oltre_145 = vals[5], ntn_totale = vals[6]
  )
}
ntn <- bind_rows(lapply(ntn_index$zip_path, read_ntn)) |>
  arrange(anno) |>
  mutate(variazione_annua_ntn = ntn_totale / lag(ntn_totale) - 1)

# ------------------------------------------------------------------------------
# 6. Audit finale e salvataggi
# ------------------------------------------------------------------------------
audit_summary <- data.frame(
  controllo = c(
    "Periodo OMI", "Semestri OMI", "Zone tipologia 20 unione", "Zone pannello bilanciato",
    "Zona esclusa dal balanced panel", "Osservazioni master",
    "Superficie locazione 2018+", "Superficie vendita 2018+",
    "Copertura popolazione ISTAT 2011", "Copertura famiglie ISTAT 2011",
    "Zone OMI coperte ISTAT 2011", "Scarto tenure ISTAT 2011", "2024-2 incluso?"
  ),
  valore = c(
    "2016-1 / 2024-1", length(EXPECTED_SEMESTERS), n_distinct(omi$zona), length(balanced_zones),
    paste(excluded_zones$zona, collapse = ","), nrow(master),
    paste(sort(unique(omi_bal$superficie_loc[omi_bal$anno >= 2018])), collapse = ","),
    paste(sort(unique(omi_bal$superficie_compr[omi_bal$anno >= 2018])), collapse = ","),
    sprintf("%.4f%%", 100 * coverage_pop), sprintf("%.4f%%", 100 * coverage_fam),
    sprintf("%.1f%%", 100 * zone_share), sprintf("%.6f%%", 100 * tenure_gap_rate), "NO"
  ),
  esito = c("PASS", "PASS", "PASS", "PASS", "INFO", "PASS", "PASS", "PASS",
            "PASS", "PASS", "PASS", "PASS", "PASS"),
  stringsAsFactors = FALSE
)

profile_by_band <- master |>
  distinct(zona, fascia, quota_affitto_2011, spazio_mq_procapite_2011,
           quota_vuote_2011, quota_terziaria_2011, quota_occupati_15plus_2011) |>
  group_by(fascia) |>
  summarise(
    n_zone = n(),
    quota_affitto_2011 = mean(quota_affitto_2011),
    spazio_mq_procapite_2011 = mean(spazio_mq_procapite_2011),
    quota_vuote_2011 = mean(quota_vuote_2011),
    quota_terziaria_2011 = mean(quota_terziaria_2011),
    quota_occupati_15plus_2011 = mean(quota_occupati_15plus_2011),
    .groups = "drop"
  )

# Dizionario essenziale: serve a impedire interpretazioni errate nelle fasi successive.
data_dictionary <- data.frame(
  variabile = c(
    "loc_medio", "compr_medio", "log_rent", "log_sale", "rent_sale_ratio_annualized",
    "log_rent_sale_gap", "d_log_rent", "d_log_sale", "d_log_rent_sale_gap",
    "sample_levels_2018plus", "quota_affitto_2011", "spazio_mq_procapite_2011",
    "quota_vuote_2011", "quota_terziaria_2011", "quota_occupati_15plus_2011",
    "distance_to_B_core_km", "DE_peripheral", "break_2019_2", "catchup_2023_2"
  ),
  definizione = c(
    "Punto medio della forchetta OMI di locazione, euro/mq/mese",
    "Punto medio della forchetta OMI di compravendita, euro/mq",
    "Logaritmo di loc_medio", "Logaritmo di compr_medio",
    "12*loc_medio/compr_medio; descrittivo, interpretabile in livello soprattutto dal 2018 quando le superfici sono omogenee",
    "log(12*loc_medio)-log(compr_medio)",
    "Variazione semestrale di log_rent; 2018-1 impostata NA per cambio N->L",
    "Variazione semestrale di log_sale",
    "Variazione semestrale del log rent-sale gap; 2018-1 impostata NA",
    "1 dal 2018-1: locazione e vendita entrambe su superficie L",
    "Famiglie in affitto / famiglie totali, Censimento 2011",
    "Superficie abitazioni occupate / componenti famiglie residenti, Censimento 2011",
    "Abitazioni vuote / (abitazioni occupate + vuote), Censimento 2011",
    "Residenti con istruzione terziaria / residenti di 6 anni e piu, Censimento 2011",
    "Occupati 15+ / popolazione 15+ ricostruita da FL + NFL, Censimento 2011",
    "Distanza del punto rappresentativo della zona dal centroide dell'unione delle zone B; robustness, non main treatment",
    "1 per fasce D/E", "1 solo nel semestre 2019-2", "1 solo nel semestre 2023-2"
  ),
  fonte = c(rep("OMI - Agenzia delle Entrate", 10), rep("ISTAT Censimento 2011", 5),
            "KML OMI 2024-1", "OMI", "Costruita", "Costruita"),
  stringsAsFactors = FALSE
)

write_csv_v10(master, "master_naples_housing_panel.csv")
write_csv_v10(omi_audit, "audit_omi_semesters.csv")
write_csv_v10(audit_summary, "audit_summary.csv")
write_csv_v10(excluded_zones, "excluded_zones.csv")
write_csv_v10(zone_geography, "zone_geography.csv")
write_csv_v10(zone_neighbors, "zone_neighbors.csv")
write_csv_v10(istat_zone, "istat2011_zone_characteristics.csv")
write_csv_v10(profile_by_band, "baseline_profile_by_band.csv")
write_csv_v10(ntn, "ntn_napoli_2016_2024.csv")
write_csv_v10(data_dictionary, "data_dictionary.csv")

saveRDS(master, file.path(PROCESSED_DIR, "master_naples_housing_panel.rds"))
saveRDS(omi_sf, file.path(PROCESSED_DIR, "omi_zones_2024_1.rds"))
suppressMessages(st_write(
  omi_sf |> left_join(istat_zone, by = "zona"),
  file.path(OUTPUT_DIR, "zones_omi_istat2011.gpkg"),
  delete_dsn = TRUE, quiet = TRUE
))

message("V10 data build completato.")
message("Master: ", nrow(master), " righe; ", n_distinct(master$zona), " zone; ", n_distinct(master$semestre), " semestri.")
message("2024-2 escluso per disegno.")
