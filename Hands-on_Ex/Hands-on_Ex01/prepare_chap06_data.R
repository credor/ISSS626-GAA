# =============================================================================
# Hands-on Exercise 6 — Spatio-Temporal Point Patterns Analysis
# Data preparation
#
# Reads the two raw downloads and writes the two analysis-ready inputs, using
# the data/aspatial + data/geospatial convention from Hands-on Ex01 and Ex02
# rather than the chapter's data/rawdata.
#
# Run once. After that the .qmd reads only the outputs.
# =============================================================================

pacman::p_load(sf, tidyverse)

# ---- Paths ------------------------------------------------------------------
# raw_* are the files exactly as downloaded. Change these two names if yours
# differ; everything else follows from them.

raw_fire  <- "data/aspatial/fire_archive_M-C61_803250.csv"
raw_admin <- "data/geospatial/Batas_Wilayah_KelurahanDesa_10K_AR.shp"

out_fire  <- "data/aspatial/forestfires.csv"
out_admin <- "data/geospatial/Kepulauan_Bangka_Belitung.shp"

stopifnot(file.exists(raw_fire), file.exists(raw_admin))

# =============================================================================
# STEP 1 — Extract the Kepulauan Bangka Belitung sub-districts
# =============================================================================
# The source covers all 38 provinces — roughly 80,000+ kelurahan polygons at
# 1:10,000. Reading it all in to keep ~300 is slow and memory-hungry, so push
# the filter down to GDAL and read only the matching rows.

layer_name <- tools::file_path_sans_ext(basename(raw_admin))

kbb <- try(
  st_read(raw_admin,
          query = sprintf(
            "SELECT * FROM \"%s\" WHERE UPPER(WADMPR) LIKE '%%BANGKA%%'",
            layer_name),
          quiet = TRUE),
  silent = TRUE)

if (inherits(kbb, "try-error") || nrow(kbb) == 0) {
  message("GDAL query returned nothing — reading the full layer instead. ",
          "This will take a few minutes.")
  admin_all <- st_read(raw_admin, quiet = TRUE)

  prov_col <- intersect(c("WADMPR", "PROVINSI", "NAME_1"),
                        names(admin_all))[1]
  if (is.na(prov_col)) {
    print(names(admin_all))
    stop("No province column recognised. Set prov_col manually.")
  }
  kbb <- admin_all %>%
    filter(grepl("bangka", .data[[prov_col]], ignore.case = TRUE))
}

kbb <- st_make_valid(kbb)

cat("Sub-district features extracted:", nrow(kbb),
    " (chapter reports 297)\n")

# ---- Verify the extraction caught BOTH islands ------------------------------
# Matching "bangka" against the PROVINCE field is safe — Kepulauan Bangka
# Belitung is the only province containing that string. Matching it against a
# KABUPATEN field would not be: you would keep Bangka, Bangka Barat, Bangka
# Tengah and Bangka Selatan while silently dropping Belitung, Belitung Timur
# and Pangkalpinang, losing the whole second island. The feature count would
# still look plausible, so check the kabupaten list instead.

kab_col <- intersect(c("WADMKK", "KABKOT", "NAME_2"), names(kbb))[1]

if (!is.na(kab_col)) {
  cat("\nKabupaten/kota present (expect 7):\n")
  print(sort(unique(kbb[[kab_col]])))

  if (!any(grepl("belitung", kbb[[kab_col]], ignore.case = TRUE))) {
    stop("No Belitung kabupaten found — an island has been dropped.")
  }
}

cat("\nBounding box:\n")
print(st_bbox(kbb))
cat("Chapter: xmin 105.11  ymin -3.12  xmax 106.85  ymax -1.50\n")
cat("An xmax near 106.2 means you have Bangka only, missing Belitung.\n")

# Leave in WGS84. The .qmd transforms to EPSG:32748 itself, so transforming
# here as well would be a double transform.
st_write(kbb, out_admin, delete_dsn = TRUE, quiet = TRUE)
cat("\nWritten:", out_admin, "\n")

# =============================================================================
# STEP 2 — Clip the fire points to the province polygon
# =============================================================================
# A bounding box is not the province. Bangka and Belitung are islands, so the
# box you drew in FIRMS is mostly sea and also catches a strip of mainland
# Sumatra. MODIS picks up gas flares and vessel detections offshore. Clipping
# to the actual polygon is what "only forest fires within Kepulauan Bangka
# Belitung" means.

fire_raw <- read_csv(raw_fire, show_col_types = FALSE)
cat("\nRaw MODIS detections in the bounding box:", nrow(fire_raw), "\n")

fire_pts <- fire_raw %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

inside <- lengths(st_intersects(fire_pts, st_union(kbb))) > 0

cat("Inside the province polygon:", sum(inside), "\n")
cat("Dropped (sea, Sumatra, neighbouring provinces):", sum(!inside), "\n")

fire_kbb <- fire_pts[inside, ] %>%
  filter(acq_date >= as.Date("2023-01-01"),
         acq_date <= as.Date("2023-12-31")) %>%
  st_drop_geometry() %>%
  relocate(longitude, latitude)

write_csv(fire_kbb, out_fire)
cat("Written:", out_fire, "—", nrow(fire_kbb), "points\n")

# =============================================================================
# STEP 3 — Check against the chapter
# =============================================================================

chk_b <- st_read(out_admin, quiet = TRUE)
chk_f <- read_csv(out_fire, show_col_types = FALSE)

tibble(
  item = c("Sub-district features", "Fire points", "Boundary CRS",
           "Fire date range", "Geometry type"),
  yours = c(as.character(nrow(chk_b)),
            as.character(nrow(chk_f)),
            st_crs(chk_b)$input,
            paste(range(chk_f$acq_date), collapse = " to "),
            as.character(unique(st_geometry_type(chk_b))[1])),
  chapter = c("297", "741", "WGS 84", "2023-01-10 to 2023-12-18", "POLYGON")
) %>% print(n = Inf)

# Confidence distribution — if Prof Kam filtered on confidence and did not say
# so, that is the most likely reason your point count differs from 741.
if ("confidence" %in% names(chk_f)) {
  cat("\nConfidence distribution of the retained points:\n")
  print(summary(chk_f$confidence))
}

cat("\nIf counts differ, state the number you got, the retrieval date, the\n")
cat("bounding box and the FIRMS product. A documented difference reads\n")
cat("better than an unexplained match.\n")
