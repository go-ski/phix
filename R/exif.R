# ============================================================================
#  exif.R — ExifTool interface: read and write photo metadata
#
#  Extensibility: add new write wrappers (write_caption, write_copyright, …)
#  by following the write_gps / write_datetime pattern and extending
#  write_metadata() with additional argument branches.
# ============================================================================

# Read GPS + orientation for a vector of paths in one ExifTool call.
# Returns a data frame: path, name, lat, lng, orient, datetime.
read_meta <- function(paths) {
  out <- data.frame(
    path     = paths,
    name     = basename(paths),
    lat      = NA_real_,
    lng      = NA_real_,
    orient   = NA_integer_,
    datetime = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )
  if (!length(paths)) return(out)

  d <- tryCatch(
    exiftoolr::exif_read(
      paths,
      tags = c("GPSLatitude", "GPSLongitude",
               "GPSLatitudeRef", "GPSLongitudeRef", "Orientation",
               "DateTimeOriginal", "SubSecTimeOriginal"),
      args = "-n"          # -n => numeric (decimal degrees) instead of strings
    ),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(out)

  getcol <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))
  glat <- suppressWarnings(as.numeric(getcol("GPSLatitude")))
  glng <- suppressWarnings(as.numeric(getcol("GPSLongitude")))
  rlat <- as.character(getcol("GPSLatitudeRef"))
  rlng <- as.character(getcol("GPSLongitudeRef"))
  ornt <- suppressWarnings(as.integer(getcol("Orientation")))

  # Parse DateTimeOriginal ("YYYY:MM:DD HH:MM:SS") into POSIXct.
  dto_raw <- as.character(getcol("DateTimeOriginal"))
  dto_clean <- sub("^(\\d{4}):(\\d{2}):(\\d{2})", "\\1-\\2-\\3", dto_raw)
  dto <- suppressWarnings(
    as.POSIXct(dto_clean, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
  # Attach sub-second offset when available.
  subsec <- suppressWarnings(as.numeric(getcol("SubSecTimeOriginal")))
  valid_ss <- !is.na(subsec) & !is.na(dto)
  dto[valid_ss] <- dto[valid_ss] + subsec[valid_ss] / 10^nchar(
    as.character(as.integer(subsec[valid_ss]))
  )

  # Hemisphere: magnitude * sign from N/S/E/W ref. If ref is absent, keep
  # whatever sign ExifTool already returned (covers composite-style values).
  lat <- abs(glat); lng <- abs(glng)
  s <- !is.na(rlat) & toupper(substr(rlat, 1, 1)) == "S"; lat[s] <- -lat[s]
  w <- !is.na(rlng) & toupper(substr(rlng, 1, 1)) == "W"; lng[w] <- -lng[w]
  lat[is.na(rlat)] <- glat[is.na(rlat)]
  lng[is.na(rlng)] <- glng[is.na(rlng)]

  src <- normalizePath(d$SourceFile, winslash = "/", mustWork = FALSE)
  key <- normalizePath(paths,        winslash = "/", mustWork = FALSE)
  m <- match(key, src)
  out$lat      <- lat[m]
  out$lng      <- lng[m]
  out$orient   <- ornt[m]
  out$datetime <- dto[m]
  out
}

# Write GPS coordinates and/or a creation datetime to a photo in ONE ExifTool
# call.  Pass `gps` as list(lat, lng) to write the location tags, and/or `dt`
# as a POSIXct to write the three date/time tags.  Supplying both applies all
# tags in a single exif_call() (one process launch, one file rewrite) instead
# of two.  Supplying neither is a no-op.  ExifTool expects a colon-separated
# date ("YYYY:MM:DD HH:MM:SS").
write_metadata <- function(path, gps = NULL, dt = NULL) {
  args <- character(0)
  if (!is.null(gps)) {
    latref <- if (gps$lat >= 0) "N" else "S"
    lngref <- if (gps$lng >= 0) "E" else "W"
    args <- c(args,
      sprintf("-GPSLatitude=%.8f",  abs(gps$lat)),
      sprintf("-GPSLatitudeRef=%s", latref),
      sprintf("-GPSLongitude=%.8f", abs(gps$lng)),
      sprintf("-GPSLongitudeRef=%s", lngref),
      "-GPSMapDatum=WGS-84"
    )
  }
  if (!is.null(dt)) {
    stopifnot(inherits(dt, "POSIXct"))
    stamp <- format(dt, "%Y:%m:%d %H:%M:%S", tz = "UTC")
    args <- c(args,
      sprintf("-DateTimeOriginal=%s", stamp),
      sprintf("-CreateDate=%s",       stamp),
      sprintf("-ModifyDate=%s",       stamp)
    )
  }
  if (!length(args)) return(invisible(FALSE))   # nothing to write
  exiftoolr::exif_call(
    args = c(args,
      "-overwrite_original",   # edit in place, no _original backup file
      "-P"                     # preserve filesystem modify time
    ),
    path = path
  )
  invisible(TRUE)
}

# Thin wrappers preserving the original single-purpose call sites.
write_gps <- function(path, lat, lng) {
  write_metadata(path, gps = list(lat = lat, lng = lng))
}
write_datetime <- function(path, dt) {
  write_metadata(path, dt = dt)
}
