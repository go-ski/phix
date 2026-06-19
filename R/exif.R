# ============================================================================
#  exif.R — ExifTool interface: read and write photo metadata
#
#  Extensibility: add new write wrappers (write_caption, write_copyright, …)
#  by following the write_gps / write_datetime pattern and extending
#  write_metadata() with additional argument branches.
# ============================================================================

#' Read GPS, orientation, and datetime metadata from photos
#'
#' Calls ExifTool once for all `paths` and returns a data frame with one row
#' per path.  Missing or unreadable tags are returned as `NA`.
#'
#' @param paths A character vector of file paths.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{path}{Full file path (character).}
#'   \item{name}{Basename of the file (character).}
#'   \item{lat}{GPS latitude in decimal degrees, negative for South (numeric).}
#'   \item{lng}{GPS longitude in decimal degrees, negative for West (numeric).}
#'   \item{orient}{EXIF orientation value 1--8, or `NA` (integer).}
#'   \item{datetime}{`DateTimeOriginal` as UTC `POSIXct`, or `NA`.}
#' }
#'
#' @examples
#' \dontrun{
#' meta <- read_meta(list_photos("~/Pictures/scans"))
#' head(meta)
#' }
#'
#' @export
read_meta <- function(paths) {
  # Zero-row skeleton returned for empty input or on early exit.
  empty_frame <- function(n = 0L) {
    data.frame(
      path     = character(n),
      name     = character(n),
      lat      = numeric(n),
      lng      = numeric(n),
      orient   = integer(n),
      datetime = as.POSIXct(character(n)),
      stringsAsFactors = FALSE
    )
  }
  if (!length(paths)) return(empty_frame())
  out <- data.frame(
    path     = paths,
    name     = basename(paths),
    lat      = NA_real_,
    lng      = NA_real_,
    orient   = NA_integer_,
    datetime = as.POSIXct(NA_character_),
    stringsAsFactors = FALSE
  )

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
  if (is.null(d) || !nrow(d)) return(out)  # ExifTool returned nothing

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

  # Hemisphere: magnitude * sign from N/S/E/W ref.  If ref is absent, keep
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

#' Write GPS coordinates and/or a creation datetime to a photo
#'
#' Issues a single ExifTool call so that supplying both `gps` and `dt` results
#' in only one file rewrite.  Supplying neither is a no-op (returns `FALSE`
#' invisibly).
#'
#' @param path  A single character file path.
#' @param gps   A list with numeric elements `lat` and `lng` (decimal degrees,
#'   negative for South/West), or `NULL` to skip GPS.
#' @param dt    A `POSIXct` value for the original capture time (written to
#'   `DateTimeOriginal` and `CreateDate`; `ModifyDate` is set to the current
#'   UTC time), or `NULL` to skip datetime.
#'
#' @return `TRUE` invisibly when tags were written, `FALSE` invisibly when
#'   both `gps` and `dt` are `NULL`.
#'
#' @examples
#' \dontrun{
#' write_metadata(
#'   "scan001.jpg",
#'   gps = list(lat = 45.123, lng = 9.456),
#'   dt  = as.POSIXct("1975-08-14 14:30:00", tz = "UTC")
#' )
#' }
#'
#' @export
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
    stamp     <- format(dt,         "%Y:%m:%d %H:%M:%S", tz = "UTC")
    now_stamp <- format(Sys.time(), "%Y:%m:%d %H:%M:%S", tz = "UTC")
    args <- c(args,
      sprintf("-DateTimeOriginal=%s", stamp),
      sprintf("-CreateDate=%s",       stamp),
      sprintf("-ModifyDate=%s",       now_stamp)
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

#' Write GPS coordinates to a photo
#'
#' Thin wrapper around [write_metadata()] for GPS-only writes.
#'
#' @param path A single character file path.
#' @param lat  Latitude in decimal degrees (negative for South).
#' @param lng  Longitude in decimal degrees (negative for West).
#'
#' @return `TRUE` invisibly.
#'
#' @examples
#' \dontrun{
#' write_gps("scan001.jpg", lat = 45.123, lng = 9.456)
#' }
#'
#' @export
write_gps <- function(path, lat, lng) {
  write_metadata(path, gps = list(lat = lat, lng = lng))
}

#' Write a creation datetime to a photo
#'
#' Thin wrapper around [write_metadata()] for datetime-only writes.
#'
#' @param path A single character file path.
#' @param dt   A `POSIXct` value for the original capture time.
#'
#' @return `TRUE` invisibly.
#'
#' @examples
#' \dontrun{
#' write_datetime("scan001.jpg",
#'                dt = as.POSIXct("1975-08-14 14:30:00", tz = "UTC"))
#' }
#'
#' @export
write_datetime <- function(path, dt) {
  write_metadata(path, dt = dt)
}
