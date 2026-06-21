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
#'   \item{datetime}{`DateTimeOriginal` as `POSIXct` in UTC, or `NA`. The UTC
#'     offset is resolved from, in order: `OffsetTimeOriginal`,
#'     `OffsetTimeDigitized`, `TimeZoneOffset` (integer hours), `GPSDateTime`
#'     delta (GPS time is always UTC), and GPS-coordinate timezone lookup via
#'     `lutz`. When none are available the camera's local clock time is
#'     returned with a UTC label.}
#'   \item{tz_offset}{The resolved capture-location UTC offset in signed integer
#'     seconds (e.g. `7200` for `+02:00`), or `NA` when no tier resolved an
#'     offset. The capture-local wall clock is `datetime + tz_offset`.}
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
      tz_offset = integer(n),
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
    tz_offset = NA_integer_,
    stringsAsFactors = FALSE
  )

  d <- tryCatch(
    exiftoolr::exif_read(
      paths,
      tags = c("GPSLatitude", "GPSLongitude",
               "GPSLatitudeRef", "GPSLongitudeRef", "Orientation",
               "DateTimeOriginal", "SubSecTimeOriginal",
               "OffsetTimeOriginal", "OffsetTimeDigitized",
               "TimeZoneOffset", "GPSDateTime"),
      args = "-n"          # -n => numeric (decimal degrees) instead of strings
    ),
    error = function(e) NULL
  )
  if (is.null(d) || !nrow(d)) return(out)  # ExifTool returned nothing

  getcol <- function(nm) if (nm %in% names(d)) d[[nm]] else rep(NA, nrow(d))

  # --- GPS coordinates -------------------------------------------------------
  glat <- suppressWarnings(as.numeric(getcol("GPSLatitude")))
  glng <- suppressWarnings(as.numeric(getcol("GPSLongitude")))
  rlat <- as.character(getcol("GPSLatitudeRef"))
  rlng <- as.character(getcol("GPSLongitudeRef"))
  ornt <- suppressWarnings(as.integer(getcol("Orientation")))

  # Hemisphere: magnitude * sign from N/S/E/W ref.  If ref is absent, keep
  # whatever sign ExifTool already returned (covers composite-style values).
  lat <- abs(glat); lng <- abs(glng)
  s <- !is.na(rlat) & toupper(substr(rlat, 1, 1)) == "S"; lat[s] <- -lat[s]
  w <- !is.na(rlng) & toupper(substr(rlng, 1, 1)) == "W"; lng[w] <- -lng[w]
  lat[is.na(rlat)] <- glat[is.na(rlat)]
  lng[is.na(rlng)] <- glng[is.na(rlng)]

  # --- Datetime: five-tier UTC offset resolution -----------------------------
  # Parse DateTimeOriginal ("YYYY:MM:DD HH:MM:SS") naively as UTC first;
  # the tiers below correct to true UTC wherever offset information exists.
  dto_raw   <- as.character(getcol("DateTimeOriginal"))
  dto_clean <- sub("^(\\d{4}):(\\d{2}):(\\d{2})", "\\1-\\2-\\3", dto_raw)
  dto <- suppressWarnings(
    as.POSIXct(dto_clean, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )

  # Helper: parse "+HH:MM" / "-HH:MM" strings into signed integer seconds.
  # Only the elements matching the pattern are processed to avoid subscript
  # errors from strsplit on empty or malformed strings.
  parse_offset <- function(s) {
    ok   <- grepl("^[+-]\\d{2}:\\d{2}$", s)
    secs <- rep(NA_integer_, length(s))
    if (!any(ok)) return(secs)
    sign  <- ifelse(startsWith(s[ok], "-"), -1L, 1L)
    parts <- strsplit(sub("^[+-]", "", s[ok]), ":")
    h     <- as.integer(vapply(parts, `[[`, "", 1L))
    m     <- as.integer(vapply(parts, `[[`, "", 2L))
    secs[ok] <- sign * (h * 3600L + m * 60L)
    secs
  }

  # Tier 1 & 2: OffsetTimeOriginal / OffsetTimeDigitized (EXIF 2.31, "+HH:MM").
  # OffsetTimeOriginal is preferred; OffsetTimeDigitized used as fallback.
  off1        <- parse_offset(as.character(getcol("OffsetTimeOriginal")))
  off2        <- parse_offset(as.character(getcol("OffsetTimeDigitized")))
  offset_secs <- ifelse(!is.na(off1), off1, off2)
  valid_off   <- !is.na(offset_secs) & !is.na(dto)
  dto[valid_off] <- dto[valid_off] - offset_secs[valid_off]

  # Tier 3: TimeZoneOffset (non-standard integer hours, e.g. -7 for UTC-7).
  tzo       <- suppressWarnings(as.integer(getcol("TimeZoneOffset")))
  needs_tzo <- is.na(offset_secs) & !is.na(tzo) & !is.na(dto)
  dto[needs_tzo]        <- dto[needs_tzo] - tzo[needs_tzo] * 3600L
  offset_secs[needs_tzo] <- tzo[needs_tzo] * 3600L

  # Tier 4: GPSDateTime delta. GPSDateTime is always UTC, so
  # DateTimeOriginal − GPSDateTime gives the camera's local offset. Round to
  # the nearest 15 min to absorb GPS-to-shutter jitter (all real UTC offsets
  # are multiples of 15 min).
  gps_raw   <- as.character(getcol("GPSDateTime"))
  gps_clean <- sub("Z$", "", sub("^(\\d{4}):(\\d{2}):(\\d{2})", "\\1-\\2-\\3", gps_raw))
  gps_utc   <- suppressWarnings(
    as.POSIXct(gps_clean, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  )
  needs_gps_delta <- is.na(offset_secs) & !is.na(gps_utc) & !is.na(dto)
  if (any(needs_gps_delta)) {
    raw_delta      <- as.numeric(dto[needs_gps_delta]) -
                      as.numeric(gps_utc[needs_gps_delta])
    rounded_offset <- round(raw_delta / 900) * 900L
    dto[needs_gps_delta] <- .POSIXct(
      as.numeric(dto[needs_gps_delta]) - rounded_offset, tz = "UTC"
    )
    offset_secs[needs_gps_delta] <- rounded_offset
  }

  # Tier 5: GPS-coordinate timezone lookup via lutz. Re-parse DateTimeOriginal
  # in the looked-up Olson zone; as.numeric() on the result yields the correct
  # UTC epoch seconds regardless of the display zone.
  needs_geo_tz <- is.na(offset_secs) & !is.na(dto) & !is.na(lat) & !is.na(lng)
  if (any(needs_geo_tz)) {
    tznames <- lutz::tz_lookup_coords(
      lat[needs_geo_tz], lng[needs_geo_tz], method = "fast", warn = FALSE
    )
    idx <- which(needs_geo_tz)
    for (i in seq_along(idx)) {
      tz_i <- tznames[i]
      if (!is.na(tz_i) && nzchar(tz_i) && tz_i %in% OlsonNames()) {
        dto_local    <- as.POSIXct(dto_clean[idx[i]],
                                   format = "%Y-%m-%d %H:%M:%S", tz = tz_i)
        # Offset = (local wall clock read as UTC) − (true UTC instant).
        offset_secs[idx[i]] <- as.integer(
          round(as.numeric(dto[idx[i]]) - as.numeric(dto_local))
        )
        dto[idx[i]] <- .POSIXct(as.numeric(dto_local), tz = "UTC")
      }
    }
  }

  # Sub-second offset. String-based parse preserves leading zeros
  # (e.g. "05" → 0.05 s, not 0.5 s as nchar(as.integer()) would give).
  subsec_raw  <- as.character(getcol("SubSecTimeOriginal"))
  subsec_frac <- suppressWarnings(as.numeric(paste0("0.", subsec_raw)))
  valid_ss    <- !is.na(subsec_frac) & !is.na(dto)
  dto[valid_ss] <- dto[valid_ss] + subsec_frac[valid_ss]

  src <- normalizePath(d$SourceFile, winslash = "/", mustWork = FALSE)
  key <- normalizePath(paths,        winslash = "/", mustWork = FALSE)
  m <- match(key, src)
  out$lat       <- lat[m]
  out$lng       <- lng[m]
  out$orient    <- ornt[m]
  out$datetime  <- dto[m]
  out$tz_offset <- as.integer(offset_secs[m])
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
#' @param dt    A `POSIXct` value (a true-UTC instant) for the original capture
#'   time, or `NULL` to skip datetime. When written, the capture-location wall
#'   clock (`dt + tz_offset`) is stored in `DateTimeOriginal` and `CreateDate`,
#'   and the matching offset in `OffsetTimeOriginal` / `OffsetTimeDigitized`.
#' @param tz_offset Signed integer seconds giving the capture-location UTC
#'   offset (e.g. `7200` for `+02:00`). When `NULL` or `NA`, `dt` is treated as
#'   the wall clock directly and no `OffsetTimeOriginal`/`OffsetTimeDigitized`
#'   tags are written (the offset stays unknown rather than being fabricated).
#'
#' @details
#' Any write also stamps `ModifyDate` with the current time in UTC and sets
#' `OffsetTime` to `+00:00`, so the modification time is unambiguous. This
#' happens on every write, including GPS-only writes.
#'
#' @return `TRUE` invisibly when tags were written, `FALSE` invisibly when
#'   both `gps` and `dt` are `NULL`.
#'
#' @examples
#' \dontrun{
#' write_metadata(
#'   "scan001.jpg",
#'   gps = list(lat = 45.123, lng = 9.456),
#'   dt  = as.POSIXct("1975-08-14 14:30:00", tz = "UTC"),
#'   tz_offset = 7200
#' )
#' }
#'
#' @export
write_metadata <- function(path, gps = NULL, dt = NULL, tz_offset = NULL) {
  # Format signed seconds as an EXIF "+HH:MM" / "-HH:MM" offset string.
  format_offset <- function(secs) {
    sign  <- if (secs < 0) "-" else "+"
    secs  <- abs(as.integer(round(secs)))
    sprintf("%s%02d:%02d", sign, secs %/% 3600L, (secs %% 3600L) %/% 60L)
  }

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
    have_off  <- !is.null(tz_offset) && !is.na(tz_offset)
    # Wall clock at the capture location = UTC instant shifted by the offset.
    local_dt  <- if (have_off) dt + as.numeric(tz_offset) else dt
    stamp     <- format(local_dt, "%Y:%m:%d %H:%M:%S", tz = "UTC")
    args <- c(args,
      sprintf("-DateTimeOriginal=%s", stamp),
      sprintf("-CreateDate=%s",       stamp)
    )
    if (have_off) {
      off_str <- format_offset(tz_offset)
      args <- c(args,
        sprintf("-OffsetTimeOriginal=%s",  off_str),
        sprintf("-OffsetTimeDigitized=%s", off_str)
      )
    }
  }
  if (!length(args)) return(invisible(FALSE))   # nothing to write

  # Any write updates ModifyDate to the current time, unambiguously in UTC.
  now_stamp <- format(Sys.time(), "%Y:%m:%d %H:%M:%S", tz = "UTC")
  args <- c(args,
    sprintf("-ModifyDate=%s", now_stamp),
    "-OffsetTime=+00:00"
  )

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
#' @param dt   A `POSIXct` value (a true-UTC instant) for the original capture
#'   time.
#' @param tz_offset Signed integer seconds for the capture-location UTC offset,
#'   or `NULL`/`NA` to leave the offset unknown. See [write_metadata()].
#'
#' @return `TRUE` invisibly.
#'
#' @examples
#' \dontrun{
#' write_datetime("scan001.jpg",
#'                dt = as.POSIXct("1975-08-14 14:30:00", tz = "UTC"),
#'                tz_offset = 7200)
#' }
#'
#' @export
write_datetime <- function(path, dt, tz_offset = NULL) {
  write_metadata(path, dt = dt, tz_offset = tz_offset)
}
