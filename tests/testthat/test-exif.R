test_that("read_meta returns correct zero-row structure for empty input", {
  result <- read_meta(character(0))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result,
               c("path", "name", "lat", "lng", "orient", "datetime", "tz_offset"))
  expect_type(result$path,     "character")
  expect_type(result$name,     "character")
  expect_type(result$lat,      "double")
  expect_type(result$lng,      "double")
  expect_type(result$orient,   "integer")
  expect_s3_class(result$datetime, "POSIXct")
  expect_type(result$tz_offset, "integer")
})

test_that("write_metadata with no gps and no dt returns FALSE invisibly", {
  result <- write_metadata("/nonexistent/dummy.jpg")
  expect_false(result)
})

test_that("write_gps delegates to write_metadata (no-op on bad path guarded by exiftoolr)", {
  # write_metadata itself returns FALSE only when args is empty (no gps, no dt).
  # With gps supplied it returns TRUE after calling exiftoolr; we don't call
  # exiftoolr here, just confirm the function exists and has the right formals.
  expect_true(is.function(write_gps))
  expect_identical(names(formals(write_gps)), c("path", "lat", "lng"))
})

test_that("write_datetime requires POSIXct", {
  expect_error(
    write_metadata("/tmp/dummy.jpg", dt = "2025-01-01"),
    class = "simpleError"
  )
})

test_that("any write stamps ModifyDate and OffsetTime (GPS-only included)", {
  captured <- NULL
  testthat::local_mocked_bindings(
    exif_call = function(args, path) { captured <<- args; invisible(TRUE) },
    .package = "exiftoolr"
  )
  write_metadata("dummy.jpg", gps = list(lat = 45, lng = 9))
  expect_true(any(grepl("^-ModifyDate=", captured)))
  expect_true("-OffsetTime=+00:00" %in% captured)
  # GPS-only write must not write capture-time offset tags.
  expect_false(any(grepl("^-OffsetTimeOriginal=", captured)))
})

test_that("datetime write applies tz_offset to wall clock and offset tags", {
  captured <- NULL
  testthat::local_mocked_bindings(
    exif_call = function(args, path) { captured <<- args; invisible(TRUE) },
    .package = "exiftoolr"
  )
  # 12:30 UTC at +02:00 => 14:30 local wall clock.
  write_metadata(
    "dummy.jpg",
    dt = as.POSIXct("1975-08-14 12:30:00", tz = "UTC"),
    tz_offset = 7200
  )
  expect_true("-DateTimeOriginal=1975:08:14 14:30:00" %in% captured)
  expect_true("-CreateDate=1975:08:14 14:30:00" %in% captured)
  expect_true("-OffsetTimeOriginal=+02:00" %in% captured)
  expect_true("-OffsetTimeDigitized=+02:00" %in% captured)
})

test_that("datetime write without tz_offset omits capture-time offset tags", {
  captured <- NULL
  testthat::local_mocked_bindings(
    exif_call = function(args, path) { captured <<- args; invisible(TRUE) },
    .package = "exiftoolr"
  )
  write_metadata(
    "dummy.jpg",
    dt = as.POSIXct("1975-08-14 14:30:00", tz = "UTC")
  )
  expect_true("-DateTimeOriginal=1975:08:14 14:30:00" %in% captured)
  expect_false(any(grepl("^-OffsetTimeOriginal=", captured)))
})

test_that("build_metadata_args returns character(0) when nothing to write", {
  expect_identical(build_metadata_args(), character(0))
})

test_that("build_metadata_args formats GPS tags with auto-stamps", {
  args <- build_metadata_args(gps = list(lat = 45.5, lng = -9.25))
  expect_true("-GPSLatitude=45.50000000" %in% args)
  expect_true("-GPSLatitudeRef=N" %in% args)
  expect_true("-GPSLongitude=9.25000000" %in% args)
  expect_true("-GPSLongitudeRef=W" %in% args)
  expect_true("-GPSMapDatum=WGS-84" %in% args)
  # Auto-stamps always present on a non-empty write.
  expect_true(any(grepl("^-ModifyDate=", args)))
  expect_true("-OffsetTime=+00:00" %in% args)
  # No date supplied => no capture-time tags.
  expect_false(any(grepl("^-DateTimeOriginal=", args)))
})

test_that("build_metadata_args formats datetime tags with offset", {
  args <- build_metadata_args(
    dt = as.POSIXct("1975-08-14 12:30:00", tz = "UTC"),
    tz_offset = 7200
  )
  expect_true("-DateTimeOriginal=1975:08:14 14:30:00" %in% args)
  expect_true("-CreateDate=1975:08:14 14:30:00" %in% args)
  expect_true("-OffsetTimeOriginal=+02:00" %in% args)
  expect_true("-OffsetTimeDigitized=+02:00" %in% args)
})

test_that("build_metadata_args matches what write_metadata passes to ExifTool", {
  captured <- NULL
  testthat::local_mocked_bindings(
    exif_call = function(args, path) { captured <<- args; invisible(TRUE) },
    .package = "exiftoolr"
  )
  gps <- list(lat = 45.5, lng = -9.25)
  # ModifyDate uses Sys.time(), so drop it before comparing.
  drop_modify <- function(a) a[!grepl("^-ModifyDate=", a)]
  write_metadata("dummy.jpg", gps = gps)
  expected <- c(build_metadata_args(gps = gps), "-overwrite_original", "-P")
  expect_identical(drop_modify(captured), drop_modify(expected))
})
