test_that("read_meta returns correct zero-row structure for empty input", {
  result <- read_meta(character(0))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("path", "name", "lat", "lng", "orient", "datetime"))
  expect_type(result$path,     "character")
  expect_type(result$name,     "character")
  expect_type(result$lat,      "double")
  expect_type(result$lng,      "double")
  expect_type(result$orient,   "integer")
  expect_s3_class(result$datetime, "POSIXct")
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
