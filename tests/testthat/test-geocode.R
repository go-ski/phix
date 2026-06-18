test_that("geocode_osm returns NULL for empty query", {
  expect_null(geocode_osm(""))
  expect_null(geocode_osm("   "))
})
