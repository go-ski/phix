test_that("list_photos returns character(0) for missing/invalid input", {
  expect_identical(list_photos(NA_character_),          character(0))
  expect_identical(list_photos(""),                     character(0))
  expect_identical(list_photos("/nonexistent/path/xyz"), character(0))
  expect_identical(list_photos(character(0)),            character(0))
  expect_identical(list_photos(c("/a", "/b")),           character(0)) # length != 1
})

test_that("list_photos finds jpg and jpg only, ignores txt and sub-dirs", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "photo1.jpg"))
  file.create(file.path(tmp, "photo2.JPG"))   # case-insensitive
  file.create(file.path(tmp, "notes.txt"))
  dir.create(file.path(tmp, "subdir"))

  result <- list_photos(tmp)
  expect_length(result, 2)
  expect_true(all(grepl("\\.jpg$", tolower(result))))
})

test_that("list_photos recognises all supported extensions", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  exts <- c("jpg", "jpeg", "jpe", "tif", "tiff", "png", "webp",
            "heic", "heif", "dng", "cr2", "cr3", "nef", "arw",
            "orf", "raf", "rw2")
  for (ext in exts) file.create(file.path(tmp, paste0("img.", ext)))

  result <- list_photos(tmp)
  expect_length(result, length(exts))
})
