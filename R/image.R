# ============================================================================
#  image.R — Image utilities: orientation correction and thumbnail generation
# ============================================================================

# Bake EXIF orientation into the pixels (emulates exiftool/-auto-orient) so the
# re-encoded thumbnail always shows upright in the browser.
apply_orientation <- function(img, o) {
  if (is.na(o)) return(img)
  switch(as.character(o),
    "2" = magick::image_flop(img),
    "3" = magick::image_rotate(img, 180),
    "4" = magick::image_flip(img),
    "5" = magick::image_flop(magick::image_rotate(img, 90)),
    "6" = magick::image_rotate(img, 90),
    "7" = magick::image_flop(magick::image_rotate(img, 270)),
    "8" = magick::image_rotate(img, 270),
    img
  )
}

# Make a web-friendly thumbnail; returns its basename under THUMB_DIR (or NULL).
make_thumb <- function(path, orient, max_px = 1000) {
  img <- tryCatch(magick::image_read(path), error = function(e) NULL)
  if (is.null(img)) return(NULL)                # e.g. some RAW formats
  img <- apply_orientation(img, orient)
  img <- magick::image_scale(img, paste0(max_px, "x", max_px, ">"))  # shrink only
  img <- magick::image_strip(img)               # drop metadata incl. orientation
  out <- file.path(THUMB_DIR,
                   sprintf("t%d.jpg", as.integer(stats::runif(1, 1, 1e9))))
  magick::image_write(img, path = out, format = "jpeg", quality = 88)
  basename(out)
}
