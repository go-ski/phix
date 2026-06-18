# ============================================================================
#  globals.R — Package-private environment for runtime state
#
#  .phix_env holds constants that are set once at package load (.onLoad) and
#  mutable state (THUMB_DIR) that is initialised at app launch (run_phix()).
#  Nothing in this environment is exported.
# ============================================================================

.phix_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Photo extensions scanned when listing a directory.
  .phix_env$PHOTO_EXT <- c(
    "jpg", "jpeg", "jpe", "tif", "tiff", "png", "webp",
    "heic", "heif", "dng", "cr2", "cr3", "nef", "arw",
    "orf", "raf", "rw2"
  )

  # Formats browsers can display natively — serve the original file for
  # these; everything else falls back to a magick-generated JPEG thumbnail.
  .phix_env$BROWSER_PHOTO_EXT <- c(
    "jpg", "jpeg", "jpe", "png", "webp", "heic", "heif"
  )

  # THUMB_DIR is set at runtime inside run_phix(), not here, because
  # shiny::addResourcePath() must not be called at package load time.
  .phix_env$THUMB_DIR <- NULL
}
