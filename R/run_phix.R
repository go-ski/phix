# ============================================================================
#  run_phix.R — App entry point
# ============================================================================

#' Launch the phix EXIF editor
#'
#' Opens the interactive Shiny application for editing GPS coordinates and
#' creation dates in scanned or digitised photos.
#'
#' @param ... Arguments passed to [shiny::shinyApp()], for example `options`
#'   (a list passed to [shiny::runApp()], e.g. `list(port = 3838)`).
#'
#' @details
#' **ExifTool requirement:** phix writes EXIF metadata via ExifTool.  If
#' ExifTool is not found on your system, install it once with:
#'
#' ```r
#' exiftoolr::install_exiftool()
#' ```
#'
#' **In-place editing:** all writes use `-overwrite_original` (no backup files
#' are created).  Back up your originals before editing.
#'
#' @return A [shiny::shinyApp()] object (invisibly when run interactively).
#'
#' @examples
#' \dontrun{
#' run_phix()
#' }
#'
#' @export
run_phix <- function(...) {
  # 1. Create and register temp directory for thumbnails + viewer HTML.
  thumb_dir <- file.path(tempdir(), "phix_thumbs")
  dir.create(thumb_dir, showWarnings = FALSE, recursive = TRUE)
  unlink(list.files(thumb_dir, full.names = TRUE))
  shiny::addResourcePath("thumbs", thumb_dir)
  .phix_env$THUMB_DIR <- thumb_dir

  # 2. Copy viewer HTML from inst/app/ to thumb_dir so it is served at
  #    /thumbs/viewer.html by the Shiny static-file handler.
  viewer_src <- system.file("app", "viewer.html", package = "phix")
  file.copy(viewer_src, file.path(thumb_dir, "viewer.html"), overwrite = TRUE)

  # 3. Launch.
  shiny::shinyApp(ui = app_ui(), server = app_server, ...)
}
