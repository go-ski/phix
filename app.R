# ============================================================================
#  Photo EXIF Editor  —  add / correct GPS location and creation date in photos
#  ----------------------------------------------------------------------------
#  Intended use-case
#     Digitised or scanned photos (film prints, slides, negatives) often lack
#     GPS coordinates and have wrong or missing creation dates.  This app lets
#     you work through a folder of such images and write correct metadata
#     directly into each file's EXIF tags.
#
#  Run in Positron (or RStudio):
#     1. Open this file.
#     2. Click the "Run App" button, OR run:  shiny::runApp()
#     3. Paste a folder path into the app, click "Load photos".
#
#  File layout
#     app.R          — this file: dependencies, source() calls, shinyApp()
#     R/config.R     — constants, temp-dir setup, viewer HTML, list_photos()
#     R/exif.R       — read_meta(), write_metadata(), write_gps(), write_datetime()
#     R/image.R      — apply_orientation(), make_thumb()
#     R/geocode.R    — geocode_osm()
#     R/ui.R         — ui object
#     R/server.R     — server function
#
#  All writes use ExifTool via exiftoolr (-overwrite_original), so no
#  _original backup file is created.  Back up your originals first.
# ============================================================================

## ---- 1. Dependencies -------------------------------------------------------
required <- c("shiny", "bslib", "leaflet", "exiftoolr",
              "magick", "DT", "httr", "jsonlite")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

#library(shiny)
#library(bslib)
#library(leaflet)
#library(DT)

# ExifTool is a separate command-line program that exiftoolr drives.
# This installs a private copy the first time, if one isn't already on PATH.
if (is.null(tryCatch(exiftoolr::exif_version(), error = function(e) NULL))) {
  message("Installing ExifTool (one-time) ...")
  exiftoolr::install_exiftool()
}

## ---- 2. Source helper files ------------------------------------------------
source("R/config.R")   # constants, temp dir, viewer HTML, list_photos()
source("R/exif.R")     # read_meta(), write_metadata(), write_gps(), write_datetime()
source("R/image.R")    # apply_orientation(), make_thumb()
source("R/geocode.R")  # geocode_osm()
source("R/ui.R")       # ui
source("R/server.R")   # server

## ---- 3. Launch -------------------------------------------------------------
shiny::shinyApp(ui = ui, server = server)
