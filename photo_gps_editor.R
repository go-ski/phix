# ============================================================================
#  Photo GPS Editor  —  view / set / copy photo locations via EXIF
#  ----------------------------------------------------------------------------
#  Run in Positron (or RStudio):
#     1. Open this file.
#     2. Click the "Run App" button, OR run:  shiny::runApp("photo_gps_editor.R")
#     3. Paste a folder path into the app, click "Load photos".
#
#  What it does
#     * Lists every photo in a folder and shows its location (or "no-location").
#     * Shows the current photo + its point on a searchable world map.
#     * Click the map to choose a new location, then "Save" writes it to the
#       photo's EXIF and advances to the next photo.
#     * "Copy location" / "Paste & save" copies coordinates between photos.
#
#  Writing is done with ExifTool (the de-facto standard), wrapped by exiftoolr.
#  Files are edited in place with -overwrite_original, so back up first if
#  you want to be safe.
# ============================================================================

## ---- 1. Dependencies -------------------------------------------------------
required <- c("shiny", "leaflet", "exiftoolr", "magick", "DT", "httr", "jsonlite")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}

library(shiny)
library(leaflet)
library(DT)

# ExifTool is a separate command-line program that exiftoolr drives.
# This installs a private copy the first time, if one isn't already on PATH.
if (is.null(tryCatch(exiftoolr::exif_version(), error = function(e) NULL))) {
  message("Installing ExifTool (one-time) ...")
  exiftoolr::install_exiftool()
}

## ---- 2. Constants & helpers ------------------------------------------------
PHOTO_EXT <- c("jpg", "jpeg", "jpe", "tif", "tiff", "png", "webp",
               "heic", "heif", "dng", "cr2", "cr3", "nef", "arw",
               "orf", "raf", "rw2")

THUMB_DIR <- file.path(tempdir(), "photo_gps_thumbs")
dir.create(THUMB_DIR, showWarnings = FALSE, recursive = TRUE)
unlink(list.files(THUMB_DIR, full.names = TRUE))   # clean stale thumbs
addResourcePath("thumbs", THUMB_DIR)

# List candidate photo files in a directory, sorted.
list_photos <- function(dir) {
  if (length(dir) != 1 || is.na(dir) || !nzchar(dir) || !dir.exists(dir))
    return(character(0))
  pat <- paste0("\\.(", paste(PHOTO_EXT, collapse = "|"), ")$")
  sort(list.files(dir, pattern = pat, ignore.case = TRUE, full.names = TRUE))
}

# Read GPS + orientation for a vector of paths in one ExifTool call.
# Returns a data frame: path, name, lat, lng, orient, datetime.
read_meta <- function(paths) {
  out <- data.frame(
    path     = paths,
    name     = basename(paths),
    lat      = NA_real_,
    lng      = NA_real_,
    orient   = NA_integer_,
    datetime = as.POSIXct(NA),
    stringsAsFactors = FALSE
  )
  if (!length(paths)) return(out)

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
  if (is.null(d) || !nrow(d)) return(out)

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

  # Hemisphere: magnitude * sign from N/S/E/W ref. If ref is absent, keep
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

# Write a signed lat/lng into a photo's EXIF GPS tags.
write_gps <- function(path, lat, lng) {
  latref <- if (lat >= 0) "N" else "S"
  lngref <- if (lng >= 0) "E" else "W"
  exiftoolr::exif_call(
    args = c(
      sprintf("-GPSLatitude=%.8f",  abs(lat)),
      sprintf("-GPSLatitudeRef=%s", latref),
      sprintf("-GPSLongitude=%.8f", abs(lng)),
      sprintf("-GPSLongitudeRef=%s", lngref),
      "-GPSMapDatum=WGS-84",
      "-overwrite_original",   # edit in place, no _original backup file
      "-P"                     # preserve filesystem modify time
    ),
    path = path
  )
  invisible(TRUE)
}

# Write a POSIXct datetime into the three main EXIF date/time tags.
# ExifTool expects "YYYY:MM:DD HH:MM:SS" (colon-separated date).
write_datetime <- function(path, dt) {
  stopifnot(inherits(dt, "POSIXct"))
  stamp <- format(dt, "%Y:%m:%d %H:%M:%S", tz = "UTC")
  exiftoolr::exif_call(
    args = c(
      sprintf("-DateTimeOriginal=%s", stamp),
      sprintf("-CreateDate=%s",       stamp),
      sprintf("-ModifyDate=%s",       stamp),
      "-overwrite_original",
      "-P"
    ),
    path = path
  )
  invisible(TRUE)
}

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

# Geocode a place name with OpenStreetMap Nominatim. Exactly ONE request per
# call (fires on the Search button / Enter) -- no type-ahead completion.
# Returns list(lat, lng, name) or NULL.
geocode_osm <- function(q) {
  q <- trimws(q)
  if (!length(q) || !nzchar(q)) return(NULL)
  res <- tryCatch(
    httr::GET(
      "https://nominatim.openstreetmap.org/search",
      query = list(q = q, format = "json", limit = 1),
      # Nominatim requires a descriptive User-Agent; without it requests fail.
      httr::user_agent("photo_gps_editor R/Shiny (single-search)")
    ),
    error = function(e) NULL
  )
  if (is.null(res) || httr::status_code(res) != 200) return(NULL)
  dat <- tryCatch(
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  if (is.data.frame(dat) && nrow(dat) < 1) return(NULL)
  list(
    lat  = as.numeric(dat$lat[1]),
    lng  = as.numeric(dat$lon[1]),
    name = as.character(dat$display_name[1])
  )
}

## ---- 3. UI -----------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    .photo-box   { text-align:center; margin:8px 0; }
    .photo-box img { max-width:100%; max-height:300px;
                     border:1px solid #ccc; border-radius:6px; }
    .loc-info    { font-size:13px; line-height:1.5em; margin:6px 0; }
    .hint        { color:#666; font-size:12px; }
    #search_q .form-group, #search_q.form-group { margin-bottom:0; }
    .date-row    { display:flex; gap:6px; align-items:flex-end; margin-top:6px; }
    .date-row > div { flex:1; }
    ")),
    tags$script(HTML("
      document.addEventListener('keydown', function(e){
        if (e.key === 'Enter' && document.activeElement &&
            document.activeElement.id === 'search_q') {
          e.preventDefault();
          document.getElementById('search_go').click();
        }
      });
    "))
  ),
  titlePanel("Photo GPS Editor"),
  fluidRow(
    column(
      width = 4,
      wellPanel(
        textInput("dir", "Photo directory", value = "",
                  placeholder = "/path/to/photos"),
        actionButton("load", "Load photos", class = "btn-primary", width = "100%")
      ),
      uiOutput("status"),
      div(class = "photo-box", uiOutput("photo")),
      fluidRow(
        column(6, actionButton("prev", "\u25C0 Prev", width = "100%")),
        column(6, actionButton("nxt",  "Next \u25B6", width = "100%"))
      ),
      uiOutput("locinfo"),
      actionButton("save", "Save selected point \u2192 photo",
                   class = "btn-success", width = "100%"),
      br(), br(),
      # ---- creation-date editor -------------------------------------------
      tags$hr(),
      tags$strong("Creation date / time (UTC)"),
      div(class = "date-row",
        div(dateInput("edit_date", label = NULL, value = Sys.Date())),
        div(textInput("edit_time", label = NULL, value = "00:00:00",
                      placeholder = "HH:MM:SS"))
      ),
      actionButton("save_date", "Save date \u2192 photo",
                   class = "btn-warning", width = "100%"),
      tags$hr(),
      # ---------------------------------------------------------------------
      fluidRow(
        column(6, actionButton("copy",  "Copy location",  width = "100%")),
        column(6, actionButton("paste", "Paste & save",   width = "100%"))
      ),
      div(class = "hint", style = "margin-top:8px;",
          "Search or click the map to choose a point, then Save. ",
          "Copy grabs the current photo's location; Paste & save writes it ",
          "to the photo you're now on and moves to the next."),
      br(),
      DTOutput("tbl")
    ),
    column(
      width = 8,
      div(style = "display:flex; gap:6px; align-items:center; margin-bottom:6px;",
          div(style = "flex:1;",
              textInput("search_q", label = NULL,
                        placeholder = "Search a place, then press Enter or click Search")),
          actionButton("search_go", "Search", class = "btn-primary")
      ),
      leafletOutput("map", height = "80vh")
    )
  )
)

## ---- 4. Server -------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    meta    = NULL,   # data frame of all photos
    idx     = 0,      # currently selected row (1-based)
    pending = NULL,   # list(lat,lng) chosen on the map but not yet saved
    clip    = NULL,   # copied list(lat,lng)
    thumb   = NULL    # basename of current thumbnail
  )

  # --- base map (rendered once) --------------------------------------------
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "Street") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addLayersControl(
        baseGroups = c("Street", "Satellite"),
        options = layersControlOptions(collapsed = TRUE)
      ) |>
      setView(lng = 0, lat = 20, zoom = 2) |>
      addControl(
        html = "Click the map to set a location for the current photo.",
        position = "topright"
      )
  })

  tbl_proxy <- dataTableProxy("tbl")

  # --- redraw markers + recenter for the current photo ----------------------
  update_map <- function(recenter = TRUE) {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    row <- rv$meta[rv$idx, ]
    p <- leafletProxy("map") |> clearGroup("current") |> clearGroup("pending")
    if (!is.na(row$lat) && !is.na(row$lng)) {
      p <- p |> addMarkers(row$lng, row$lat, group = "current",
                           label = "Saved location")
      if (recenter) p <- p |> setView(row$lng, row$lat, zoom = 13)
    }
    if (!is.null(rv$pending)) {
      p |> addCircleMarkers(rv$pending$lng, rv$pending$lat, group = "pending",
                            color = "red", fillColor = "red", radius = 8,
                            fillOpacity = 0.9, label = "New location (unsaved)")
    }
    invisible()
  }

  # --- show a given photo (thumbnail, map, table selection) -----------------
  show_current <- function() {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    rv$pending <- NULL
    row <- rv$meta[rv$idx, ]
    rv$thumb <- make_thumb(row$path, row$orient)
    update_map(recenter = TRUE)
    selectRows(tbl_proxy, rv$idx)
    # Populate the date / time editor inputs.
    dt <- row$datetime
    if (!is.na(dt)) {
      updateDateInput(session, "edit_date",
                      value = as.Date(format(dt, "%Y-%m-%d", tz = "UTC")))
      updateTextInput(session, "edit_time",
                      value = format(dt, "%H:%M:%S", tz = "UTC"))
    } else {
      updateDateInput(session, "edit_date", value = Sys.Date())
      updateTextInput(session, "edit_time", value = "00:00:00")
    }
    invisible()
  }

  # central navigation (clamped); everything that moves goes through here
  go_to <- function(i) {
    if (is.null(rv$meta) || !nrow(rv$meta)) return(invisible())
    rv$idx <- max(1, min(nrow(rv$meta), i))
    show_current()
  }

  # --- load a directory -----------------------------------------------------
  observeEvent(input$load, {
    files <- list_photos(input$dir)
    if (!length(files)) {
      showNotification("No photos found in that directory.", type = "error")
      rv$meta <- NULL; rv$idx <- 0; rv$pending <- NULL
      return()
    }
    withProgress(message = "Reading EXIF ...", value = 0.5, {
      rv$meta <- read_meta(files)
    })
    go_to(1)
  })

  # --- navigation buttons ---------------------------------------------------
  observeEvent(input$nxt,  go_to(rv$idx + 1))
  observeEvent(input$prev, go_to(rv$idx - 1))

  # --- table row selection --------------------------------------------------
  observeEvent(input$tbl_rows_selected, {
    s <- input$tbl_rows_selected
    if (length(s) == 1 && !is.na(s) && s != rv$idx) go_to(s)
  })

  # --- click map => set pending point ---------------------------------------
  observeEvent(input$map_click, {
    if (rv$idx < 1) return()
    cl <- input$map_click
    rv$pending <- list(lat = cl$lat, lng = cl$lng)
    leafletProxy("map") |>
      clearGroup("pending") |>
      addCircleMarkers(cl$lng, cl$lat, group = "pending",
                       color = "red", fillColor = "red", radius = 8,
                       fillOpacity = 0.9, label = "New location (unsaved)")
  })

  # --- place search: ONE Nominatim request on Search/Enter (no completion) --
  observeEvent(input$search_go, {
    q <- input$search_q
    if (is.null(q) || !nzchar(trimws(q))) return()
    hit <- tryCatch(geocode_osm(q), error = function(e) NULL)
    if (is.null(hit) || is.na(hit$lat) || is.na(hit$lng)) {
      showNotification(sprintf("No match for \u201C%s\u201D.", trimws(q)),
                       type = "warning")
      return()
    }
    leafletProxy("map") |>
      clearGroup("search") |>
      setView(hit$lng, hit$lat, zoom = 14) |>
      addCircleMarkers(hit$lng, hit$lat, group = "search",
                       radius = 6, color = "#2c7fb8", fillColor = "#2c7fb8",
                       fillOpacity = 0.7, label = hit$name)
    showNotification(paste0("Found: ", hit$name), type = "message", duration = 4)
  })

  # --- save the pending point to the current photo, then advance ------------
  observeEvent(input$save, {
    if (rv$idx < 1) return()
    if (is.null(rv$pending)) {
      showNotification("Click a point on the map first.", type = "warning")
      return()
    }
    row <- rv$meta[rv$idx, ]
    ok <- tryCatch({ write_gps(row$path, rv$pending$lat, rv$pending$lng); TRUE },
                   error = function(e) {
                     showNotification(paste("Write failed:", conditionMessage(e)),
                                      type = "error"); FALSE })
    if (!ok) return()
    meta <- rv$meta
    meta$lat[rv$idx] <- rv$pending$lat
    meta$lng[rv$idx] <- rv$pending$lng
    rv$meta <- meta
    showNotification(sprintf("Saved %.6f, %.6f \u2192 %s",
                             rv$pending$lat, rv$pending$lng, row$name),
                     type = "message")
    if (rv$idx >= nrow(rv$meta)) {
      showNotification("That was the last photo.", type = "message")
      rv$pending <- NULL; update_map()
    } else {
      go_to(rv$idx + 1)
    }
  })

  # --- save creation date to the current photo ------------------------------
  observeEvent(input$save_date, {
    if (rv$idx < 1) return()
    # Validate date input.
    d <- tryCatch(as.Date(input$edit_date), error = function(e) NA)
    if (is.na(d)) {
      showNotification("Invalid date.", type = "error"); return()
    }
    # Validate time input ("HH:MM:SS" or "HH:MM").
    t_str <- trimws(input$edit_time)
    if (!grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", t_str)) {
      showNotification("Time must be HH:MM or HH:MM:SS.", type = "error")
      return()
    }
    if (!grepl(":\\d{2}$", t_str)) t_str <- paste0(t_str, ":00")  # add seconds
    dt <- tryCatch(
      as.POSIXct(paste(format(d, "%Y-%m-%d"), t_str), tz = "UTC"),
      error = function(e) NA
    )
    if (is.na(dt) || !inherits(dt, "POSIXct")) {
      showNotification("Could not parse date/time.", type = "error"); return()
    }
    row <- rv$meta[rv$idx, ]
    ok <- tryCatch({ write_datetime(row$path, dt); TRUE },
                   error = function(e) {
                     showNotification(paste("Write failed:", conditionMessage(e)),
                                      type = "error"); FALSE })
    if (!ok) return()
    meta <- rv$meta
    meta$datetime[rv$idx] <- dt
    rv$meta <- meta
    showNotification(
      sprintf("Date saved: %s \u2192 %s",
              format(dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"), row$name),
      type = "message"
    )
  })

  # --- copy current photo's location to the clipboard buffer ----------------
  observeEvent(input$copy, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]
    if (!is.null(rv$pending)) {
      rv$clip <- rv$pending
    } else if (!is.na(row$lat) && !is.na(row$lng)) {
      rv$clip <- list(lat = row$lat, lng = row$lng)
    } else {
      showNotification("This photo has no location to copy.", type = "warning")
      return()
    }
    showNotification(sprintf("Copied %.6f, %.6f", rv$clip$lat, rv$clip$lng),
                     type = "message")
  })

  # --- paste buffer to current photo, write, advance ------------------------
  observeEvent(input$paste, {
    if (rv$idx < 1) return()
    if (is.null(rv$clip)) {
      showNotification("Nothing copied yet.", type = "warning"); return()
    }
    row <- rv$meta[rv$idx, ]
    ok <- tryCatch({ write_gps(row$path, rv$clip$lat, rv$clip$lng); TRUE },
                   error = function(e) {
                     showNotification(paste("Write failed:", conditionMessage(e)),
                                      type = "error"); FALSE })
    if (!ok) return()
    meta <- rv$meta
    meta$lat[rv$idx] <- rv$clip$lat
    meta$lng[rv$idx] <- rv$clip$lng
    rv$meta <- meta
    showNotification(sprintf("Pasted location \u2192 %s", row$name),
                     type = "message")
    if (rv$idx >= nrow(rv$meta)) {
      showNotification("That was the last photo.", type = "message")
      rv$pending <- NULL; update_map()
    } else {
      go_to(rv$idx + 1)
    }
  })

  # --- left-panel readouts --------------------------------------------------
  output$status <- renderUI({
    if (is.null(rv$meta)) return(helpText("Load a directory to begin."))
    row <- rv$meta[rv$idx, ]
    tagList(
      tags$strong(sprintf("Photo %d of %d", rv$idx, nrow(rv$meta))),
      tags$div(row$name)
    )
  })

  output$photo <- renderUI({
    if (rv$idx < 1) return(NULL)
    if (is.null(rv$thumb))
      return(tags$em("Preview not available for this file type."))
    tags$img(src = file.path("thumbs", rv$thumb))
  })

  output$locinfo <- renderUI({
    if (rv$idx < 1) return(NULL)
    row   <- rv$meta[rv$idx, ]
    saved <- if (is.na(row$lat) || is.na(row$lng)) "no-location"
             else sprintf("%.6f, %.6f", row$lat, row$lng)
    pend  <- if (is.null(rv$pending)) "\u2014"
             else sprintf("%.6f, %.6f", rv$pending$lat, rv$pending$lng)
    dt_str <- if (is.na(row$datetime)) "no date"
              else format(row$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    tags$div(class = "loc-info",
      tags$div(tags$strong("Saved: "), saved),
      tags$div(tags$strong("Selected (unsaved): "), pend),
      tags$div(tags$strong("Creation date (UTC): "), dt_str),
      if (!is.null(rv$clip))
        tags$div(tags$strong("Clipboard: "),
                 sprintf("%.6f, %.6f", rv$clip$lat, rv$clip$lng))
    )
  })

  # --- photo list table -----------------------------------------------------
  table_df <- reactive({
    req(!is.null(rv$meta))
    data.frame(
      File = rv$meta$name,
      Location = ifelse(is.na(rv$meta$lat) | is.na(rv$meta$lng),
                        "no-location",
                        sprintf("%.5f, %.5f", rv$meta$lat, rv$meta$lng)),
      Date = ifelse(is.na(rv$meta$datetime),
                    "no date",
                    format(rv$meta$datetime, "%Y-%m-%d %H:%M", tz = "UTC")),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })

  output$tbl <- renderDT({
    datatable(
      table_df(),
      selection = "single",
      rownames  = TRUE,
      options = list(pageLength = 10, dom = "tip", scrollY = "300px",
                     scrollCollapse = TRUE)
    )
  })
}

## ---- 5. Launch -------------------------------------------------------------
shinyApp(ui = ui, server = server)
