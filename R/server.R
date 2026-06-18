# ============================================================================
#  server.R — Shiny server function
#
#  All external-package calls are fully namespace-qualified (pkg::fun()) so
#  this file works regardless of which packages are attached at the time
#  shiny::runApp() auto-sources R/*.R.
#
#  Internal structure:
#    1. Reactive state (rv)
#    2. Outputs rendered once (map, tbl_proxy)
#    3. Internal helpers (update_map, show_current, go_to, parse_dt_inputs)
#    4. Directory auto-completion observer
#    5. Load / navigation observers
#    6. Map interaction observers (click, search)
#    7. Save / copy observers (GPS and date)
#    8. Sidebar output renders (counts_compact, status, current_gps, current_date)
#    9. Photo list table (reactive + renderDT)
# ============================================================================

server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    meta               = NULL,   # data frame of all photos
    idx                = 0,      # currently selected row (1-based)
    thumb              = NULL,   # basename of JPEG thumbnail (RAW/TIFF fallback)
    photo_url          = NULL,   # URL fed to the popup window
    date_clipboard_set = FALSE   # TRUE once the user explicitly sets the date clipboard
    # Clipboard GPS lives in input$clip_lat / input$clip_lng (text inputs).
    # Clipboard date lives in input$edit_date / input$edit_time (date inputs).
    # Both persist across photo navigation; Copy buttons load values into them.
  )

  # --- base map (rendered once) --------------------------------------------
  output$map <- leaflet::renderLeaflet({
    leaflet::leaflet() |>
      leaflet::addTiles(group = "Street") |>
      leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                group = "Satellite") |>
      leaflet::addLayersControl(
        baseGroups = c("Street", "Satellite"),
        options = leaflet::layersControlOptions(collapsed = TRUE)
      ) |>
      leaflet::setView(lng = 0, lat = 20, zoom = 2) |>
      leaflet::addControl(
        html = "Click the map to set a clipboard location for the current photo.",
        position = "topright"
      )
  })

  tbl_proxy <- DT::dataTableProxy("tbl")

  # --- redraw the saved-location marker for the current photo ---------------
  # "pending" group (clipboard GPS marker) is managed separately by the
  # debounced clip_lat/clip_lng observer and is never cleared here so that
  # the clipboard persists across photo navigation.
  update_map <- function(recenter = TRUE) {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    row <- rv$meta[rv$idx, ]
    p <- leaflet::leafletProxy("map") |>
      leaflet::clearGroup("current")
    if (!is.na(row$lat) && !is.na(row$lng)) {
      p <- p |> leaflet::addMarkers(row$lng, row$lat, group = "current",
                                    label = "Saved location")
      if (recenter) p <- p |> leaflet::setView(row$lng, row$lat, zoom = 13)
    }
    invisible()
  }

  # --- show a given photo (thumbnail, map, table selection) -----------------
  # Does NOT touch the clipboard inputs (clip_lat, clip_lng, edit_date,
  # edit_time) — those persist until the user changes them explicitly.
  show_current <- function() {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    row <- rv$meta[rv$idx, ]
    ext <- tolower(tools::file_ext(row$path))
    if (ext %in% BROWSER_PHOTO_EXT) {
      rv$thumb     <- NULL
      rv$photo_url <- paste0("originals/", basename(row$path))
    } else {
      rv$thumb     <- make_thumb(row$path, row$orient)
      rv$photo_url <- if (!is.null(rv$thumb)) paste0("thumbs/", rv$thumb) else NULL
    }
    if (!is.null(rv$photo_url))
      session$sendCustomMessage("photo_window_update",
                                list(url = rv$photo_url, force = FALSE))
    update_map(recenter = TRUE)
    DT::selectRows(tbl_proxy, rv$idx)
    invisible()
  }

  # central navigation (clamped); everything that moves goes through here
  go_to <- function(i) {
    if (is.null(rv$meta) || !nrow(rv$meta)) return(invisible())
    rv$idx <- max(1, min(nrow(rv$meta), i))
    show_current()
  }

  # Parse the date/time clipboard inputs ("YYYY-MM-DD" + "HH:MM[:SS]") into a
  # single UTC POSIXct.  Returns the POSIXct on success, or NULL after showing
  # a notification.
  parse_dt_inputs <- function(notify_type = "error") {
    d <- tryCatch(as.Date(input$edit_date), error = function(e) NA)
    if (is.na(d)) {
      shiny::showNotification("Invalid date.", type = notify_type); return(NULL)
    }
    t_str <- trimws(input$edit_time)
    if (!grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", t_str)) {
      shiny::showNotification("Time must be HH:MM or HH:MM:SS.", type = notify_type)
      return(NULL)
    }
    if (!grepl(":\\d{2}$", t_str)) t_str <- paste0(t_str, ":00")
    dt <- tryCatch(
      as.POSIXct(paste(format(d, "%Y-%m-%d"), t_str), tz = "UTC"),
      error = function(e) NA
    )
    if (is.na(dt) || !inherits(dt, "POSIXct")) {
      shiny::showNotification("Could not parse date/time.", type = notify_type)
      return(NULL)
    }
    dt
  }

  # --- directory autocompletion ---------------------------------------------
  dir_input_d <- shiny::debounce(shiny::reactive(input$dir), 300)

  shiny::observe({
    raw <- dir_input_d()
    if (is.null(raw) || !nzchar(trimws(raw))) {
      session$sendCustomMessage("dir_completions", list())
      return()
    }
    raw <- trimws(raw)

    if (dir.exists(raw) && grepl("/$", raw)) {
      parent <- raw
    } else {
      parent <- dirname(raw)
    }

    if (!dir.exists(parent)) {
      session$sendCustomMessage("dir_completions", list())
      return()
    }

    subdirs <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
    prefix  <- if (dir.exists(raw) && grepl("/$", raw)) "" else basename(raw)
    if (nzchar(prefix)) {
      subdirs <- subdirs[startsWith(tolower(basename(subdirs)), tolower(prefix))]
    }
    subdirs <- sort(subdirs)
    if (length(subdirs) > 20) subdirs <- subdirs[seq_len(20)]
    subdirs <- paste0(subdirs, "/")
    session$sendCustomMessage("dir_completions", as.list(subdirs))
  })

  # --- load a directory -----------------------------------------------------
  shiny::observeEvent(input$load, {
    files <- list_photos(input$dir)
    if (!length(files)) {
      shiny::showNotification("No photos found in that directory.", type = "error")
      rv$meta <- NULL; rv$idx <- 0
      return()
    }
    shiny::addResourcePath("originals",
                           normalizePath(trimws(input$dir), mustWork = FALSE))
    shiny::withProgress(message = "Reading EXIF ...", value = 0.5, {
      rv$meta <- read_meta(files)
    })
    go_to(1)
  })

  # --- navigation buttons ---------------------------------------------------
  shiny::observeEvent(input$nxt,  go_to(rv$idx + 1))
  shiny::observeEvent(input$prev, go_to(rv$idx - 1))

  # --- track whether date clipboard has been explicitly set -----------------
  # ignoreInit = TRUE so the default Sys.Date() / "00:00:00" values don't
  # immediately light up the border before the user has entered anything.
  shiny::observeEvent(input$edit_date, { rv$date_clipboard_set <- TRUE },
                      ignoreInit = TRUE)
  shiny::observeEvent(input$edit_time, { rv$date_clipboard_set <- TRUE },
                      ignoreInit = TRUE)

  # --- open / focus photo popup window --------------------------------------
  shiny::observeEvent(input$view_photo, {
    if (rv$idx < 1 || is.null(rv$photo_url)) {
      shiny::showNotification("Load a photo first.", type = "warning"); return()
    }
    session$sendCustomMessage("photo_window_update",
                              list(url = rv$photo_url, force = TRUE))
  })

  # --- table row selection --------------------------------------------------
  shiny::observeEvent(input$tbl_rows_selected, {
    s <- input$tbl_rows_selected
    if (length(s) == 1 && !is.na(s) && s != rv$idx) go_to(s)
  })

  # --- map click => fill clipboard lat/lng inputs ---------------------------
  shiny::observeEvent(input$map_click, {
    if (rv$idx < 1) return()
    cl <- input$map_click
    shiny::updateTextInput(session, "clip_lat",
                           value = sprintf("%.6f", cl$lat))
    shiny::updateTextInput(session, "clip_lng",
                           value = sprintf("%.6f", cl$lng))
  })

  # --- clipboard GPS inputs => update map marker (debounced) ----------------
  clip_lat_d <- shiny::debounce(shiny::reactive(input$clip_lat), 400)
  clip_lng_d <- shiny::debounce(shiny::reactive(input$clip_lng), 400)

  shiny::observe({
    lat <- suppressWarnings(as.numeric(clip_lat_d()))
    lng <- suppressWarnings(as.numeric(clip_lng_d()))
    p <- leaflet::leafletProxy("map") |> leaflet::clearGroup("pending")
    if (!is.na(lat) && !is.na(lng) &&
        abs(lat) <= 90 && abs(lng) <= 180) {
      p |> leaflet::addCircleMarkers(
        lng, lat, group = "pending",
        color = "red", fillColor = "red", radius = 8,
        fillOpacity = 0.9, label = "Clipboard location (unsaved)"
      )
    }
  })

  # --- place search: ONE Nominatim request on Search/Enter ------------------
  shiny::observeEvent(input$search_go, {
    q <- input$search_q
    if (is.null(q) || !nzchar(trimws(q))) return()
    hit <- tryCatch(geocode_osm(q), error = function(e) NULL)
    if (is.null(hit) || is.na(hit$lat) || is.na(hit$lng)) {
      shiny::showNotification(sprintf("No match for \u201C%s\u201D.", trimws(q)),
                              type = "warning")
      return()
    }
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("search") |>
      leaflet::setView(hit$lng, hit$lat, zoom = 14) |>
      leaflet::addCircleMarkers(hit$lng, hit$lat, group = "search",
                                radius = 6, color = "#2c7fb8",
                                fillColor = "#2c7fb8", fillOpacity = 0.7,
                                label = hit$name)
    shiny::showNotification(paste0("Found: ", hit$name), type = "message",
                            duration = 4)
  })

  # --- copy current photo's GPS into the clipboard inputs -------------------
  shiny::observeEvent(input$copy, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]
    if (is.na(row$lat) || is.na(row$lng)) {
      shiny::showNotification("This photo has no location to copy.",
                              type = "warning")
      return()
    }
    shiny::updateTextInput(session, "clip_lat",
                           value = sprintf("%.6f", row$lat))
    shiny::updateTextInput(session, "clip_lng",
                           value = sprintf("%.6f", row$lng))
    shiny::showNotification(sprintf("Copied %.6f, %.6f", row$lat, row$lng),
                            type = "message")
  })

  # --- copy current photo's date into the clipboard date/time inputs --------
  shiny::observeEvent(input$copy_date, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]
    dt <- row$datetime
    if (is.na(dt)) {
      shiny::showNotification("This photo has no date to copy.", type = "warning")
      return()
    }
    shiny::updateDateInput(session, "edit_date",
                           value = as.Date(format(dt, "%Y-%m-%d", tz = "UTC")))
    shiny::updateTextInput(session, "edit_time",
                           value = format(dt, "%H:%M:%S", tz = "UTC"))
    rv$date_clipboard_set <- TRUE
    shiny::showNotification(
      sprintf("Copied %s", format(dt, "%Y-%m-%d %H:%M:%S", tz = "UTC")),
      type = "message")
  })

  # --- save clipboard GPS and/or date to photo, then advance ----------------
  # Only writes tags that actually differ from the file's saved values.
  shiny::observeEvent(input$save_both, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]

    # GPS: parse clipboard lat/lng inputs.
    gps <- NULL
    lat_in <- suppressWarnings(as.numeric(trimws(input$clip_lat)))
    lng_in <- suppressWarnings(as.numeric(trimws(input$clip_lng)))
    if (!is.na(lat_in) && !is.na(lng_in) &&
        abs(lat_in) <= 90 && abs(lng_in) <= 180) {
      moved <- is.na(row$lat) || is.na(row$lng) ||
        abs(row$lat - lat_in) > 1e-7 ||
        abs(row$lng - lng_in) > 1e-7
      if (moved) gps <- list(lat = lat_in, lng = lng_in)
    }

    # Date: only attempt if the date clipboard was explicitly set AND time is
    # non-empty.  This keeps GPS saves independent of date-input validity, and
    # prevents the default Sys.Date()/"00:00:00" values from silently
    # overwriting a photo's date when the user only intended to save GPS.
    dt <- NULL
    if (isTRUE(rv$date_clipboard_set) && nzchar(trimws(input$edit_time))) {
      dt_in <- parse_dt_inputs()
      if (is.null(dt_in)) return()   # abort on parse error (non-empty but invalid time)
      fmt <- function(x) format(x, "%Y:%m:%d %H:%M:%S", tz = "UTC")
      if (is.na(row$datetime) || fmt(row$datetime) != fmt(dt_in)) dt <- dt_in
    }

    if (is.null(gps) && is.null(dt)) {
      shiny::showNotification("Nothing changed \u2014 nothing to save.",
                              type = "warning")
      return()
    }
    ok <- tryCatch({ write_metadata(row$path, gps = gps, dt = dt); TRUE },
                   error = function(e) {
                     shiny::showNotification(
                       paste("Write failed:", conditionMessage(e)), type = "error")
                     FALSE })
    if (!ok) return()
    meta <- rv$meta
    if (!is.null(gps)) { meta$lat[rv$idx] <- gps$lat; meta$lng[rv$idx] <- gps$lng }
    if (!is.null(dt))  meta$datetime[rv$idx] <- dt
    rv$meta <- meta
    saved <- paste(c(if (!is.null(gps)) "GPS", if (!is.null(dt)) "date"),
                   collapse = " + ")
    shiny::showNotification(sprintf("Saved %s \u2192 %s", saved, row$name),
                            type = "message")
    if (rv$idx >= nrow(rv$meta)) {
      shiny::showNotification("That was the last photo.", type = "message")
      update_map()
    } else {
      go_to(rv$idx + 1)
    }
  })

  # --- drive clipboard border colors ----------------------------------------
  # Sends {gps, date} booleans to the JS handler whenever the relevant inputs
  # change; the handler toggles .clipboard-active on each input element.
  shiny::observe({
    lat <- trimws(input$clip_lat)
    lng <- trimws(input$clip_lng)
    session$sendCustomMessage("set_clipboard_border", list(
      gps  = nzchar(lat) && nzchar(lng),
      date = isTRUE(rv$date_clipboard_set) && nzchar(trimws(input$edit_time))
    ))
  })

  # --- compact counts row in sidebar ----------------------------------------
  output$counts_compact <- shiny::renderUI({
    if (is.null(rv$meta)) return(NULL)
    n     <- nrow(rv$meta)
    n_gps <- sum(is.na(rv$meta$lat) | is.na(rv$meta$lng))
    n_dt  <- sum(is.na(rv$meta$datetime))
    shiny::div(class = "d-flex gap-1 flex-wrap mb-1",
      shiny::span(class = "badge text-bg-primary",  paste0(n,     " photos")),
      shiny::span(class = "badge text-bg-warning",  paste0(n_gps, " no GPS")),
      shiny::span(class = "badge text-bg-danger",   paste0(n_dt,  " no date"))
    )
  })

  # --- photo name / index readout -------------------------------------------
  output$status <- shiny::renderUI({
    if (is.null(rv$meta)) return(shiny::helpText("Load a directory to begin."))
    row <- rv$meta[rv$idx, ]
    shiny::tagList(
      shiny::div(class = "sidebar-label",
                 sprintf("Photo %d of %d", rv$idx, nrow(rv$meta))),
      shiny::div(style = "font-size:14px; font-weight:500; color:#212529;",
                 row$name)
    )
  })

  # --- current GPS readout (from photo) with inline Copy button -------------
  output$current_gps <- shiny::renderUI({
    if (rv$idx < 1) return(shiny::p(class = "current-val mt-1", "\u2014"))
    row <- rv$meta[rv$idx, ]
    txt <- if (is.na(row$lat) || is.na(row$lng)) "no location"
           else sprintf("%.6f,\u2002%.6f", row$lat, row$lng)
    shiny::div(class = "d-flex justify-content-between align-items-center mt-1 mb-1",
      shiny::span(class = "current-val", txt),
      shiny::actionButton("copy", "Copy", class = "btn-sm btn-outline-secondary py-0")
    )
  })

  # --- current date readout (from photo) with inline Copy button ------------
  output$current_date <- shiny::renderUI({
    if (rv$idx < 1) return(shiny::p(class = "current-val mt-1", "\u2014"))
    row <- rv$meta[rv$idx, ]
    txt <- if (is.na(row$datetime)) "no date"
           else format(row$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    shiny::div(class = "d-flex justify-content-between align-items-center mt-1 mb-1",
      shiny::span(class = "current-val", txt),
      shiny::actionButton("copy_date", "Copy", class = "btn-sm btn-outline-secondary py-0")
    )
  })

  # --- photo list table -----------------------------------------------------
  table_df <- shiny::reactive({
    shiny::req(!is.null(rv$meta))
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

  output$tbl <- DT::renderDT({
    DT::datatable(
      table_df(),
      selection = "single",
      rownames  = TRUE,
      options = list(pageLength = 10, dom = "tip", scrollY = "300px",
                     scrollCollapse = TRUE)
    )
  })
}
