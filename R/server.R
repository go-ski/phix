# ============================================================================
#  server.R — Shiny server function
#
#  app_server is called by run_phix() via shiny::shinyApp().
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

app_server <- function(input, output, session) {

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
        html = "Click the map to set a clipboard location",
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
  # edit_time) -- those persist until the user changes them explicitly.
  show_current <- function() {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    row <- rv$meta[rv$idx, ]
    ext <- tolower(tools::file_ext(row$path))
    if (ext %in% .phix_env$BROWSER_PHOTO_EXT) {
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

  # --- timezone helpers -----------------------------------------------------
  # Format signed seconds as an EXIF-style "+HH:MM" / "-HH:MM" string; "" on NA.
  fmt_offset <- function(secs) {
    if (is.null(secs) || is.na(secs)) return("")
    sg   <- if (secs < 0) "-" else "+"
    secs <- abs(as.integer(round(secs)))
    sprintf("%s%02d:%02d", sg, secs %/% 3600L, (secs %% 3600L) %/% 60L)
  }

  # Capture-local wall-clock string for a true-UTC instant + offset (seconds).
  # The offset (or an "unknown" note) is appended when show_offset = TRUE.
  fmt_local <- function(dt, off, fmt = "%Y-%m-%d %H:%M:%S", show_offset = TRUE) {
    if (is.na(dt)) return("no date")
    shifted <- if (!is.na(off)) dt + as.numeric(off) else dt
    s <- format(shifted, fmt, tz = "UTC")
    if (!show_offset) return(s)
    if (!is.na(off)) paste0(s, " ", fmt_offset(off)) else paste0(s, " (offset unknown)")
  }

  # Resolve the capture-location UTC offset (signed seconds) from coordinates
  # and a Date, via lutz.  Noon avoids DST midnight edge cases.  NA when the
  # lookup fails or inputs are missing.
  resolve_offset <- function(lat, lng, date) {
    if (is.na(lat) || is.na(lng) || is.na(date)) return(NA_integer_)
    tz_i <- tryCatch(
      lutz::tz_lookup_coords(lat, lng, method = "fast", warn = FALSE),
      error = function(e) NA_character_
    )
    if (is.na(tz_i) || !nzchar(tz_i) || !(tz_i %in% OlsonNames())) return(NA_integer_)
    day      <- format(date, "%Y-%m-%d")
    local_dt <- as.POSIXct(paste(day, "12:00:00"), tz = tz_i)
    utc_dt   <- as.POSIXct(paste(day, "12:00:00"), tz = "UTC")
    as.integer(round(as.numeric(utc_dt) - as.numeric(local_dt)))
  }

  # Parse the date/time clipboard inputs ("YYYY-MM-DD" + "HH:MM[:SS]"), which
  # are entered in the photo's *local* timezone, into a true-UTC POSIXct plus
  # the offset applied.  The offset is resolved from the clipboard/saved GPS
  # (preferred) or the photo's existing tz_offset; when unknown the entered
  # time is treated as the UTC instant directly.  Returns a list(dt, tz_offset)
  # on success, or NULL after showing a notification.
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
    local <- tryCatch(
      as.POSIXct(paste(format(d, "%Y-%m-%d"), t_str), tz = "UTC"),
      error = function(e) NA
    )
    if (is.na(local) || !inherits(local, "POSIXct")) {
      shiny::showNotification("Could not parse date/time.", type = notify_type)
      return(NULL)
    }
    off <- clipboard_offset(d)
    dt  <- if (!is.na(off)) local - as.numeric(off) else local
    list(dt = dt, tz_offset = off)
  }

  # Offset (signed seconds) for the current edit, resolved from the clipboard
  # GPS first, then the saved photo's resolved tz_offset.  NA when unknown.
  clipboard_offset <- function(date) {
    lat <- suppressWarnings(as.numeric(trimws(input$clip_lat)))
    lng <- suppressWarnings(as.numeric(trimws(input$clip_lng)))
    off <- resolve_offset(lat, lng, date)
    if (!is.na(off)) return(off)
    if (rv$idx >= 1 && !is.null(rv$meta)) return(rv$meta$tz_offset[rv$idx])
    NA_integer_
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
  shiny::observeEvent(input$edit_time,
                      { rv$date_clipboard_set <- nzchar(trimws(input$edit_time)) },
                      ignoreInit = TRUE)

  # --- clear date clipboard (triggered by the x button on edit_date) --------
  # Only clears edit_time (which cascades date_clipboard_set to FALSE via its
  # observer) and resets the flag directly.  Deliberately does NOT call
  # updateDateInput: that would trigger the edit_date observer and cause a
  # brief reactive flicker where the flag toggles TRUE then FALSE.
  shiny::observeEvent(input$clear_date_clipboard, {
    shiny::updateTextInput(session, "edit_time", value = "")
    rv$date_clipboard_set <- FALSE
  })

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
    off   <- row$tz_offset
    local <- if (!is.na(off)) dt + as.numeric(off) else dt
    shiny::updateDateInput(session, "edit_date",
                           value = as.Date(format(local, "%Y-%m-%d", tz = "UTC")))
    shiny::updateTextInput(session, "edit_time",
                           value = format(local, "%H:%M:%S", tz = "UTC"))
    rv$date_clipboard_set <- TRUE
    shiny::showNotification(
      sprintf("Copied %s", fmt_local(dt, off)),
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
    # parse_dt_inputs() returns the true-UTC instant plus the offset applied.
    dt  <- NULL
    off <- NULL
    if (isTRUE(rv$date_clipboard_set) && nzchar(trimws(input$edit_time))) {
      parsed <- parse_dt_inputs()
      if (is.null(parsed)) return()  # abort on parse error (non-empty but invalid time)
      fmt <- function(x) format(x, "%Y:%m:%d %H:%M:%S", tz = "UTC")
      if (is.na(row$datetime) || fmt(row$datetime) != fmt(parsed$dt)) {
        dt  <- parsed$dt
        off <- parsed$tz_offset
      }
    }

    # Backfill offset onto an existing date when new GPS resolves a timezone
    # but the date field itself was not changed.  The DateTimeOriginal wall
    # clock stays fixed; only OffsetTimeOriginal/Digitized and the internal
    # UTC instant are added.
    if (is.null(dt) && !is.null(gps) && !is.na(row$datetime) &&
        is.na(row$tz_offset)) {
      new_off <- resolve_offset(gps$lat, gps$lng,
                                as.Date(format(row$datetime, "%Y-%m-%d", tz = "UTC")))
      if (!is.na(new_off)) {
        # row$datetime here is the raw camera wall clock labelled UTC; the true
        # UTC instant is that wall clock minus the offset.
        dt  <- row$datetime - as.numeric(new_off)
        off <- new_off
      }
    }

    if (is.null(gps) && is.null(dt)) {
      shiny::showNotification("Nothing changed \u2014 nothing to save.",
                              type = "warning")
      return()
    }
    ok <- tryCatch({
      write_metadata(row$path, gps = gps, dt = dt, tz_offset = off); TRUE
    }, error = function(e) {
      shiny::showNotification(
        paste("Write failed:", conditionMessage(e)), type = "error")
      FALSE })
    if (!ok) return()
    meta <- rv$meta
    if (!is.null(gps)) { meta$lat[rv$idx] <- gps$lat; meta$lng[rv$idx] <- gps$lng }
    if (!is.null(dt)) {
      meta$datetime[rv$idx]  <- dt
      meta$tz_offset[rv$idx] <- if (is.null(off)) NA_integer_ else as.integer(off)
    }
    rv$meta <- meta
    saved <- paste(c(if (!is.null(gps)) "GPS", if (!is.null(dt)) "date"),
                   collapse = " + ")
    shiny::showNotification(sprintf("Saved %s \u2192 %s", saved, row$name),
                            type = "message")
    update_map()
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
    if (is.null(rv$meta)) return(NULL)
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
    txt <- fmt_local(row$datetime, row$tz_offset)
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
      Date = vapply(
        seq_len(nrow(rv$meta)),
        function(i) fmt_local(rv$meta$datetime[i], rv$meta$tz_offset[i],
                              fmt = "%Y-%m-%d %H:%M", show_offset = FALSE),
        character(1)
      ),
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
