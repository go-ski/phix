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
#    7. Save / copy / paste observers (GPS and date)
#    8. Sidebar output renders (counts_compact, status, locinfo)
#    9. Photo list table (reactive + renderDT)
# ============================================================================

server <- function(input, output, session) {

  rv <- shiny::reactiveValues(
    meta      = NULL,   # data frame of all photos
    idx       = 0,      # currently selected row (1-based)
    pending   = NULL,   # list(lat,lng) chosen on the map but not yet saved
    clip      = NULL,   # copied list(lat,lng)
    date_clip = NULL,   # copied POSIXct datetime
    thumb     = NULL,   # basename of JPEG thumbnail (RAW/TIFF fallback)
    photo_url = NULL    # URL fed to the popup window
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
        html = "Click the map to set a location for the current photo.",
        position = "topright"
      )
  })

  tbl_proxy <- DT::dataTableProxy("tbl")

  # --- redraw markers + recenter for the current photo ----------------------
  update_map <- function(recenter = TRUE) {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    row <- rv$meta[rv$idx, ]
    p <- leaflet::leafletProxy("map") |>
      leaflet::clearGroup("current") |>
      leaflet::clearGroup("pending")
    if (!is.na(row$lat) && !is.na(row$lng)) {
      p <- p |> leaflet::addMarkers(row$lng, row$lat, group = "current",
                                    label = "Saved location")
      if (recenter) p <- p |> leaflet::setView(row$lng, row$lat, zoom = 13)
    }
    if (!is.null(rv$pending)) {
      p |> leaflet::addCircleMarkers(
        rv$pending$lng, rv$pending$lat, group = "pending",
        color = "red", fillColor = "red", radius = 8,
        fillOpacity = 0.9, label = "New location (unsaved)"
      )
    }
    invisible()
  }

  # --- show a given photo (thumbnail, map, table selection) -----------------
  show_current <- function() {
    if (rv$idx < 1 || is.null(rv$meta)) return(invisible())
    rv$pending <- NULL
    row <- rv$meta[rv$idx, ]
    ext <- tolower(tools::file_ext(row$path))
    if (ext %in% BROWSER_PHOTO_EXT) {
      # Browser-displayable: serve the original at full resolution.
      rv$thumb     <- NULL
      rv$photo_url <- paste0("originals/", basename(row$path))
    } else {
      # RAW / TIFF: transcode to a JPEG thumbnail the browser can show.
      rv$thumb     <- make_thumb(row$path, row$orient)
      rv$photo_url <- if (!is.null(rv$thumb)) paste0("thumbs/", rv$thumb) else NULL
    }
    # Auto-update the popup window if already open (force = FALSE).
    if (!is.null(rv$photo_url))
      session$sendCustomMessage("photo_window_update",
                                list(url = rv$photo_url, force = FALSE))
    update_map(recenter = TRUE)
    DT::selectRows(tbl_proxy, rv$idx)
    # Populate the date / time editor inputs.
    dt <- row$datetime
    if (!is.na(dt)) {
      shiny::updateDateInput(session, "edit_date",
                             value = as.Date(format(dt, "%Y-%m-%d", tz = "UTC")))
      shiny::updateTextInput(session, "edit_time",
                             value = format(dt, "%H:%M:%S", tz = "UTC"))
    } else {
      shiny::updateDateInput(session, "edit_date", value = Sys.Date())
      shiny::updateTextInput(session, "edit_time", value = "00:00:00")
    }
    invisible()
  }

  # central navigation (clamped); everything that moves goes through here
  go_to <- function(i) {
    if (is.null(rv$meta) || !nrow(rv$meta)) return(invisible())
    rv$idx <- max(1, min(nrow(rv$meta), i))
    show_current()
  }

  # Parse the date/time editor inputs ("YYYY-MM-DD" + "HH:MM[:SS]") into a
  # single UTC POSIXct.  Returns the POSIXct on success, or NULL after showing
  # a notification.  `notify_type` lets callers downgrade errors to warnings
  # (Copy date treats an unparseable value as a soft warning, not an error).
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
    if (!grepl(":\\d{2}$", t_str)) t_str <- paste0(t_str, ":00")  # add seconds
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

    # Determine the parent to list: if the input ends with "/" or is itself a
    # directory, list its children; otherwise list siblings of the partial name.
    if (dir.exists(raw) && grepl("/$", raw)) {
      parent <- raw
    } else {
      parent <- dirname(raw)
    }

    if (!dir.exists(parent)) {
      session$sendCustomMessage("dir_completions", list())
      return()
    }

    # List immediate subdirectories of `parent`, filter to those matching the
    # typed prefix (case-insensitive on all platforms), cap at 20 entries.
    subdirs <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
    prefix  <- if (dir.exists(raw) && grepl("/$", raw)) "" else basename(raw)
    if (nzchar(prefix)) {
      subdirs <- subdirs[startsWith(tolower(basename(subdirs)), tolower(prefix))]
    }
    subdirs <- sort(subdirs)
    if (length(subdirs) > 20) subdirs <- subdirs[seq_len(20)]

    # Append a trailing slash so selecting a completion extends the path.
    subdirs <- paste0(subdirs, "/")
    session$sendCustomMessage("dir_completions", as.list(subdirs))
  })

  # --- load a directory -----------------------------------------------------
  shiny::observeEvent(input$load, {
    files <- list_photos(input$dir)
    if (!length(files)) {
      shiny::showNotification("No photos found in that directory.", type = "error")
      rv$meta <- NULL; rv$idx <- 0; rv$pending <- NULL
      return()
    }
    # Serve originals from this directory so the popup can display full-res
    # images without re-encoding.  The path is re-registered on every load so
    # switching folders always points to the current directory.
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

  # --- click map => set pending point ---------------------------------------
  shiny::observeEvent(input$map_click, {
    if (rv$idx < 1) return()
    cl <- input$map_click
    rv$pending <- list(lat = cl$lat, lng = cl$lng)
    leaflet::leafletProxy("map") |>
      leaflet::clearGroup("pending") |>
      leaflet::addCircleMarkers(cl$lng, cl$lat, group = "pending",
                                color = "red", fillColor = "red", radius = 8,
                                fillOpacity = 0.9, label = "New location (unsaved)")
  })

  # --- place search: ONE Nominatim request on Search/Enter (no completion) --
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

  # --- save any changed GPS and/or date in one ExifTool call, then advance --
  # The single save action only writes tags that actually differ from what is
  # already in the file: a pending map point that moved the location, and/or a
  # date/time that differs from the saved value.  Unchanged tags are left
  # untouched so the file is only rewritten when there is something to change.
  shiny::observeEvent(input$save_both, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]

    # GPS change: a pending point that differs from the saved location.
    gps <- NULL
    if (!is.null(rv$pending)) {
      moved <- is.na(row$lat) || is.na(row$lng) ||
        abs(row$lat - rv$pending$lat) > 1e-7 ||
        abs(row$lng - rv$pending$lng) > 1e-7
      if (moved) gps <- rv$pending
    }

    # Date change: parse the inputs and compare at second resolution (the saved
    # value may carry sub-seconds the inputs can't show, so compare formatted
    # strings to avoid a spurious rewrite).
    dt_in <- parse_dt_inputs()
    if (is.null(dt_in)) return()        # invalid date/time entry — abort
    dt <- NULL
    fmt <- function(x) format(x, "%Y:%m:%d %H:%M:%S", tz = "UTC")
    if (is.na(row$datetime) || fmt(row$datetime) != fmt(dt_in)) dt <- dt_in

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
      rv$pending <- NULL; update_map()
    } else {
      go_to(rv$idx + 1)
    }
  })

  # --- copy current photo's date to the date clipboard ---------------------
  shiny::observeEvent(input$copy_date, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]
    if (!is.na(row$datetime)) {
      rv$date_clip <- row$datetime
    } else {
      # Fall back to whatever is currently shown in the date/time inputs.
      dt <- parse_dt_inputs("warning")
      if (is.null(dt)) return()
      rv$date_clip <- dt
    }
    shiny::showNotification(
      sprintf("Copied date: %s",
              format(rv$date_clip, "%Y-%m-%d %H:%M:%S", tz = "UTC")),
      type = "message"
    )
  })

  # --- paste date clipboard to current photo, write, advance ----------------
  shiny::observeEvent(input$paste_date, {
    if (rv$idx < 1) return()
    if (is.null(rv$date_clip)) {
      shiny::showNotification("No date copied yet.", type = "warning"); return()
    }
    row <- rv$meta[rv$idx, ]
    ok <- tryCatch({ write_datetime(row$path, rv$date_clip); TRUE },
                   error = function(e) {
                     shiny::showNotification(
                       paste("Write failed:", conditionMessage(e)), type = "error")
                     FALSE })
    if (!ok) return()
    meta <- rv$meta
    meta$datetime[rv$idx] <- rv$date_clip
    rv$meta <- meta
    shiny::showNotification(sprintf("Pasted date \u2192 %s", row$name),
                            type = "message")
    if (rv$idx >= nrow(rv$meta)) {
      shiny::showNotification("That was the last photo.", type = "message")
    } else {
      go_to(rv$idx + 1)
    }
  })

  # --- copy current photo's location to the clipboard buffer ----------------
  shiny::observeEvent(input$copy, {
    if (rv$idx < 1) return()
    row <- rv$meta[rv$idx, ]
    if (!is.null(rv$pending)) {
      rv$clip <- rv$pending
    } else if (!is.na(row$lat) && !is.na(row$lng)) {
      rv$clip <- list(lat = row$lat, lng = row$lng)
    } else {
      shiny::showNotification("This photo has no location to copy.",
                              type = "warning")
      return()
    }
    shiny::showNotification(sprintf("Copied %.6f, %.6f", rv$clip$lat, rv$clip$lng),
                            type = "message")
  })

  # --- paste buffer to current photo, write, advance ------------------------
  shiny::observeEvent(input$paste, {
    if (rv$idx < 1) return()
    if (is.null(rv$clip)) {
      shiny::showNotification("Nothing copied yet.", type = "warning"); return()
    }
    row <- rv$meta[rv$idx, ]
    ok <- tryCatch({ write_gps(row$path, rv$clip$lat, rv$clip$lng); TRUE },
                   error = function(e) {
                     shiny::showNotification(
                       paste("Write failed:", conditionMessage(e)), type = "error")
                     FALSE })
    if (!ok) return()
    meta <- rv$meta
    meta$lat[rv$idx] <- rv$clip$lat
    meta$lng[rv$idx] <- rv$clip$lng
    rv$meta <- meta
    shiny::showNotification(sprintf("Pasted location \u2192 %s", row$name),
                            type = "message")
    if (rv$idx >= nrow(rv$meta)) {
      shiny::showNotification("That was the last photo.", type = "message")
      rv$pending <- NULL; update_map()
    } else {
      go_to(rv$idx + 1)
    }
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

  # --- left-panel readouts --------------------------------------------------
  output$status <- shiny::renderUI({
    if (is.null(rv$meta)) return(shiny::helpText("Load a directory to begin."))
    row <- rv$meta[rv$idx, ]
    shiny::tagList(
      shiny::tags$strong(sprintf("Photo %d of %d", rv$idx, nrow(rv$meta))),
      shiny::tags$div(row$name)
    )
  })

  output$locinfo <- shiny::renderUI({
    if (rv$idx < 1) return(NULL)
    row   <- rv$meta[rv$idx, ]
    saved <- if (is.na(row$lat) || is.na(row$lng)) "no-location"
             else sprintf("%.6f, %.6f", row$lat, row$lng)
    pend  <- if (is.null(rv$pending)) "\u2014"
             else sprintf("%.6f, %.6f", rv$pending$lat, rv$pending$lng)
    dt_str <- if (is.na(row$datetime)) "no date"
              else format(row$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    shiny::tags$div(class = "loc-info",
      shiny::tags$div(shiny::tags$strong("Saved: "), saved),
      shiny::tags$div(shiny::tags$strong("Selected (unsaved): "), pend),
      shiny::tags$div(shiny::tags$strong("Creation date (UTC): "), dt_str),
      if (!is.null(rv$clip))
        shiny::tags$div(shiny::tags$strong("Clipboard GPS: "),
                        sprintf("%.6f, %.6f", rv$clip$lat, rv$clip$lng)),
      if (!is.null(rv$date_clip))
        shiny::tags$div(shiny::tags$strong("Clipboard date: "),
                        format(rv$date_clip, "%Y-%m-%d %H:%M:%S", tz = "UTC"))
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
