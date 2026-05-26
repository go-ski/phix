# geotag_app.R
# ---------------------------------------------------------------------------
# Add / view GPS locations for photos (e.g. a Synology Photos library),
# using a free OpenStreetMap map with a place-name search box. No API key.
#
# Given a directory of photos, this app:
#   * reads each photo's GPS EXIF and lists which ones are "no-location"
#   * shows the current photo + its location on a map (or a world map if none)
#   * lets you SEARCH for a place, then click the map to pick coordinates
#   * lets you COPY a location from one photo and paste/apply it to others
#     (incl. "apply to all shown" for a whole batch shot at one spot)
#
# The current photo is tracked by file path, so saving never loses your place:
# selection, table page, and the displayed photo stay put. A tagged photo stays
# in the list (now showing its coordinates) until you click "Refresh list" or
# re-scan, which re-applies the "no location" filter.
#
# Synology Photos reads location from each file's EXIF, so once you write GPS
# here and Synology re-indexes (it detects changed files; you can also force a
# re-index in Synology Photos > Settings), the photos appear on its map.
#
# ---- one-time setup -------------------------------------------------------
#   install.packages(c("shiny", "leaflet", "leaflet.extras", "watcher",
#                       "exiftoolr", "magick", "DT"))
#   exiftoolr::install_exiftool()   # downloads ExifTool (only needed once)
#
# ---- run in Positron (with live autoreload) -------------------------------
#   The {watcher} package gives Shiny event-based (non-polling) autoreload.
#   Autoreload only attaches for file-path apps, and the option is best set
#   BEFORE launching, so run it like this from the Console:
#
#       options(shiny.autoreload = TRUE)        # uses {watcher} automatically
#       shiny::runApp("geotag_app.R")
#
#   Tip: put options(shiny.autoreload = TRUE) in your .Rprofile to keep it on.
#
# ---- safety ---------------------------------------------------------------
#   Files are edited IN PLACE. Test on a COPY first.
#   "-overwrite_original" (below) means no backup is kept. Remove that flag to
#   keep a "<name>_original" backup of every file you change.
# ---------------------------------------------------------------------------

library(shiny)
library(leaflet)
library(leaflet.extras)
library(exiftoolr)
library(magick)
library(DT)

# Live autoreload backed by {watcher} (event-based, not polling). Setting it
# here helps on reloads; for first launch, also set it in the Console before
# runApp() as shown in the header (autoreload only spawns for file-path apps).
options(shiny.autoreload = TRUE)
# options(shiny.autoreload.interval = 500)  # watcher batch latency in ms (default 250)

img_exts <- c("jpg", "jpeg", "png", "tif", "tiff", "heic", "heif", "webp", "gif", "bmp")
PAGE_LEN <- 10L

# --- helpers ---------------------------------------------------------------

list_images <- function(dir) {
  pat <- paste0("\\.(", paste(img_exts, collapse = "|"), ")$")
  list.files(dir, pattern = pat, ignore.case = TRUE,
             full.names = TRUE, recursive = TRUE)
}

# Read GPS as signed decimal degrees (-n). Returns one row per file.
read_gps_table <- function(paths) {
  empty <- data.frame(path = character(), file = character(),
                      lat = numeric(), lon = numeric(),
                      stringsAsFactors = FALSE)
  if (length(paths) == 0) return(empty)
  ex <- exif_read(paths, tags = c("GPSLatitude", "GPSLongitude"), args = "-n")
  lat <- if (!is.null(ex$GPSLatitude))  suppressWarnings(as.numeric(ex$GPSLatitude))  else rep(NA_real_, nrow(ex))
  lon <- if (!is.null(ex$GPSLongitude)) suppressWarnings(as.numeric(ex$GPSLongitude)) else rep(NA_real_, nrow(ex))
  data.frame(path = ex$SourceFile, file = basename(ex$SourceFile),
             lat = lat, lon = lon, stringsAsFactors = FALSE)
}

# Write GPS into a single file. ExifTool handles JPEG, HEIC, TIFF, etc.
write_gps <- function(path, lat, lon) {
  exif_call(
    args = c(
      sprintf("-GPSLatitude=%.8f",  abs(lat)),
      sprintf("-GPSLatitudeRef=%s",  if (lat >= 0) "N" else "S"),
      sprintf("-GPSLongitude=%.8f", abs(lon)),
      sprintf("-GPSLongitudeRef=%s", if (lon >= 0) "E" else "W"),
      "-P",                  # preserve the file's modification timestamp
      "-overwrite_original"  # remove this line to keep a *_original backup
    ),
    path = path
  )
}

# Build the two-column display data frame the table shows.
tbl_display <- function(df) {
  data.frame(
    File = df$file,
    Location = ifelse(is.na(df$lat) | is.na(df$lon),
                      "no-location",
                      sprintf("%.5f, %.5f", df$lat, df$lon)),
    stringsAsFactors = FALSE
  )
}

# Browser-renderable preview cache (HEIC may fail unless ImageMagick has libheic;
# GPS writing still works regardless).
preview_dir <- file.path(tempdir(), "geotag_previews")
dir.create(preview_dir, showWarnings = FALSE)
addResourcePath("previews", preview_dir)

make_preview <- function(path) {
  key <- gsub("[^A-Za-z0-9]", "_", path)
  out <- file.path(preview_dir, paste0(key, ".jpg"))
  if (!file.exists(out)) {
    img <- tryCatch(image_read(path), error = function(e) NULL)
    if (is.null(img)) return(NULL)
    ok <- tryCatch({
      image_write(image_scale(img, "900x900"), out, format = "jpeg"); TRUE
    }, error = function(e) FALSE)
    if (!ok) return(NULL)
  }
  if (file.exists(out)) paste0("previews/", basename(out)) else NULL
}

# --- UI --------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Photo geotagging \u2014 add missing GPS locations"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      textInput("dir", "Photo directory", value = "",
                placeholder = "/Volumes/photo  or  C:/Users/me/Pictures"),
      checkboxInput("only_missing", "Show only photos with no location", TRUE),
      fluidRow(
        column(6, actionButton("scan", "Scan directory", class = "btn-primary", width = "100%")),
        column(6, actionButton("refresh", "Refresh list", width = "100%"))
      ),
      tags$hr(),
      fluidRow(
        column(6, actionButton("prev", "\u2190 Prev", width = "100%")),
        column(6, actionButton("nxt",  "Next \u2192", width = "100%"))
      ),
      tags$br(),
      DTOutput("tbl")
    ),
    mainPanel(
      width = 8,
      uiOutput("status"),
      fluidRow(
        column(5, uiOutput("preview")),
        column(7, leafletOutput("map", height = 380))
      ),
      tags$br(),
      verbatimTextOutput("coords"),
      actionButton("save", "Save location to photo", class = "btn-success"),
      uiOutput("save_msg"),
      tags$hr(),
      wellPanel(
        tags$b("Copy / paste location"),
        tags$div(style = "font-size:90%;color:#555;margin:4px 0 8px;",
                 "Copy grabs this photo's location (existing GPS, or a pin you ",
                 "just placed). Then navigate elsewhere and paste it."),
        fluidRow(
          column(6, actionButton("copyloc", "\u2398 Copy from this photo", width = "100%")),
          column(6, uiOutput("clip_msg"))
        ),
        tags$br(),
        fluidRow(
          column(4, actionButton("pasteloc", "Paste here", width = "100%")),
          column(4, actionButton("pastesave", "Paste + Save", class = "btn-success", width = "100%")),
          column(4, actionButton("applyall", "Apply to all shown", class = "btn-warning", width = "100%"))
        )
      )
    )
  )
)

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  # rv$photos : full source of truth (path, file, lat, lon)
  # rv$tbl    : the subset/order currently shown in the table
  # rv$cur    : path of the current photo (stable across data changes)
  rv <- reactiveValues(photos = NULL, tbl = NULL, cur = NULL,
                       sel = NULL, save_msg = NULL, clip = NULL)

  proxy <- function() dataTableProxy("tbl")

  # (Re)build the table subset from the filter. Keeps the current photo if it
  # is still present; otherwise selects the first row.
  rebuild_tbl <- function() {
    df <- rv$photos
    if (is.null(df)) { rv$tbl <- NULL; rv$cur <- NULL; return() }
    if (isTRUE(input$only_missing))
      df <- df[is.na(df$lat) | is.na(df$lon), , drop = FALSE]
    rv$tbl <- df
    if (nrow(df) > 0) {
      if (is.null(rv$cur) || !(rv$cur %in% df$path)) rv$cur <- df$path[1]
    } else {
      rv$cur <- NULL
    }
  }

  # Push updated cell values to the table WITHOUT resetting page or selection.
  refresh_tbl_data <- function() {
    df <- rv$tbl
    if (is.null(df) || nrow(df) == 0) return()
    replaceData(proxy(), tbl_display(df),
                resetPaging = FALSE, clearSelection = "none", rownames = FALSE)
  }

  # Move table page + selection to the current photo (no re-render).
  sync_table <- function() {
    df <- rv$tbl
    if (is.null(df) || nrow(df) == 0 || is.null(rv$cur)) return()
    pos <- match(rv$cur, df$path)
    if (is.na(pos)) return()
    selectRows(proxy(), pos)
    selectPage(proxy(), ceiling(pos / PAGE_LEN))
  }

  # write GPS to one file, update memory + table cell; returns TRUE on success
  save_one <- function(path, lat, lon) {
    ok <- tryCatch({ write_gps(path, lat, lon); TRUE },
                   error = function(e) {
                     showNotification(paste("Error:", conditionMessage(e)), type = "error")
                     FALSE
                   })
    if (ok) {
      i <- which(rv$photos$path == path)
      rv$photos$lat[i] <- lat; rv$photos$lon[i] <- lon
      j <- which(rv$tbl$path == path)
      if (length(j)) { rv$tbl$lat[j] <- lat; rv$tbl$lon[j] <- lon }
    }
    ok
  }

  observeEvent(input$scan, {
    req(nzchar(input$dir))
    if (!dir.exists(input$dir)) {
      showNotification("Directory not found.", type = "error"); return()
    }
    paths <- list_images(input$dir)
    if (length(paths) == 0)
      showNotification("No image files found in that directory.", type = "warning")
    withProgress(message = "Reading EXIF\u2026", {
      rv$photos <- read_gps_table(paths)
    })
    rv$cur <- NULL          # force selection of first row in rebuild
    rebuild_tbl()
    rv$sel <- NULL; rv$save_msg <- NULL
  })

  observeEvent(input$refresh,      { rebuild_tbl(); rv$sel <- NULL; rv$save_msg <- NULL })
  observeEvent(input$only_missing, { rebuild_tbl(); rv$sel <- NULL; rv$save_msg <- NULL })

  current <- reactive({
    req(rv$cur, rv$photos)
    row <- rv$photos[rv$photos$path == rv$cur, , drop = FALSE]
    req(nrow(row) == 1)
    row
  })

  # --- table (re-renders only when rv$tbl is rebuilt) ---
  output$tbl <- renderDT({
    df <- rv$tbl
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Message = "No photos loaded."),
                       rownames = FALSE, options = list(dom = "t")))
    sel   <- match(isolate(rv$cur), df$path); if (is.na(sel)) sel <- 1L
    start <- (ceiling(sel / PAGE_LEN) - 1L) * PAGE_LEN
    datatable(tbl_display(df), rownames = FALSE,
              selection = list(mode = "single", selected = sel),
              options = list(pageLength = PAGE_LEN, displayStart = start,
                             dom = "tp", scrollX = TRUE))
  })

  # user clicks a row -> set current photo (guarded to avoid feedback loops)
  observeEvent(input$tbl_rows_selected, {
    df <- rv$tbl; req(df)
    p <- df$path[input$tbl_rows_selected]
    if (!identical(p, rv$cur)) { rv$cur <- p; rv$sel <- NULL; rv$save_msg <- NULL }
  })

  observeEvent(input$prev, {
    df <- rv$tbl; req(df, nrow(df) > 0, rv$cur)
    pos <- match(rv$cur, df$path); if (is.na(pos)) pos <- 1L
    rv$cur <- df$path[max(1L, pos - 1L)]; rv$sel <- NULL; rv$save_msg <- NULL
    sync_table()
  })
  observeEvent(input$nxt, {
    df <- rv$tbl; req(df, nrow(df) > 0, rv$cur)
    pos <- match(rv$cur, df$path); if (is.na(pos)) pos <- 0L
    rv$cur <- df$path[min(nrow(df), pos + 1L)]; rv$sel <- NULL; rv$save_msg <- NULL
    sync_table()
  })

  # --- preview + status ---
  output$preview <- renderUI({
    cur <- current()
    src <- make_preview(cur$path)
    if (is.null(src))
      div(style = "padding:1em;border:1px solid #ccc;border-radius:4px;",
          "Preview not available for this file type ",
          "(GPS can still be written).", tags$br(), tags$small(cur$file))
    else
      tags$img(src = src,
               style = "max-width:100%;max-height:380px;border:1px solid #ccc;border-radius:4px;")
  })

  output$status <- renderUI({
    cur <- current()
    has <- !is.na(cur$lat) && !is.na(cur$lon)
    if (has)
      div(strong(cur$file), " \u2014 existing location: ",
          sprintf("%.6f, %.6f", cur$lat, cur$lon))
    else
      div(strong(cur$file), " \u2014 ",
          span("no-location", style = "color:#b00;font-weight:bold;"))
  })

  # --- map (OpenStreetMap + place-name search box) ---
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 0, lat = 20, zoom = 2) |>
      addSearchOSM(options = searchOptions(
        position             = "topright",
        collapsed            = FALSE,
        autoCollapse         = FALSE,
        hideMarkerOnCollapse = TRUE,
        zoom                 = 13,
        textPlaceholder      = "Search a place\u2026"
      ))
  })

  # recenter / mark when the current photo changes
  observe({
    cur <- current()
    has <- !is.na(cur$lat) && !is.na(cur$lon)
    p <- leafletProxy("map") |> clearGroup("existing") |> clearGroup("picked")
    if (has)
      p |> setView(cur$lon, cur$lat, zoom = 12) |>
        addMarkers(cur$lon, cur$lat, group = "existing", popup = "Existing location")
    else
      p |> setView(0, 20, zoom = 2)
  })

  # click the map to pick a new location (search just pans you to the area)
  observeEvent(input$map_click, {
    cl <- input$map_click
    rv$sel <- list(lat = cl$lat, lon = cl$lng)
    leafletProxy("map") |> clearGroup("picked") |>
      addMarkers(cl$lng, cl$lat, group = "picked", popup = "Selected location")
  })

  output$coords <- renderText({
    if (is.null(rv$sel)) {
      cur <- current()
      if (!is.na(cur$lat) && !is.na(cur$lon))
        sprintf("Search/click the map to choose a new location.\nCurrent: %.6f, %.6f",
                cur$lat, cur$lon)
      else
        "Search for a place, then click the map to choose a location for this photo."
    } else {
      sprintf("Selected: %.6f, %.6f  \u2014  click 'Save location to photo' to write it.",
              rv$sel$lat, rv$sel$lon)
    }
  })

  # --- save ---
  output$save_msg <- renderUI(rv$save_msg)

  observeEvent(input$save, {
    req(rv$sel)
    cur <- current()
    if (save_one(cur$path, rv$sel$lat, rv$sel$lon)) {
      rv$sel <- NULL
      rv$save_msg <- div(style = "color:green;margin-top:6px;", "Saved \u2713")
      refresh_tbl_data()       # update the row in place; keep page + selection
      showNotification("GPS written to photo.", type = "message")
    }
  })

  # --- copy / paste location ---
  output$clip_msg <- renderUI({
    if (is.null(rv$clip))
      span("Clipboard: empty", style = "color:#888;")
    else
      span(sprintf("Clipboard: %.6f, %.6f", rv$clip$lat, rv$clip$lon),
           style = "font-weight:bold;")
  })

  # copy the current photo's location: a just-placed pin wins, else existing GPS
  observeEvent(input$copyloc, {
    cur <- current()
    src <- if (!is.null(rv$sel)) c(rv$sel$lat, rv$sel$lon)
           else if (!is.na(cur$lat) && !is.na(cur$lon)) c(cur$lat, cur$lon)
           else NULL
    if (is.null(src)) {
      showNotification("This photo has no location to copy. Place a pin first.",
                       type = "warning")
      return()
    }
    rv$clip <- list(lat = src[1], lon = src[2])
    showNotification(sprintf("Copied %.6f, %.6f to clipboard.", src[1], src[2]),
                     type = "message")
  })

  # stage the clipboard location on the current photo (does not write yet)
  observeEvent(input$pasteloc, {
    if (is.null(rv$clip)) { showNotification("Clipboard is empty.", type = "warning"); return() }
    rv$sel <- list(lat = rv$clip$lat, lon = rv$clip$lon)
    leafletProxy("map") |> clearGroup("picked") |>
      addMarkers(rv$clip$lon, rv$clip$lat, group = "picked", popup = "Pasted location") |>
      setView(rv$clip$lon, rv$clip$lat, zoom = 12)
  })

  # paste + write the clipboard location to the current photo in one step
  observeEvent(input$pastesave, {
    if (is.null(rv$clip)) { showNotification("Clipboard is empty.", type = "warning"); return() }
    cur <- current()
    if (save_one(cur$path, rv$clip$lat, rv$clip$lon)) {
      rv$sel <- NULL
      rv$save_msg <- div(style = "color:green;margin-top:6px;", "Pasted + saved \u2713")
      refresh_tbl_data()
      showNotification("Clipboard location written to photo.", type = "message")
    }
  })

  # apply the clipboard location to every photo currently listed (with confirm)
  observeEvent(input$applyall, {
    if (is.null(rv$clip)) { showNotification("Clipboard is empty.", type = "warning"); return() }
    df <- rv$tbl; req(df, nrow(df) > 0)
    showModal(modalDialog(
      title = "Apply clipboard location to all shown photos",
      sprintf("Write %.6f, %.6f to all %d photo(s) currently listed? This edits the files.",
              rv$clip$lat, rv$clip$lon, nrow(df)),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"),
                       actionButton("applyall_ok", "Apply", class = "btn-danger"))
    ))
  })

  observeEvent(input$applyall_ok, {
    removeModal()
    df <- isolate(rv$tbl); req(rv$clip, df, nrow(df) > 0)
    paths <- df$path; n <- length(paths); okn <- 0L
    withProgress(message = "Writing GPS to photos\u2026", {
      for (k in seq_along(paths)) {
        if (save_one(paths[k], rv$clip$lat, rv$clip$lon)) okn <- okn + 1L
        incProgress(1 / n)
      }
    })
    rv$sel <- NULL
    refresh_tbl_data()
    showNotification(sprintf("Applied location to %d of %d photo(s).", okn, n),
                     type = "message")
  })
}

shinyApp(ui, server)
