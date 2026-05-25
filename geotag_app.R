# geotag_app.R
# ---------------------------------------------------------------------------
# Add / view GPS locations for photos (e.g. a Synology Photos library),
# using a free OpenStreetMap map with a place-name search box. No API key.
#
# Given a directory of photos, this app:
#   * reads each photo's GPS EXIF and lists which ones are "no-location"
#   * shows the current photo + its location on a map (or a world map if none)
#   * lets you SEARCH for a place by name, then click the map to pick
#     coordinates and write them into the file
#   * lets you COPY the location from any already-geotagged photo onto the
#     current photo
#
# Synology Photos reads location from each file's EXIF, so once you write GPS
# here and Synology re-indexes (it detects changed files; you can also force a
# re-index in Synology Photos > Settings), the photos appear on its map.
#
# ---- one-time setup -------------------------------------------------------
#   install.packages(c("shiny", "leaflet", "leaflet.extras",
#                       "exiftoolr", "magick", "DT"))
#   exiftoolr::install_exiftool()   # downloads ExifTool (only needed once)
#
# ---- run in Positron ------------------------------------------------------
#   Open this file and click "Run App", or in the Console:
#       shiny::runApp("geotag_app.R")
#   Then type a directory path in the app and click "Scan directory".
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

img_exts <- c("jpg", "jpeg", "png", "tif", "tiff", "heic", "heif", "webp", "gif", "bmp")

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
      actionButton("scan", "Scan directory", class = "btn-primary"),
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
      # --- copy-location panel ---
      tags$div(
        style = "margin:8px 0; padding:8px 10px; border:1px solid #e3e3e3; border-radius:4px; background:#fafafa;",
        tags$strong("Copy location from another photo"),
        fluidRow(
          column(9, selectInput("copy_source", NULL, choices = character(0), width = "100%")),
          column(3, tags$div(style = "margin-top:0px;",
                             actionButton("copy_btn", "Copy here",
                                          class = "btn-info", width = "100%")))
        ),
        uiOutput("copy_hint")
      ),
      actionButton("save", "Save location to photo", class = "btn-success"),
      uiOutput("save_msg")
    )
  )
)

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(photos = NULL, idx = NULL, sel = NULL, save_msg = NULL)

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
    rv$idx <- if (nrow(isolate(view_df())) > 0) 1L else NULL
    rv$sel <- NULL
    rv$save_msg <- NULL
  })

  # filtered view of the photos
  view_df <- reactive({
    df <- rv$photos
    if (is.null(df) || nrow(df) == 0) return(df)
    if (isTRUE(input$only_missing))
      df <- df[is.na(df$lat) | is.na(df$lon), , drop = FALSE]
    df
  })

  # all photos that currently have a location (candidate copy sources)
  geotagged <- reactive({
    df <- rv$photos
    if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
    df[!is.na(df$lat) & !is.na(df$lon), , drop = FALSE]
  })

  # keep idx valid when the list shrinks (e.g. after saving with filter on)
  observe({
    df <- view_df()
    if (!is.null(df) && nrow(df) > 0 && !is.null(rv$idx) && rv$idx > nrow(df))
      rv$idx <- nrow(df)
  })

  observeEvent(input$only_missing, {
    if (!is.null(rv$photos) && nrow(view_df()) > 0) { rv$idx <- 1L; rv$sel <- NULL }
  })

  current <- reactive({
    df <- view_df(); req(df, nrow(df) > 0, rv$idx)
    df[rv$idx, , drop = FALSE]
  })

  # --- table + navigation ---
  output$tbl <- renderDT({
    df <- view_df()
    if (is.null(df) || nrow(df) == 0)
      return(datatable(data.frame(Message = "No photos loaded."),
                       rownames = FALSE, options = list(dom = "t")))
    disp <- data.frame(
      File = df$file,
      Location = ifelse(is.na(df$lat) | is.na(df$lon),
                        "no-location",
                        sprintf("%.5f, %.5f", df$lat, df$lon))
    )
    datatable(disp, rownames = FALSE, selection = "single",
              options = list(pageLength = 10, dom = "tp", scrollX = TRUE))
  })

  observeEvent(input$tbl_rows_selected, {
    rv$idx <- input$tbl_rows_selected
    rv$sel <- NULL; rv$save_msg <- NULL
  })

  observeEvent(input$prev, {
    df <- view_df(); req(df, nrow(df) > 0, rv$idx)
    rv$idx <- max(1L, rv$idx - 1L); rv$sel <- NULL; rv$save_msg <- NULL
    selectRows(dataTableProxy("tbl"), rv$idx)
  })
  observeEvent(input$nxt, {
    df <- view_df(); req(df, nrow(df) > 0, rv$idx)
    rv$idx <- min(nrow(df), rv$idx + 1L); rv$sel <- NULL; rv$save_msg <- NULL
    selectRows(dataTableProxy("tbl"), rv$idx)
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
    proxy <- leafletProxy("map") |> clearGroup("existing") |> clearGroup("picked")
    if (has)
      proxy |> setView(cur$lon, cur$lat, zoom = 12) |>
        addMarkers(cur$lon, cur$lat, group = "existing", popup = "Existing location")
    else
      proxy |> setView(0, 20, zoom = 2)
  })

  # click the map to pick a new location (search just pans you to the area)
  observeEvent(input$map_click, {
    cl <- input$map_click
    rv$sel <- list(lat = cl$lat, lon = cl$lng)
    leafletProxy("map") |> clearGroup("picked") |>
      addMarkers(cl$lng, cl$lat, group = "picked", popup = "Selected location")
  })

  # --- copy location from another photo ---
  # Keep the source dropdown in sync with whichever photos currently have GPS.
  observe({
    src <- geotagged()
    if (nrow(src) == 0) {
      updateSelectInput(session, "copy_source", choices = character(0))
    } else {
      choices <- setNames(src$path,
                          sprintf("%s  (%.5f, %.5f)", src$file, src$lat, src$lon))
      keep <- isolate(input$copy_source)
      updateSelectInput(session, "copy_source", choices = choices,
                        selected = if (!is.null(keep) && keep %in% choices) keep else NULL)
    }
  })

  output$copy_hint <- renderUI({
    if (nrow(geotagged()) == 0)
      tags$em(style = "color:#888;", "No geotagged photos to copy from yet.")
    else
      tags$small(style = "color:#888;",
                 "Stages the chosen photo's coordinates onto the current photo \u2014 review on the map, then Save.")
  })

  observeEvent(input$copy_btn, {
    req(nzchar(input$copy_source))
    src <- rv$photos[rv$photos$path == input$copy_source, , drop = FALSE]
    req(nrow(src) == 1, !is.na(src$lat), !is.na(src$lon))
    cur <- current()
    if (identical(src$path, cur$path)) {
      showNotification("Source and target are the same photo.", type = "warning"); return()
    }
    rv$sel <- list(lat = src$lat, lon = src$lon)
    rv$save_msg <- NULL
    leafletProxy("map") |> clearGroup("picked") |>
      setView(src$lon, src$lat, zoom = 12) |>
      addMarkers(src$lon, src$lat, group = "picked",
                 popup = paste0("Copied from ", src$file))
    showNotification(sprintf("Copied location from %s \u2014 click 'Save location to photo' to write it.",
                             src$file), type = "message")
  })

  output$coords <- renderText({
    if (is.null(rv$sel)) {
      cur <- current()
      if (!is.na(cur$lat) && !is.na(cur$lon))
        sprintf("Search/click the map, or copy from another photo, to set a new location.\nCurrent: %.6f, %.6f",
                cur$lat, cur$lon)
      else
        "Search/click the map, or copy from another photo, to set this photo's location."
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
    ok <- tryCatch({ write_gps(cur$path, rv$sel$lat, rv$sel$lon); TRUE },
                   error = function(e) {
                     showNotification(paste("Error:", conditionMessage(e)), type = "error")
                     FALSE
                   })
    if (ok) {
      i <- which(rv$photos$path == cur$path)
      rv$photos$lat[i] <- rv$sel$lat
      rv$photos$lon[i] <- rv$sel$lon
      rv$sel <- NULL
      rv$save_msg <- div(style = "color:green;margin-top:6px;", "Saved \u2713")
      showNotification("GPS written to photo.", type = "message")
    }
  })
}

shinyApp(ui, server)
