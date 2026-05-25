# geotag_app.R
# ---------------------------------------------------------------------------
# Add / view GPS locations for photos (e.g. a Synology Photos library).
# OpenStreetMap base map, with a SERVER-SIDE place-name search (no API key,
# and not subject to the flaky addSearchOSM JS control).
#
# Given a directory of photos, this app:
#   * reads each photo's GPS EXIF and lists which ones are "no-location"
#   * shows the current photo + its location on a map (or a world map if none)
#   * lets you search a place by name (R queries Nominatim), pans/drops a pin
#     there, and writes the chosen coordinates into the file
#   * you can also just click anywhere on the map to set/fine-tune the pin
#
# Synology Photos reads location from each file's EXIF, so once you write GPS
# here and Synology re-indexes (it detects changed files; you can also force a
# re-index in Synology Photos > Settings), the photos appear on its map.
#
# ---- one-time setup -------------------------------------------------------
#   install.packages(c("shiny", "leaflet", "httr2",
#                       "exiftoolr", "magick", "DT"))
#   exiftoolr::install_exiftool()   # downloads ExifTool (only needed once)
#
# ---- run in Positron ------------------------------------------------------
#   Open this file and click "Run App", or:  shiny::runApp("geotag_app.R")
#
# ---- safety ---------------------------------------------------------------
#   Files are edited IN PLACE. Test on a COPY first.
#   Remove "-overwrite_original" below to keep a "<name>_original" backup.
# ---------------------------------------------------------------------------

library(shiny)
library(leaflet)
library(httr2)
library(exiftoolr)
library(magick)
library(DT)

# Nominatim asks for a descriptive User-Agent with contact info. Put your own
# email here so you're a good citizen of the free service.
NOMINATIM_UA <- "geotag-shiny-app/1.0 (georgeost@gmail.com)"

img_exts <- c("jpg", "jpeg", "png", "tif", "tiff", "heic", "heif", "webp", "gif", "bmp")

# --- helpers ---------------------------------------------------------------

list_images <- function(dir) {
  pat <- paste0("\\.(", paste(img_exts, collapse = "|"), ")$")
  list.files(dir, pattern = pat, ignore.case = TRUE,
             full.names = TRUE, recursive = TRUE)
}

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

# Server-side geocoder: ask Nominatim for matches, return a data.frame.
# Returns NULL on any failure; the caller shows a notification.
geocode_osm <- function(query) {
  if (!nzchar(trimws(query))) return(NULL)
  resp <- tryCatch(
    request("https://nominatim.openstreetmap.org/search") |>
      req_url_query(q = query, format = "jsonv2", limit = 8, addressdetails = 1) |>
      req_user_agent(NOMINATIM_UA) |>
      req_timeout(20) |>
      req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  dat <- tryCatch(resp_body_json(resp, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.null(dat) || length(dat) == 0 || is.null(dat$display_name)) return(NULL)
  data.frame(
    label = dat$display_name,
    lat   = suppressWarnings(as.numeric(dat$lat)),
    lon   = suppressWarnings(as.numeric(dat$lon)),
    stringsAsFactors = FALSE
  )
}

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
      # --- place-name search (server-side via Nominatim) ---
      fluidRow(
        column(9, textInput("place", NULL, width = "100%",
                            placeholder = "Search a town / place, then press Search")),
        column(3, actionButton("go_search", "Search", width = "100%"))
      ),
      uiOutput("search_choices"),
      fluidRow(
        column(5, uiOutput("preview")),
        column(7, leafletOutput("map", height = 380))
      ),
      tags$br(),
      verbatimTextOutput("coords"),
      actionButton("save", "Save location to photo", class = "btn-success"),
      uiOutput("save_msg")
    )
  )
)

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  rv <- reactiveValues(photos = NULL, idx = NULL, sel = NULL,
                       save_msg = NULL, search_results = NULL)

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

  view_df <- reactive({
    df <- rv$photos
    if (is.null(df) || nrow(df) == 0) return(df)
    if (isTRUE(input$only_missing))
      df <- df[is.na(df$lat) | is.na(df$lon), , drop = FALSE]
    df
  })

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

  # --- map (plain OpenStreetMap; search is handled server-side below) ---
  output$map <- renderLeaflet({
    leaflet() |> addTiles() |> setView(lng = 0, lat = 20, zoom = 2)
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

  # helper to drop the candidate pin and remember the selection
  set_pick <- function(lat, lon, label = "Selected location", zoom = NULL) {
    rv$sel <- list(lat = lat, lon = lon)
    p <- leafletProxy("map") |> clearGroup("picked")
    if (!is.null(zoom)) p <- p |> setView(lon, lat, zoom = zoom)
    p |> addMarkers(lon, lat, group = "picked", popup = label)
  }

  # --- place-name search (server-side) ---
  do_search <- function() {
    req(nzchar(input$place))
    res <- geocode_osm(input$place)
    if (is.null(res)) {
      rv$search_results <- NULL
      showNotification("No matches (or the search service didn't respond).",
                       type = "warning")
      return()
    }
    rv$search_results <- res
    set_pick(res$lat[1], res$lon[1], res$label[1], zoom = 12)  # jump to best match
  }
  observeEvent(input$go_search, do_search())
  # allow pressing Enter in the box to search too
  observeEvent(input$place_search, do_search(), ignoreInit = TRUE)

  output$search_choices <- renderUI({
    res <- rv$search_results
    if (is.null(res) || nrow(res) == 0) return(NULL)
    selectInput("search_pick", "Matches (pick the right one):",
                choices = setNames(seq_len(nrow(res)), res$label),
                width = "100%")
  })

  observeEvent(input$search_pick, {
    res <- rv$search_results; req(res)
    i <- as.integer(input$search_pick)
    if (is.na(i) || i < 1 || i > nrow(res)) return()
    set_pick(res$lat[i], res$lon[i], res$label[i], zoom = 12)
  })

  # click the map to set / fine-tune the pin
  observeEvent(input$map_click, {
    cl <- input$map_click
    set_pick(cl$lat, cl$lng)
  })

  output$coords <- renderText({
    if (is.null(rv$sel)) {
      cur <- current()
      if (!is.na(cur$lat) && !is.na(cur$lon))
        sprintf("Search a place or click the map to choose a new location.\nCurrent: %.6f, %.6f",
                cur$lat, cur$lon)
      else
        "Search a place or click the map to choose a location for this photo."
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
