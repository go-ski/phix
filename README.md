# Photo EXIF Editor

A Shiny app for adding or correcting GPS location and creation date metadata
in digitised or scanned photos (film prints, slides, negatives).

Scanned images typically have no GPS coordinates and either a wrong creation
date (the scan date) or none at all.  This app lets you work through a folder
of such images and write correct EXIF metadata directly into each file using
[ExifTool](https://exiftool.org), the de-facto standard command-line metadata
tool, driven from R via the
[exiftoolr](https://CRAN.R-project.org/package=exiftoolr) package.

---

## Features

### GPS location
- Browse a folder of photos; each row in the table shows the filename, its
  current GPS coordinates (or *no-location*), and its creation date.
- View the current photo as a thumbnail alongside an interactive
  Street / Satellite map (Leaflet + OpenStreetMap / Esri).
- **Search** a place name (OpenStreetMap Nominatim) to pan the map to a
  known location.
- **Click the map** to drop a red "pending" marker at the desired coordinates.
- **Save selected point → photo** writes `GPSLatitude`, `GPSLatitudeRef`,
  `GPSLongitude`, `GPSLongitudeRef`, and `GPSMapDatum=WGS-84` to the file and
  advances to the next photo.
- **Copy location** captures the current photo's GPS coordinates into an
  in-app clipboard.  **Paste & save** writes that location to the photo you
  are viewing and advances — efficient when many consecutive photos share the
  same location.

### Creation date / time
- A date picker and a UTC time field let you set or correct the three main
  EXIF date tags: `DateTimeOriginal`, `CreateDate`, and `ModifyDate`.
- **Save date → photo** writes the entered value and updates the table.

### Navigation
- Prev / Next buttons and clicking any table row move between photos.
- The map re-centres and the thumbnail updates automatically.

### Directory auto-complete
- Typing in the directory box suggests matching sub-directories in a
  dropdown, with keyboard (arrow-key) navigation.

---

## Requirements

R packages (installed automatically on first run if missing):

| Package | Purpose |
|---------|---------|
| shiny | Web app framework |
| leaflet | Interactive map |
| exiftoolr | R wrapper for ExifTool |
| magick | Thumbnail generation / orientation correction |
| DT | Interactive photo-list table |
| httr | HTTP requests to Nominatim |
| jsonlite | Parse Nominatim JSON responses |

**ExifTool** itself is installed automatically by `exiftoolr::install_exiftool()`
on first run if it is not already on your `PATH`.

---

## Usage

```r
shiny::runApp("shinyEXIF.R")
```

Or open the file in Positron / RStudio and click **Run App**.

1. Paste a folder path into the *Photo directory* box and click **Load photos**.
2. For each photo:
   - **GPS**: search or click the map, then click **Save selected point → photo**.
   - **Date**: adjust the date/time fields, then click **Save date → photo**.
   - Use **Copy / Paste & save** to repeat a location across multiple photos.
3. All edits are written in place (`-overwrite_original`).  
   **Back up your originals before editing.**

---

## Extensibility

The helper functions `write_gps()` and `write_datetime()` follow the same
pattern: build an ExifTool argument list and call `exiftoolr::exif_call()`.
Additional tags — caption (`ImageDescription`), copyright, camera make/model,
keywords, etc. — can be supported by adding equivalent helper functions and
corresponding UI inputs.