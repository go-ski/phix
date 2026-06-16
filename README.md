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
- An interactive Leaflet map with two switchable tile layers —
  **OpenStreetMap** (street detail) and **Esri World Imagery** (satellite/aerial)
  — occupies the main panel.  Click **📷 View photo** to open the current photo
  in a separate resizable popup window; the window updates automatically as you
  navigate between photos.
- **Search by place name** using the text box and *Search* button, which
  queries the **OpenStreetMap Nominatim** geocoder and pans the map to the
  first matching result.
- **Click anywhere on the map** to drop a red "pending" marker at the desired
  coordinates — works on both the OpenStreetMap and Esri Satellite layers.
- **Save changes → photo** writes `GPSLatitude`, `GPSLatitudeRef`,
  `GPSLongitude`, `GPSLongitudeRef`, and `GPSMapDatum=WGS-84` to the file and
  advances to the next photo.
- **Copy location** captures the current photo's GPS coordinates into an
  in-app clipboard.  **Paste & save** writes that location to the photo you
  are viewing and advances — efficient when many consecutive photos share the
  same location.

### Creation date / time
- A date picker and a UTC time field let you set or correct the creation date.
  The current photo's `DateTimeOriginal` tag is read and pre-filled into the
  inputs for editing.
- **Save changes → photo** saves the entered date if it has changed from the
  stored value, writing three tags — `DateTimeOriginal` (original capture time),
  `CreateDate` / `DateTimeDigitized` (digitisation time), and `ModifyDate` —
  all set to the same timestamp.  If a GPS point was also selected on the map,
  both GPS and date are written in a single ExifTool call.
- **Copy date** captures the current photo's date into an in-app clipboard.
  **Paste date** writes that date to the photo you are viewing and advances —
  efficient when many consecutive photos share the same date.

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
| bslib | UI layout and theming (Bootstrap 5 page, sidebar, cards) |
| leaflet | Interactive map (OpenStreetMap + Esri Satellite tile layers) |
| exiftoolr | R wrapper for ExifTool |
| magick | Thumbnail generation / orientation correction |
| DT | Interactive photo-list table |
| httr | HTTP requests to OpenStreetMap Nominatim geocoder |
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
   - **GPS**: locate the photo's position using either of two methods:
     - *Search* — type a place name and click **Search**; the Nominatim
       geocoder pans the Leaflet map to the result.
     - *Click* — switch to the **OpenStreetMap** or **Esri Satellite** layer,
       navigate to the correct spot, and click the map to drop a red marker.
     Then click **Save changes → photo** to write the coordinates.
   - **Date**: adjust the date/time fields; **Save changes → photo** will also
     write the new date if it differs from the saved value.
   - Use **Copy location** / **Paste & save** to repeat a GPS location across
     multiple photos; use **Copy date** / **Paste & save date** to do the same for dates.
3. All edits are written in place (`-overwrite_original`).  
   **Back up your originals before editing.**

---

## Extensibility

The helper functions `write_gps()` and `write_datetime()` follow the same
pattern: build an ExifTool argument list and call `exiftoolr::exif_call()`.
Additional tags — caption (`ImageDescription`), copyright, camera make/model,
keywords, etc. — can be supported by adding equivalent helper functions and
corresponding UI inputs.