# phix — Photo EXIF GPS and Date Editor

A Shiny application for adding or correcting GPS
location and creation-date metadata in digitized or scanned photos (film
prints, slides, negatives).

Scanned legacy images typically have no GPS coordinates and either a wrong creation
date (the scan date) or none at all. **phix** lets you work through a folder
of such images and write correct EXIF metadata directly into each image file. Locations 
are selected by clicking or searching a map or copied from other photos.

[Exchangeable Image File Format](https://en.wikipedia.org/wiki/Exif) (EXIF) is a widely used standard for storing metadata in digital image 
files.

---

## Installation

```r
# Install from CRAN (once released)
install.packages("phix")

# Or install the development version from GitHub
# install.packages("pak")
pak::pak("go-ski/phix")
```

**One-time ExifTool setup** (required before first use):

```r
exiftoolr::install_exiftool()
```

---

## Usage

```r
library(phix)
run_phix()
```

1. Paste a folder path into the *Photo directory* box and click **Load photos**.
2. For each photo:
   - **GPS**: locate the photo's position using either of two methods:
     - *Search* — type a place name and click **Search**; the Nominatim
       geocoder pans the Leaflet map to the result.
     - *Click* — switch to the **OpenStreetMap** or **Esri Satellite** layer,
       navigate to the correct spot, and click the map to drop a red marker.
     Then click **Write active clipboard to photo** to write the coordinates.
   - **Date**: adjust the date/time fields; saving will also write the new
     date if it differs from the stored value.
   - Use **Copy location** / **Write active clipboard** to repeat a GPS
     location across multiple photos; **Copy date** works the same for dates.
3. All edits are written in place (`-overwrite_original`).  
   **Back up your originals before editing.**

---

## Features

### GPS location
- Browse a folder of photos; each row in the table shows the filename, its
  current GPS coordinates (or *no-location*), and its creation date.
- An interactive Leaflet map with two switchable tile layers —
  **OpenStreetMap** (street detail) and **Esri World Imagery** (satellite/aerial).
- Click **📷 View photo** to open the current photo in a separate resizable
  popup window; the window updates automatically as you navigate.
- **Search by place name** queries the **OpenStreetMap Nominatim** geocoder
  and pans the map to the first matching result.
- **Click anywhere on the map** to drop a red "pending" marker at the desired
  coordinates.

### Creation date / time
- A date picker and a UTC time field let you set or correct the creation date.
  The current photo's `DateTimeOriginal` tag is read and pre-filled.
- Saving writes three tags: `DateTimeOriginal`, `CreateDate` /
  `DateTimeDigitized`, and `ModifyDate`.

### Navigation
- Prev / Next buttons, arrow-key shortcuts, and clicking any table row move
  between photos.

### Directory auto-complete
- Typing in the directory box suggests matching sub-directories in a
  dropdown with keyboard navigation.

---

## Utility functions

The EXIF helper functions are also exported for programmatic use:

| Function | Description |
|---|---|
| `list_photos(dir)` | List supported photo files in a directory |
| `read_meta(paths)` | Read GPS, orientation, and datetime from photos |
| `write_metadata(path, gps, dt)` | Write GPS and/or datetime in one ExifTool call |
| `write_gps(path, lat, lng)` | Write GPS coordinates only |
| `write_datetime(path, dt)` | Write creation datetime only |
| `geocode_osm(q)` | Geocode a place name via OpenStreetMap Nominatim |

---

## Requirements

| Package | Purpose |
|---------|---------|
| shiny | Web app framework |
| bslib | UI layout and theming (Bootstrap 5) |
| leaflet | Interactive map |
| exiftoolr | R wrapper for ExifTool |
| magick | Thumbnail generation / orientation correction |
| DT | Interactive photo-list table |
| httr | HTTP requests to Nominatim geocoder |
| jsonlite | Parse Nominatim JSON responses |

**System requirement:** [ExifTool](https://exiftool.org) must be installed.
Use `exiftoolr::install_exiftool()` for a self-contained R-managed copy.

---

## Extensibility

The helper functions `write_gps()` and `write_datetime()` follow the same
pattern: build an ExifTool argument list and call `exiftoolr::exif_call()`.
Additional tags — caption (`ImageDescription`), copyright, camera make/model,
keywords, etc. — can be supported by adding equivalent helper functions and
corresponding UI inputs.
