# ============================================================================
#  config.R — App-wide constants, temp-dir setup, and file discovery
# ============================================================================

# Photo extensions scanned when listing a directory.
PHOTO_EXT <- c("jpg", "jpeg", "jpe", "tif", "tiff", "png", "webp",
               "heic", "heif", "dng", "cr2", "cr3", "nef", "arw",
               "orf", "raf", "rw2")

# Formats that browsers can display natively — serve the original file for
# these so the popup window shows the full-resolution image without any
# re-encoding or scaling.  Everything else falls back to a magick JPEG thumb.
BROWSER_PHOTO_EXT <- c("jpg", "jpeg", "jpe", "png", "webp", "heic", "heif")

THUMB_DIR <- file.path(tempdir(), "photo_gps_thumbs")
dir.create(THUMB_DIR, showWarnings = FALSE, recursive = TRUE)
unlink(list.files(THUMB_DIR, full.names = TRUE))   # clean stale thumbs
shiny::addResourcePath("thumbs", THUMB_DIR)

# Write the photo-viewer HTML page.  The popup always loads this document so
# the browser never navigates a raw image URL (which Safari and some Chrome
# configurations download instead of display).  Photo changes are pushed via
# postMessage; the page signals 'viewer_ready' once it has loaded.
writeLines(c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '<meta charset="utf-8">',
  '<title>Photo Viewer</title>',
  '<style>',
  '* { margin: 0; padding: 0; box-sizing: border-box }',
  'body { background: #111; min-height: 100vh;',
  '       display: flex; align-items: center; justify-content: center;',
  '       overflow: auto }',
  'img  { max-width: 100vw; max-height: 100vh; object-fit: contain;',
  '       display: none }',
  'p    { color: #555; font: 14px/1.5 sans-serif }',
  '</style>',
  '</head>',
  '<body>',
  '<p id="hint">Waiting for photo&hellip;</p>',
  '<img id="photo" alt="">',
  '<script>',
  'window.addEventListener("message", function(e) {',
  '  if (e.data && e.data.photoUrl) {',
  '    var img = document.getElementById("photo");',
  '    img.src = e.data.photoUrl;',
  '    img.style.display = "block";',
  '    document.getElementById("hint").style.display = "none";',
  '    document.title = decodeURIComponent(e.data.photoUrl.split("/").pop());',
  '  }',
  '});',
  '// Signal the opener that this page is ready to receive a photo URL.',
  'if (window.opener) window.opener.postMessage({ type: "viewer_ready" }, "*");',
  '</script>',
  '</body>',
  '</html>'
), con = file.path(THUMB_DIR, "viewer.html"))

# List candidate photo files in a directory, sorted.
list_photos <- function(dir) {
  if (length(dir) != 1 || is.na(dir) || !nzchar(dir) || !dir.exists(dir))
    return(character(0))
  pat <- paste0("\\.(", paste(PHOTO_EXT, collapse = "|"), ")$")
  sort(list.files(dir, pattern = pat, ignore.case = TRUE, full.names = TRUE))
}
