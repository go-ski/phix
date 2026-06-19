# ============================================================================
#  photos.R — File discovery
# ============================================================================

#' List photo files in a directory
#'
#' Returns the full paths of all supported photo files found directly inside
#' `dir`, sorted alphabetically.  Sub-directories are not recursed.
#'
#' Supported extensions: jpg, jpeg, jpe, tif, tiff, png, webp, heic, heif,
#' dng, cr2, cr3, nef, arw, orf, raf, rw2.
#'
#' @param dir A single character string giving the directory path to search.
#'
#' @return A character vector of full file paths (zero-length if none found or
#'   `dir` does not exist).
#'
#' @examples
#' # List photos in a temporary directory
#' tmp <- tempdir()
#' list_photos(tmp)
#'
list_photos <- function(dir) {
  if (length(dir) != 1 || is.na(dir) || !nzchar(dir) || !dir.exists(dir))
    return(character(0))
  pat <- paste0("\\.(", paste(.phix_env$PHOTO_EXT, collapse = "|"), ")$")
  sort(list.files(dir, pattern = pat, ignore.case = TRUE, full.names = TRUE))
}
