# ============================================================================
#  geocode.R — Place-name geocoding via OpenStreetMap Nominatim
#
#  Extensibility: add alternative backends (Google, Bing, What3Words) as
#  separate functions following the same list(lat, lng, name) return contract,
#  then expose a `geocode()` wrapper that delegates to the chosen provider.
# ============================================================================

#' Geocode a place name using [OpenStreetMap](https://openstreetmap.org/copyright) Nominatim
#'
#' Issues exactly one HTTP request to the Nominatim search API and returns the
#' top result, or `NULL` when nothing is found or the request fails.
#'
#' @param q A single character string: the place name or address to search for.
#'
#' @return A named list with elements `lat` (numeric), `lng` (numeric), and
#'   `name` (character), or `NULL`.
#'
#' @details
#' This function makes a network request.  Ensure network access is available
#' and respect the
#' [Nominatim Usage Policy](https://operations.osmfoundation.org/policies/nominatim/).
#'
#' @examples
#' \dontrun{
#' geocode_osm("Oak Ridge, Tennessee")
#' }
#'
#' @export
geocode_osm <- function(q) {
  q <- trimws(q)
  if (!length(q) || !nzchar(q)) return(NULL)
  res <- tryCatch(
    httr::GET(
      "https://nominatim.openstreetmap.org/search",
      query = list(q = q, format = "json", limit = 1),
      # Nominatim requires a descriptive User-Agent; without it requests fail.
      httr::user_agent("phix R package (single-search geocode)")
    ),
    error = function(e) NULL
  )
  if (is.null(res) || httr::status_code(res) != 200) return(NULL)
  dat <- tryCatch(
    jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(dat) || length(dat) == 0) return(NULL)
  if (is.data.frame(dat) && nrow(dat) < 1) return(NULL)
  list(
    lat  = as.numeric(dat$lat[1]),
    lng  = as.numeric(dat$lon[1]),
    name = as.character(dat$display_name[1])
  )
}
