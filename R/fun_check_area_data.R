
#' Check and read data.
#'
#' Read in spatial data from a local file of any GDAL supported type.
#'
#' @param spatial_data A spatial representation of the area to read.
#' Must be an \code{sf}-*POLYGON.
#' Assumed to be in the WGS84 system (normal long-lat).
#'
#' @return An sf polygon object.
end_check_area <- function(spatial_data) {
  # throws sf error if invalid
  assertthat::assert_that(
    all(any(c("sf", "sfc") %in% class(spatial_data))),
    msg = "check area: object is not an sf-object"
  )
}
