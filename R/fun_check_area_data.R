
#' Check and read data.
#'
#' Read in spatial data from a local file of any GDAL supported type.
#'
#' @param spatial_data A spatial representation of the area to read.
#' May be any GDAL compatible file, or an \code{sf}-*POLYGON.
#' Assumed to be in the WGS84 system (normal long-lat).
#' @param check_polygon Whether to check for POLYGON type and break if not.
#'
#' @return An sf polygon object.
end_check_area <- function(spatial_data,
                           check_polygon = TRUE) {
  # check for character assumed to be filename
  # throws sf error if invalid
  if (!any(c("sf", "sfc") %in% class(spatial_data))) {
    aoi <- sf::st_read(spatial_data)

    if (check_polygon) {
      # check for polygon
      assertthat::assert_that(
        sf::st_geometry_type(aoi) == "MULTIPOLYGON" |
          sf::st_geometry_type(aoi) == "POLYGON"
      )
    }

    return(aoi)
  } else {
    if (check_polygon) {
      # check for polygon
      assertthat::assert_that(
        all(
          sf::st_geometry_type(spatial_data) == "MULTIPOLYGON" |
            sf::st_geometry_type(spatial_data) == "POLYGON"
        )
      )
    }

    return(spatial_data)
  }
}
