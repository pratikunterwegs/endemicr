
#' Check and read data.
#'
#' Read in spatial data from a local file of any GDAL supported type.
#'
#' @param spatial_data A spatial representation of the area to read.
#' May be any GDAL compatible file, or an \code{sf}-*POLYGON.
#' Assumed to be in the WGS84 system (normal long-lat).
#'
#' @return An sf polygon object.
end_check_area <- function(spatial_data) {
  # check for character assumed to be filename
  # throws sf error if invalid
  if (!"sf" %in% class(spatial_data)) {

    aoi <- sf::st_read(spatial_data)

    # check for polygon
    assertthat::assert_that(
      sf::st_geometry_type(aoi) == "MULTIPOLYGON" |
        sf::st_geometry_type(aoi) == "POLYGON"
    )

    return(aoi)
  } else {

    # check for polygon
    assertthat::assert_that(
      sf::st_geometry_type(spatial_data) == "MULTIPOLYGON" |
        sf::st_geometry_type(spatial_data) == "POLYGON"
    )

    return(spatial_data)
  }
}
