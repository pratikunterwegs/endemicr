

#' Check endemicity based on species range.
#'
#' @param aoi A spatial representation of the area in which the
#' species is expected to be endemic. May be any GDAL compatible file, or an
#' \code{sf}-*POLYGON. Assumed to be in the WGS84 system (normal long-lat).
#' @param buffer_distance_km A buffer distance in kilometres. Primarily
#' useful for marine species which may be considered endemic to waters off a
#' feature such as an island.
#' @param sp_range A spatial representation of the species' range.
#' May be any GDAL compatible file, or an \code{sf}-*POLYGON.
#' Assumed to be in the WGS84 system (normal long-lat).
#' @param utm_epsg_code An EPSG code (see e.g. epsg.io/4326) for the UTM system
#' appropriate to the area of interest. Necessary to draw buffers in metres when
#' the coordinates are provided in longitude-latitude.
#'
#' @return The proportion of the species range within a buffer around the area
#' of interest.
#' @export
end_check_endemic <- function(aoi,
                              utm_epsg_code,
                              buffer_distance_km,
                              sp_range) {

  # process area of interest
  aoi <- end_check_area(aoi)
  # examine species range for issues
  sp_range <- end_check_area(sp_range)

  # check for geographic system on area of interest
  # and check that UTM EPSG code is valid
  if (sf::st_is_longlat(aoi)) {
    assertthat::assert_that(
      is.numeric(utm_epsg_code),
      msg = "check_endemic: EPSG code is not numeric"
    )

    # check the EPSG code is for UTM
    assertthat::assert_that(
      !sf::st_is_longlat(sf::st_crs(utm_epsg_code)),
      msg = "check_endemic: EPSG code is for a geographic system"
    )

    # transform area of interest to the supplied UTM CRS
    aoi <- sf::st_transform(
      x = aoi,
      crs = utm_epsg_code
    )
  }


  # draw polygon buffer if greater than 0
  if (buffer_distance_km > 0) {

    # convert argument to metres
    buffer_distance_km <- buffer_distance_km * 1000
    # flatten features using st union
    aoi <- sf::st_union(aoi)
    # prepare buffer which is an sfc_multipolygon
    aoi_buffer <- sf::st_buffer(
      x = aoi,
      dist = buffer_distance_km
    )

    # retransform to geographic coordinates
    aoi_buffer <- sf::st_transform(
      x = aoi_buffer,
      crs = 4326
    )
  }

  # check intersection with species range
  # first check for matching crs
  assertthat::assert_that(
    sf::st_crs(aoi_buffer) == sf::st_crs(sp_range),
    msg = "check_endemic: area and species range CRS are not equal"
  )

  # get intersection
  overlap <- sf::st_intersection(aoi_buffer, sp_range)

  # get
  # proportion of species range represented by the ovelap
  # with the area of interest
  # this is somewhat sensitive to the buffer size
  pct_range <- sf::st_area(overlap) / sf::st_area(sp_range)

  return(pct_range)
}
