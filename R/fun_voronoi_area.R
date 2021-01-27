
#' Divide an area into Voronoi polygons
#'
#' @param area_of_interest An sf-object showing the area of interest.
#' Must have a CRS. Voronoi points will be converted to this CRS.
#' @param region_demarcation_points A list of 2-element
#' vectors which will be used to construct Voronoi polygons from the
#' Hawaii buffer. Set to Niihau (main_islands) and Nihoa (NWHI).
#' Must be specified in decimal degrees (WGS84 CRS: EPSG code = 4326).
#' @return An sf-object with the area of interest demarcated into regions.
#' @export
demarcate_regions <- function(
                              area_of_interest,
                              region_demarcation_points = list(
                                main_islands = c(-160.1, 21.8),
                                nwhi = c(-161.9, 23)
                              )) {

  # checks
  assertthat::assert_that(
    any(c("sf", "sfc") %in% class(area_of_interest)),
    msg = "voronoi_area: area of interest must be sf or sfc"
  )

  # check that area of interest is a POLYGON
  assertthat::assert_that(
    all(sf::st_geometry_type(area_of_interest) == "MULTIPOLYGON" |
      sf::st_geometry_type(area_of_interest) == "POLYGON")
  )

  # check demarcation points
  assertthat::assert_that(
    is.list(region_demarcation_points),
    msg = "voronoi_area: region demarcation points must be a list"
  )

  # check list contents
  assertthat::assert_that(
    unique(unlist(Map(length, region_demarcation_points))) == 2,
    msg = "voronoi_area: region demarcation points must be 2-element vectors"
  )

  # check for crs
  assertthat::assert_that(
    !is.na(sf::st_crs(area_of_interest)),
    msg = "voronoi_area: area of interest has no CRS"
  )

  # prepare points for voronoi
  voronoi_points <- Reduce(rbind, region_demarcation_points)
  voronoi_points <- sf::st_multipoint(voronoi_points)

  # make sfc
  voronoi_points <- sf::st_sfc(voronoi_points, crs = 4326)

  # transform to area crs
  voronoi_points <- sf::st_transform(
    voronoi_points,
    crs = sf::st_crs(area_of_interest)
  )

  # make voronoi
  aoi_voronoi <- sf::st_voronoi(
    voronoi_points,
    sf::st_union(area_of_interest)
  )

  # THIS IS SOME NEW STUFF, A GEOMETRY COLLECTION
  aoi_voronoi <- sf::st_collection_extract(aoi_voronoi,
    type = "POLYGON"
  )

  # get crossed interesections in case there are multiple points
  aoi_regions <- sf::st_intersection(
    sf::st_union(area_of_interest),
    aoi_voronoi
  )

  # determine which area covers which demarcated region
  voronoi_points <- sf::st_cast(voronoi_points, "POINT")
  voronoi_points <- data.frame(
    names = names(region_demarcation_points),
    geometry = voronoi_points
  )

  # make sf
  voronoi_points <- sf::st_sf(voronoi_points,
    crs = sf::st_crs(aoi_voronoi)
  )

  # which area has which name
  name_order <- sf::st_contains(aoi_voronoi, voronoi_points)
  # unlist
  name_order <- unlist(name_order)

  # make sf
  aoi_regions <- data.frame(
    name = names(region_demarcation_points)[name_order],
    geometry = aoi_regions
  )
  aoi_regions <- sf::st_sf(aoi_regions,
    crs = sf::st_crs(aoi_voronoi)
  )

  # free message
  message(
    paste0("crs used is ", sf::st_crs(aoi_regions, )$input)
  )

  # return
  return(list(
    regions = aoi_regions,
    demarcation_areas = aoi_voronoi
  ))
}
