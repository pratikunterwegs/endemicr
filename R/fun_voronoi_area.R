
#' Divide an area into Voronoi polygons
#'
#' @param area_of_interest An sf-object showing the area of interest.
#' @param region_demarcation_points A list of 2-element
#' vectors which will be used to construct Voronoi polygons from the
#' Hawaii buffer. Set to Niihau (main_islands) and Nihoa (NWHI).
#'
#' @return An sf-object with the area of interest demarcated into regions.
#' @export
demarcate_regions <- function(
  area_of_interest,
  region_demarcation_points = list(
    main_islands = c(-160.1, 21.8),
    nwhi = c(-161.9, 23),
    more_west = c(-172, 25)
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

  # prepare points for voronoi
  voronoi_points <- Reduce(rbind, region_demarcation_points)
  voronoi_points <- sf::st_multipoint(voronoi_points)
  aoi_voronoi <- sf::st_voronoi(
    voronoi_points,
    sf::st_union(area_of_interest)
  )
  # convert to sfc
  aoi_voronoi <- Reduce(c, aoi_voronoi)
  aoi_voronoi <- sf::st_sfc(aoi_voronoi)
  # cast to polygon
  aoi_voronoi <- sf::st_cast(aoi_voronoi, "POLYGON")

  # set voronoi crs
  sf::st_crs(aoi_voronoi) <- 4326

  # get crossed interesections in case there are multiple points
  aoi_regions <- sf::st_intersection(
    sf::st_union(area_of_interest),
    aoi_voronoi
  )

  # determine which area covers which demarcated region
  voronoi_points <- sf::st_sfc(voronoi_points, crs = 4326)
  voronoi_points <- sf::st_cast(voronoi_points, "POINT")
  voronoi_points <- data.frame(
    names = names(region_demarcation_points),
    geometry = voronoi_points
  )
  # make sf
  voronoi_points <- sf::st_sf(voronoi_points, crs = 4326)

  # which area has which name
  name_order <- sf::st_contains(aoi_voronoi, voronoi_points)
  # unlist
  name_order <- unlist(name_order)

  # make sf
  aoi_regions <- data.frame(
    name = names(region_demarcation_points)[name_order],
    geometry = aoi_regions
  )
  aoi_regions <- sf::st_sf(aoi_regions, crs = 4326)

  # return
  return(list(
    regions = aoi_regions,
    demarcation_areas = aoi_voronoi
  ))
}
