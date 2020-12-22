
#' Check Hawaii endemicity
#'
#' @param hawaii_land_features A list of (named) sf-objects or spatial
#' data representations of land features of Hawaii.
#' @param buffer_distance_km A buffer distance in kilometres. How far off the
#' coast of Hawaii land features should species be considered to occur 'in/on/
#' off Hawaii'?
#' @param species_range A species range as sf-POLYGON object
#' or spatial data file names.
#' @param region_demarcation_points A data.frame, matrix, or list of 2-element
#' vectors which will be used to construct Voronoi polygons from the
#' Hawaii buffer. Set to Niihau (main_islands) and Nihoa (NWHI).
#'
#' @return We'll see.
#' @export
check_endemic_hawaii <- function(
  hawaii_land_features = list(),
  buffer_distance_km = 50,
  region_demarcation_points = list(
    main_islands = c(-160.1, 21.8),
    nwhi = c(-161.9, 23),
    more_west = c(-172, 25)
  ),
  species_range
) {

  # basic checks on regions
  # polygon check must be off because nwhi are lines
  hawaii_land_features <- lapply(X = hawaii_land_features,
                                 FUN = endemicr:::end_check_area,
                                 check_polygon = FALSE)

  # basic checks on species range
  # polygon check is on
  species_range <- end_check_area(species_range)

  # union features within list of Hawaii features
  hawaii_land_features <- lapply(FUN = sf::st_union,
                                 X = hawaii_land_features)

  # transform to UTM 4N -- this is hardcoded because there
  # is a correct option (more or less)
  hawaii_land_features <- Map(f = sf::st_transform,
                              x = hawaii_land_features,
                              crs = 2782)

  # draw buffer of specified size around each feature
  hawaii_buffer <- Map(f = sf::st_buffer,
                       x = hawaii_land_features,
                       dist = buffer_distance_km * 1000)

  # re transform to WGS84 (the crs of the fish range)
  hawaii_buffer <- Map(f = sf::st_transform,
                       x = hawaii_buffer,
                       crs = 4326)

  # union the two buffers and voronoi based on points
  hawaii_unified_buffer <- Reduce(f = c, x = hawaii_buffer)

  # prepare points for voronoi
  voronoi_points <- Reduce(rbind, region_demarcation_points)
  voronoi_points <- sf::st_multipoint(voronoi_points)
  # voronoi_points <- sf::st_sfc(voronoi_points)
  # sf::st_crs(voronoi_points) <- 4326
  hawaii_voronoi <- sf::st_voronoi(voronoi_points,
                                   sf::st_union(hawaii_unified_buffer))
  # convert to sfc
  hawaii_voronoi <- lapply(X = hawaii_voronoi, sf::st_sfc)

  # set all crs
  hawaii_voronoi <- lapply(hawaii_voronoi, function(x) {
    sf::st_crs(x) <- 4326
    return(x)
  })

  # get crossed interesections in case there are multiple points
  hawaii_split_buffer <- lapply(hawaii_voronoi, function(x) {
    sf::st_intersection(hawaii_unified_buffer, x)
  })

  # c the voronoi buffer
  hawaii_voronoi <- Reduce(c, hawaii_voronoi)
  # prep the point for id as sfc
  voronoi_points <- sf::st_sfc(voronoi_points, crs = 4326)

  # which area has which name
  name_order <- sf::st_within(st_cast(voronoi_points, "POINT"),
                              hawaii_voronoi)
  # unlist
  name_order <- unlist(name_order)

  # union these components of the split
  hawaii_split_buffer <- lapply(hawaii_split_buffer, sf::st_union)

  # determine species overlap with each of the split buffer objects
  overlaps <- lapply(hawaii_split_buffer, function(x) {
    end_check_endemic(
      aoi = x,
      utm_epsg_code = 2782,
      buffer_distance_km = 0,
      sp_range = species_range
    )
  })

  # make data
  overlaps <- data.frame(region = names(region_demarcation_points)[name_order],
                         p_species_range = unlist(overlaps))

  return(overlaps)

}
