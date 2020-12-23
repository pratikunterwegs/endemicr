
#' Check Hawaii endemicity
#'
#' @param hawaii_land_features A list of (named) sf-objects or spatial
#' data representations of land features of Hawaii.
#' @param buffer_distance_km A buffer distance in kilometres. How far off the
#' coast of Hawaii land features should species be considered to occur 'in/on/
#' off Hawaii'?
#' @param species_range A species range as sf-POLYGON object
#' or spatial data file names.
#' @param region_demarcation_points A list of 2-element
#' vectors which will be used to construct Voronoi polygons from the
#' Hawaii buffer. Set to Niihau (main_islands) and Nihoa (NWHI).
#'
#' @return A data frame of the species global range in Hawaii.
#' @export
check_endemic_hawaii <- function(
  hawaii_land_features = list(),
  buffer_distance_km = 50,
  region_demarcation_points = list(
    main_islands = c(-160.1, 21.8),
    nwhi = c(-161.9, 23),
    more_west = c(-172, 25)
  ),
  species_range) {

  # basic checks on regions
  # polygon check must be off because nwhi are lines
  hawaii_land_features <- lapply(
    X = hawaii_land_features,
    FUN = sf::st_read
  )

  # basic checks on species range
  # polygon check is on
  end_check_area(species_range)

  # union features within list of Hawaii features
  hawaii_land_features <- lapply(
    FUN = sf::st_union,
    X = hawaii_land_features
  )

  # transform to UTM 4N -- this is hardcoded because there
  # is a correct option (more or less)
  hawaii_land_features <- Map(
    f = sf::st_transform,
    x = hawaii_land_features,
    crs = 2782
  )

  # draw buffer of specified size around each feature
  hawaii_buffer <- Map(
    f = sf::st_buffer,
    x = hawaii_land_features,
    dist = buffer_distance_km * 1000
  )

  # re transform to WGS84 (the crs of the fish range)
  hawaii_buffer <- Map(
    f = sf::st_transform,
    x = hawaii_buffer,
    crs = 4326
  )

  # union the two buffers and voronoi based on points
  hawaii_unified_buffer <- Reduce(f = c, x = hawaii_buffer)

  # use the voronoi function to divide the archipelago
  hawaii_regions <- endemicr::demarcate_regions(
    area_of_interest = hawaii_unified_buffer,
    region_demarcation_points = region_demarcation_points
  )

  # determine species overlap with each of the split buffer objects
  overlaps <- end_check_endemic(
    aoi = hawaii_regions,
    utm_epsg_code = 2782,
    buffer_distance_km = 0,
    sp_range = species_range
  )

  return(overlaps)
}
