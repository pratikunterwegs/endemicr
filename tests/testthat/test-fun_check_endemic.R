test_that("basic functionality works", {

  # prepare hawaii extent as string
  hawaii <- sf::st_read("../testdata/extent_hawaii.gpkg")

  # prepare fish range as string
  fish_range <- sf::st_read("../testdata/example_fish.shp")

  how_endemic <- end_check_endemic(
    aoi = hawaii,
    utm_epsg_code = 2782,
    buffer_distance_km = 1,
    sp_range = fish_range
  )

  # check this is a numeric value
  testthat::expect_true(
    is.data.frame(how_endemic)
  )

  # expect fail
  testthat::expect_error(
    end_check_endemic(
      aoi = "anyoldtext",
      utm_epsg_code = 2782,
      buffer_distance_km = 1,
      sp_range = "alsogarbage"
    )
  )
})
