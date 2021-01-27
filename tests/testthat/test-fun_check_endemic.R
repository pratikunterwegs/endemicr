test_that("basic functionality works", {

  # prepare hawaii extent as string
  hawaii <- sf::st_read("../testdata/extent_hawaii.gpkg")

  # prepare fish range as string
  fish_range <- sf::st_read("../testdata/example_fish/example_fish.shp")

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

  # test range
  testthat::expect_lte(
    sum(how_endemic$p_range),
    1.0
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

test_that("range proportion within 1", {

  # prepare hawaii extent as string
  hawaii <- sf::st_read("../testdata/extent_hawaii.gpkg")

  # prepare fish range as string
  fish_range <- sf::st_read("../testdata/excess_range_fish/data_0.shp")

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

  # test range
  testthat::expect_lte(
    sum(how_endemic$p_range),
    1.0
  )
})
