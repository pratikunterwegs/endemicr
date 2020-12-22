test_that("basic functionality works", {

  # prepare hawaii extent as string
  hawaii <- "../testdata/extent_hawaii.gpkg"

  # prepare fish range as string
  fish_range <- "../testdata/example_fish.shp"

  how_endemic <- end_check_endemic(
    aoi = hawaii,
    utm_epsg_code = 2782,
    buffer_distance_km = 1,
    sp_range = fish_range
  )

  # check this is a numeric value
  testthat::expect_true(
    is.numeric(how_endemic)
  )
})
