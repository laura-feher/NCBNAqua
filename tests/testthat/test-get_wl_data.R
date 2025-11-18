test_that("returns a data frame of water level data", {
  skip('skip - API requires authentication')
  expect_output(str(get_wl_data(park = "ASIS")), "$ water_level", fixed = TRUE)
})
