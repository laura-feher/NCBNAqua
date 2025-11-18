test_that("returns a data frame", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_output(str(time_flooded(asis_flood_depths)), "data.frame", ignore.case = TRUE)
})

test_that("returns the a data frame with the columns total_time_recorded, flooding_time, percent_time_flooded", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_true(is.numeric(time_flooded(asis_flood_depths)$total_time_recorded))
  expect_true(is.numeric(time_flooded(asis_flood_depths)$flooding_time))
  expect_true(is.numeric(time_flooded(asis_flood_depths)$percent_time_flooded))
})