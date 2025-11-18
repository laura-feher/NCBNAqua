test_that("returns a data frame", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_output(str(daily_mean_flood(asis_flood_depths, summary_metric = "min")), "data.frame", ignore.case = TRUE)
})

test_that("returns the a data frame with the columns elev_navd88, daily_mean_min_flood_depth, and daily_mean_min_flood_elev", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_true(is.numeric(daily_mean_flood(asis_flood_depths, summary_metric = "min")$elev_navd88))
  expect_true(is.numeric(daily_mean_flood(asis_flood_depths, summary_metric = "min")$daily_mean_min_flood_depth))
  expect_true(is.numeric(daily_mean_flood(asis_flood_depths, summary_metric = "min")$daily_mean_min_flood_elev))
})