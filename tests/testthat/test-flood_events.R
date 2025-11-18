test_that("returns a data frame", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_output(str(flood_events(asis_flood_depths)), "data.frame", ignore.case = TRUE)
})

test_that("returns the a data frame with the columns total_flood_events", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_true(is.numeric(flood_events(asis_flood_depths)$total_flood_events))
})