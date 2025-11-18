test_that("returns a data frame", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_output(str(longest_flood(asis_flood_depths)), "data.frame", ignore.case = TRUE)
})

test_that("returns the a data frame with the column max_consecutive_flood_days", {
  asis_flood_depths <- readRDS(test_path("testdata", "asis_flood_depths.rds"))
  expect_true(is.difftime(longest_flood(asis_flood_depths)$max_consecutive_flood_days))
})