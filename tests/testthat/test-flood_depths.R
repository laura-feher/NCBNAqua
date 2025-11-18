test_that("returns a data frame", {
  asis_wl <- readRDS(test_path("testdata", "asis_wl.rds"))
  asis_elev_df <- readRDS(test_path("testdata", "asis_elev_df.rds"))
  expect_output(str(flood_depths(wl_data = asis_wl, elev_df = asis_elev_df)), "data.frame", ignore.case = TRUE)
})

test_that("returns the a data frame with the columns elev_navd88, flood_depth, and wl_above_marsh", {
  asis_wl <- readRDS(test_path("testdata", "asis_wl.rds"))
  asis_elev_df <- readRDS(test_path("testdata", "asis_elev_df.rds"))
  expect_true(is.numeric(flood_depths(wl_data = asis_wl, elev_df = asis_elev_df)$elev_navd88))
  expect_true(is.numeric(flood_depths(wl_data = asis_wl, elev_df = asis_elev_df)$flood_depth))
  expect_true(is.numeric(flood_depths(wl_data = asis_wl, elev_df = asis_elev_df)$wl_above_marsh))
})