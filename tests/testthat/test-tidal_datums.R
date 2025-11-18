test_that("returns a data frame", {
  asis_wl <- readRDS(test_path("testdata", "asis_wl.rds"))
  expect_output(str(tidal_datums(asis_wl)), "data.frame", ignore.case = TRUE)
})

test_that("returns the tidal datums", {
  asis_wl <- readRDS(test_path("testdata", "asis_wl.rds"))
  expect_true(is.numeric(tidal_datums(asis_wl)$MHW))
  expect_true(is.numeric(tidal_datums(asis_wl)$MHHW))
  expect_true(is.numeric(tidal_datums(asis_wl)$MLW))
  expect_true(is.numeric(tidal_datums(asis_wl)$MLLW))
})
