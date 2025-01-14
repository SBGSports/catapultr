# read_CATcsv test
test_that("NROW(read_CATcsv(ofDataFileCSV())$data) == 1000", {
  skip_on_cran()
  expect_equal(NROW(read_CATcsv(ofDataFileCSV())$data), 1000)
})
