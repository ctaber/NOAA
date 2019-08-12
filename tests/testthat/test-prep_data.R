context('prep_data')

#eq_clean_data
test_that('eq_clean_data returns a tbl_df when given raw data frame with that includes columns: YEAR, MONTH, DAY, LATITUDE, LOGITUDE, EQ_PRIMARY, TOTAL_DEATHS', {
  filename <- system.file('extdata', 'earthquakes.tsv.gz', package = 'NOAA')
  expect_is(eq_clean_data(readr::read_delim(filename, delim = "\t")), 'tbl_df')
})

#eq_location_clean
test_that('eq_location_clean returns a tbl_df when given raw data frame with that includes columns: LOCATION_NAME', {
  filename <- system.file('extdata', 'earthquakes.tsv.gz', package = 'NOAA')
  expect_is(eq_location_clean(readr::read_delim(filename, delim = "\t")), 'tbl_df')
})

