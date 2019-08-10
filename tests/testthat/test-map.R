context('eq_map')

#eq_map
test_that('eq_map returns a leaflet map when given valid input', {
  valid_input <-eq_get_data(country = 'MEXICO', start_date = '2001-01-01')
  map <- eq_map(valid_input)
  expect_is(map, 'leaflet')
})

#eq_create_label
test_that('eq_create_label only returns non-NA labels and data', {
  data<- eq_get_data(country = 'MEXICO', start_date = '2006-09-09')
  data<- data%>%select(DATE, LOCATION_NAME, EQ_PRIMARY, TOTAL_DEATHS)

  # 'Location' should not be in label if LOCATION_NAME data is NA
  NA_LOCATION_NAME <- data %>% filter(DATE=='2006-09-10') %>% eq_create_label()
  expect_true(stringr::str_count(NA_LOCATION_NAME, 'Location') ==0)

  # 'Total deaths' should not be in label if TOTAL_DEATHS data is NA
  NA_TOTAL_DEATHS <- data %>% filter(DATE=='2008-02-09') %>% eq_create_label()
  expect_true(stringr::str_count(NA_TOTAL_DEATHS, 'Total deaths') ==0)

  # 'Date', 'Location', 'Total deaths', and 'Magnitude', should all be in label is no NAs
  NO_NA <- data %>% filter(DATE=='2017-09-08') %>% eq_create_label()
  expect_true(stringr::str_count(NO_NA, 'Total deaths') ==1)
  expect_true(stringr::str_count(NO_NA, 'Location') ==1)
  expect_true(stringr::str_count(NO_NA, 'Date') ==1)
  expect_true(stringr::str_count(NO_NA, 'Magnitude') ==1)

})

