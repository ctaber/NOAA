context('get_data')


test_that('get_data can take single or multiple countries as input',{
  filename <- system.file('extdata', 'earthquakes.tsv.gz', package = 'NOAA')
  one_country<-eq_get_data(filename, country = 'USA', start_date = '2001-01-01', end_date = '2003-01-01')
  two_countries<-eq_get_data(filename, country = c('MEXICO','USA'), start_date = '2001-01-01', end_date = '2003-01-01')

  expect_true(one_country %>% dplyr::select(COUNTRY) %>% unique()%>% nrow() ==1)
  expect_true(two_countries %>% dplyr::select(COUNTRY) %>% unique()%>% nrow() ==2)


} )


