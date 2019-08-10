context('get_data')


test_that('get_data can take single or multiple countries as input',{
  one_country<-eq_get_data(country = 'USA', start_date = '2001-01-01', end_date = '2003-01-01')
  two_countries<-eq_get_data(country = c('MEXICO','USA'), start_date = '2001-01-01', end_date = '2003-01-01')

  expect_true(one_country %>% select(COUNTRY) %>% unique()%>% nrow() ==1)
  expect_true(two_countries %>% select(COUNTRY) %>% unique()%>% nrow() ==2)


} )


