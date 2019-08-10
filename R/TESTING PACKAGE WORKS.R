# ##TESTING
#
# ##read in and clean data
# data<- readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#   eq_clean_data() %>% ##**************warning message************
#   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#   eq_location_clean()
#
# data<-eq_get_data(country = c('MEXICO','USA'), start_date = '2001-01-01', end_date = '2005-01-01')
# data<-eq_get_data(country = 'MEXICO', start_date = '2001-01-01')
#
#
# data %>% ##plot timeline and label
#   ggplot2::ggplot() +
#   geom_timeline(
#     aes(
#       x = DATE,
#       y = COUNTRY,
#       fill = TOTAL_DEATHS,
#       size = EQ_PRIMARY)
#   ) +
#   geom_timeline_label(
#     aes(
#       x = DATE,
#       y = COUNTRY,
#       magnitude = EQ_PRIMARY,
#       label = LOCATION_NAME,
#       n_max=3) #number of lables to show,
#
#   )
#
#
# test<-data %>%##plot interactive map  *************ONLY HAS LOCATION_NAME if not using get_data????
#      dplyr::mutate(popup_text = eq_create_label(.)) %>%
#      eq_map(annot_col = "popup_text")
# class(test)
