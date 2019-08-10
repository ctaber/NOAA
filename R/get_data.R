#' Read in Data
#'
#' This function takes a country name in all capitals, a start date as a string in 'YYYY-MM-DD' format, and optionally an end date in the same format, read in the earthquake data, cleans it, and turns a filtered cleaned dataframe.
#'
#' @param country A country name in all capital letters as a sting, eg. 'MEXICO', or a vector of country names, eg. c('MEXICO','USA')
#'
#' @param start_date A date as a string in 'YYYY-MM-DD' format to filter observations for
#'
#' @param end_date An optional paramater, a date as a string in 'YYYY-MM-DD' format to filter observations for. Defaults to todays date if not overridden.
#'
#' @importFrom lubridate today ymd
#'
#' @importFrom dplyr filter %>%
#'
#' @importFrom readr read_delim
#'
#' @details This function reads in an earthquake dataset provided with the package, cleans and formats the data, then filters for a country or countries, then returns a tbl_df data.frame
#'
#'@return tbl_df data.frame
#'
#'
#' @examples
#' \dontrun{eq_get_data(country = c('MEXICO','USA'), start_date = '2001-01-01', end_date = '2003-01-01')}
#' \dontrun{eq_get_data(country = 'MEXICO', start_date = '2001-01-01')}
#'
#' @export

#
eq_get_data<- function(country, start_date, end_date = lubridate::today()){
  data<- system.file('data', 'earthquakes.tsv.gz', package = 'NOAA') # does this work?
  # data<- readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
   data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% country & DATE >= lubridate::ymd(start_date) & DATE<= lubridate::ymd(end_date))%>%
    eq_location_clean()
}

