#' Clean and Format NOAA Data
#'
#' This function takes a dataframe and drops date related NAs, converts data types to appropriate type,
#' removes pre-BC dates due to low information, fixes date parts that do not fit conventionall format,
#' and returns a cleaned dataframe.
#'
#' @param df The raw dataframe with at least Year, Month, Day, Country, Location_Name, Latitude, Longitiude, EQ_Primary, and TOtal Death fields
#'
#' @importFrom dplyr mutate filter %>%
#'
#' @importFrom tidyr drop_na unite
#'
#' @importFrom lubridate ymd
#'
#' @importFrom stringr str_pad
#'
#' @details Takes a dataframe of earthquake information and drops date realted NAs, changes the date parts to integers, filters for post-BC dates, fixes dates components to YYYY-MM-DD format, combines date parts into a DATE field, and changes remaing fields to proper type.
#'
#' @return Returns a tbl_df date.frame
#'
#' @examples
#' \dontrun{eq_clean_data(df= data)}
#'
#' @export


eq_clean_data <- function(df){
  clean_data <- df %>%
    # drop date related NAs
   tidyr::drop_na(YEAR, MONTH, DAY) %>%
    # convert data types as appropriate
   dplyr::mutate(
     YEAR = as.integer(YEAR),
     MONTH = as.integer(MONTH),
     DAY = as.integer(DAY)
     )%>%
    # remove pre-BC dates
   dplyr::filter(YEAR>0)%>%
    # fix date parts that are not correct fomrat for YYYY-MM-DD
   dplyr::mutate(
      YEAR = stringr::str_pad(YEAR, 4, pad = "0"),
      MONTH = stringr::str_pad(MONTH, 2, pad = "0"),
      DAY = stringr::str_pad(DAY, 2, pad = "0")
      )%>%
    # combine YEAR, MONTH, DAY, and convert to date
  tidyr::unite(col='DATE', YEAR, MONTH, DAY, sep = '-', remove = FALSE) %>%
  dplyr::mutate(DATE=lubridate::ymd(DATE)) %>%
  tidyr::drop_na(LATITUDE, LONGITUDE) %>%
  dplyr::mutate(
    LATITUDE = as.numeric(LATITUDE),
    LONGITUDE = as.numeric(LONGITUDE)
    ) %>%
  dplyr::mutate(EQ_PRIMARY = as.numeric(EQ_PRIMARY),
                 TOTAL_DEATHS = as.numeric(TOTAL_DEATHS))
  return(clean_data)
}


#' Clean and Format Location Data
#'
#' This funtion cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @param df The erthquake data with raw location names
#'
#' @importFrom dplyr mutate
#'
#' @importFrom stringr str_extract str_length str_to_title
#'
#' @details This function takes the raw LOCATION_NAME and extracts everything after the ':', then removes the colon, replaces any LOCATION_NAMEs less than 5 characters with the COUNTRY name, and then converts to title case.
#'
#' @return Returns a tbl_df data.frame
#'
#' @examples
#' \dontrun{eq_location_clean(df=data)}
#'
#' @export


eq_location_clean <- function(df){
  clean_df <- df %>%
    dplyr::mutate(
      # strip out the country name (including the colon)
      LOCATION_NAME = stringr::str_extract(LOCATION_NAME, pattern=':[ ]+[A-Z]+'),
      # remove colon and leading white space
      LOCATION_NAME = stringr::str_extract(LOCATION_NAME, pattern='[A-Z]+'),
      # handle NAs
      #LOCATION_NAME = ifelse(is.na(LOCATION_NAME), 'unknown', LOCATION_NAME),
      # fix errors of non-convential names
      LOCATION_NAME = ifelse(stringr::str_length(LOCATION_NAME)<5, COUNTRY, LOCATION_NAME),
      # converts names to title case (as opposed to all caps)
      LOCATION_NAME = stringr::str_to_title(LOCATION_NAME)

    )
  return(clean_df)
}

