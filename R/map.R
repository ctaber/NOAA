#' Create Interactive Map of Earthquake Data
#'
#' This function plots earthquakes and avaialble information about said earthquakes on an interactive map that will allow the used to click on an earthquake to see the relevant details.
#'
#' @param df data to plot
#'
#' @param annot_col column to use for annotations, defaults to DATE
#'
#' @importFrom dplyr mutate %>%
#'
#' @importFrom leaflet addTiles addCircleMarkers
#'
#' @details Creates an interactive map showing the location of the earthquakes and thier date.
#'
#' @return Returns an interactive leaflet map
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#'     eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    eq_map()
#' }
#'
#' @export



eq_map <- function(df, annot_col= 'DATE'){
  # leaflet map
  m <- leaflet::leaflet(data = df) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(
      # per docs, use '~' For example, data can be a data frame containing columns latitude and longtitude, then we may add a circle layer to the map by leaflet(data) %>% addCircles(lat = ~latitude, lng = ~longtitude)
      lat = ~LATITUDE,
      lng = ~LONGITUDE,
      radius = ~EQ_PRIMARY,
      weight = .5,
      popup = ~as.character(get(annot_col))
    )

  m
}



#' Add Detailed Annotations to Leaflet Map
#'
#' This function add date, location, magnitude, and total deaths, in an annotation on an interactive map.
#'
#' @param df data to plot
#'
#' @importFrom dplyr mutate
#'
#' @details Adds annoations to an interactive map created with eq_map.
#'
#' @return Returns an interactive leaflet map with detailed annotations when present
#'
#' @examples
#' \dontrun{
#' readr::read_delim("earthquakes.tsv.gz", delim = "\t") %>%
#'     eq_clean_data() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'    eq_map(annot_col = "popup_text")
#' }
#'
#' @export


eq_create_label <- function(df){
  data <- df %>%
    #only include lables and label data if label data is not NA
    dplyr::mutate(popup_text = '',
           popup_text = ifelse(!is.na(DATE), paste('<b>','Date: ', '</b>', DATE), popup_text),
           popup_text = ifelse(!is.na(LOCATION_NAME), paste(popup_text, paste('<b>','Location: ','</b>', LOCATION_NAME),  sep= '<br/>'), popup_text),
           popup_text = ifelse(!is.na(EQ_PRIMARY), paste(popup_text, paste('<b>','Magnitude: ','</b>', EQ_PRIMARY), sep= '<br/>'), popup_text),
           popup_text = ifelse(!is.na(TOTAL_DEATHS), paste(popup_text, paste('<b>','Total deaths: ','</b>', TOTAL_DEATHS),  sep= '<br/>'), popup_text)
    )

  data$popup_text

}




