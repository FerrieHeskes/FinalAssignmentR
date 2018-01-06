#' eq_map
#'
#' This function created a leaflet map with location of earthquakes with a popup with selected information of the earthquake
#'
#' @param data A dataframe containing cleanedup NOAA earthquake data
#' @param annot_col A character containing the name of the column in the data where the value within the column is used as text in the popup
#'
#' @return A leaflet map with earthquakes and corresponding text.
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_map(data, annot_col = "DATE")
#' }
#' #' @export
#'
eq_map <- function(data, annot_col) {

  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(radius = data$EQ_PRIMARY, weight = 1, lng = data$LONGITUDE, lat = data$LATITUDE, popup = data[[annot_col]])
}

#' eq_create_label
#'
#' This function creates a HTML tekst which is used as label for a leaflet. The label consists of location,
#' magnitude and deaths from the cleanedup NOAA earthquake data
#'
#' @param data A dataframe containing cleanedup NOAA earthquake data
#'
#' @return A character containing HTML text with (if not empty) location, magnitude and deaths of an earthquake
#'
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
#'
#' @export
#'
eq_create_label <- function(data) {

  location <- ifelse(!is.na(data$LOCATION_NAME),paste("<b>Location :</b>", data$LOCATION_NAME, "<br />"),"")
  deaths <- ifelse(!is.na(data$TOTAL_DEATHS),paste("<b>Total deaths :</b>", data$TOTAL_DEATHS, "<br />"),"")
  magnitude <- ifelse(!is.na(data$EQ_PRIMARY),paste("<b>Magnitude :</b>", data$EQ_PRIMARY, "<br />"),"")
  paste0(location,deaths,magnitude)

}




