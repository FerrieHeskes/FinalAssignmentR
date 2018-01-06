#' eq_get_data
#'
#' This function reads a datafile delimited by tabs
#'
#' @param datafile A character string denoting the (compressed) file name
#'
#' @return This function returns the input data in a dataframe table
#'
#' @examples
#' \dontrun{
#' eq_get_data("data/signif.txt")
#' }
#'
#' @export
#'
eq_get_data <- function (datafile) {

  #Read the file delimited by tabs
  read.delim(datafile,sep="\t")

}

#' eq_clean_data
#'
#' This function cleans the NOAA earthquake data extracted from https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#' which was placed in a dataframe by function \code{eq_get_data}. The cleanup consists of:
#'
#' 1. Making LONGITUDE and LATITUDE numeric
#' 2. Make a new dataitem DATE using the dataitems YEAR, MONTH and DAY from the dataframe
#' 3. Cleanup the locationname via function \code{eq_clean_data} by deleting all text following a ':'
#'
#' @param datafile The dataframe constructed by function 'eq_get_data'
#'
#' @return This function returns the cleanedup NOAA earthquake data in a dataframe
#'
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' eq_clean_data(eq_get_data("data/signif.txt"))
#' }
#'
#' @export
#'
eq_clean_data <- function (datafile) {

  datafile$LATITUDE <- as.numeric(datafile$LATITUDE)
  datafile$LONGITUDE <- as.numeric(datafile$LONGITUDE)
  datafile$LOCATION_NAME <- eq_location_clean(as.character(datafile$LOCATION_NAME))

  #When year is before Christ (i.e. year lower than 0) the Date will no be set!
  #Set the date based on year
  datafile <- datafile %>%
              dplyr::mutate(DATE = as.Date(paste0(YEAR,"-",MONTH,"-",DAY),"%Y-%m-%d"))
}

#' eq_location_clean
#'
#' This function cleans the location form the NOAA earthquake data where the earthquake took place.
#' The cleanup of the location is done by ruling out all text folowed by a ':' data extracted
#' from https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1
#'
#' @param locationname The column LOCATION_NAME from the NOAA earthquake data
#'
#' @return This function returns the cleanedup LOCATION_NAME
#'
#' @importFrom stringr str_to_title str_trim
#'
#' @examples
#' \dontrun{
#' eq_clean_data(eq_get_data("data/signif.txt"))
#' }
#'
#' @export
#'
eq_location_clean <- function (locationname) {

  #Remove all text followed by a ':'
  stringr::str_to_title(stringr::str_trim(gsub("^.*:", "", locationname)))

}

