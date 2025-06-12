#' Load raw water level data for a single site from a csv, xlsx, or xls file
#'
#' @param file_path string. Either the full path to the saved file or a relative path specified with \code{\link[here]{here}}.
#' @param time_col string. The name of the column with date-times. Defaults to "Timestamp" based on the output of \code{\link{Get_timeseries1}} or \code{\link{Get_timeseries2}}. Required if `data` has date-times in a column other than "Timestamp". 
#' @param value_col string. The name of the column containing the water level values. Defaults to "NumericValue1" based on the output of \code{\link{Get_timeseries1}} or \code{\link{Get_timeseries2}}. Required if `data` has water level values in a column other than "NumericValue1".
#'
#' @returns a data frame with time series water level data.
#' 
#' @export
#'
#' @examples
#' data <- load_raw_wl(here::here("data", "ncbn_aq_points.csv"), time_col = "time", value_col = "wl")
#' 
load_raw_wl <- function(file_path = NULL, time_col = "Timestamp", value_col = "NumericValue1") {
  
  # Throw error if a file isn't specified
  if (is.null(file_path)) {
    stop("Must supply the path to a saved file of raw water level data")
  } 
  
  else if (!is.null(file_path)) {
    if (stringr::str_detect(file_path, ".csv")) {
      data <- readr::read_csv(file_path)
    } else if (stringr::str_detect(file_path, ".xslsx") | stringr::str_detect(file_path, ".xls")) {
      data <- readxl::read_excel(file_path)
    }
    
    if(!(time_col %in% colnames(data))) {
      # Throw error if the time_col isn't found in the data
      stop(paste0("Column '", time_col, "' was not found in ", file_path))
    } else if (!(value_col %in% colnames(data))) {
      # Throw error if the value_col isn't found in the data
      stop(paste0("Column '", value_col, "' was not found in ", file_path))
    }
    
    raw_record <- data %>%
      rename("Timestamp" = !!time_col, 
             "NumericValue1" = !!value_col) %>%
      mutate(Timestamp = format(as.POSIXct(Timestamp, tz = "EST5EDT"), format = "%Y-%m-%dT%H:%M:%S.0000000-05:00"))
  }
  
  return(raw_record)
}
