#' Simplify time series water level data 
#'
#' @description This function simplifies time series water level data into 2
#' columns with properly formatted date-time and water level columns.
#'
#' @param data can be either
#' \itemize{
#'  \item A list of data frames produced by either or \code{\link{Get_timeseries1}} or \code{\link{Get_timeseries2}}. The list must contain a data frame with the name "Points"
#'  that contains the raw time series data.
#'  \item A single data frame of raw time series water level data. This data
#'  frame can be produced by either 1) loading a saved file with the function
#'  or \code{\link{load_raw_wl}} or 2) extracting the "Points" data frame from the output of
#'  either of the `Get_timeseries` functions.
#' }
#' @param time_col string (optional). The name of the column containing the
#'   date-times. Defaults to "Timestamp" based on the output of the
#'   `Get_timeseries` functions. Required if `data` has date-times in a column
#'   other than "Timestamp".
#' @param value_col string (optional). The name of the column containing the
#'   water level values. Defaults to "NumericValue1" based on the output of the
#'   `Get_timeseries` functions. Required if `data` has water level values in a
#'   column other than "NumericValue1".
#'
#' @note Adapted from
#'   \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#'
#' @returns A data frame with two columns: `date_time` which contains date-times
#'   transformed into a usable format and `value` which contains the water level
#'   value.
#' @export
#'
#' @examples
#' asis_2024_aq_data1 <- readRDS(here::here("data", "asis_2024_aq_data.Rds"))
#' asis_2024_aq_data_simple1 <- TS_simplify(asis_2024_aq_data1)
#' 
#' asis_2024_aq_data2 <- asis_2024_aq_data1 %>%
#'   purrr::pluck("Points")
#' asis_2024_aq_data_simple2 <- TS_simplify(asis_2024_aq_data2)   
#' 
#' asis_2024_aq_points1 <- readRDS(here::here("data", "asis_2024_aq_points.Rds")) 
#' asis_2024_aq_points_simple1 <- TS_simplify(asis_2024_aq_points1, time_col = "time", value_col = "wl")
#' 
#' asis_2024_aq_points2 <- load_raw_wl(file_path = here::here("data", "asis_2024_aq_points.csv"), time_col = "time", value_col = "wl")
#' asis_2024_aq_points_simple2 <- TS_simplify(asis_2024_aq_points2)
#' 
TS_simplify <- function(data, time_col = "Timestamp", value_col = "NumericValue1"){
  
  # Test for the presence of the Points data frame in the list produced by Get_timeseries()
  if(!is.data.frame(data) & "Points" %in% names(data)) {
    # takes a time series of a given water quality parameter and reforms the date and time into a usable format
    # can be used on an existing df of raw water level data produced by Get_timeseries
    output <- data$Points %>%
      as.data.frame() %>%
      dplyr::select(c(1,2)) %>%
      mutate(rawdate = Timestamp) %>%
      separate(., col = rawdate, into = c("left", "right"), sep = "T") %>%
      mutate(value = NumericValue1,
             date = left,
             int = right) %>%
      dplyr::select(value, date, int) %>%
      separate(., col = int, into = c("time", "junk"), sep = "\\.") %>%
      mutate(date_time = format(as.POSIXct(paste(date, time, sep = " ")), format = "%Y-%m-%d %H:%M:%S")) %>%
      dplyr::select(date_time, value)
    
    return(output)
    
  } else {
    # formats a data frame of raw water level for calculating tidal datums
    # can be use after load_raw_wl to format data from a saved file or,
    # can be used on an existing df of raw water level data 
    output <- data %>%
      dplyr::select(all_of(c(!!time_col, !!value_col))) %>%
      rename("Timestamp" = time_col, "NumericValue1" = value_col) %>%
      mutate(rawdate = Timestamp) %>%
      separate_wider_delim(., cols = rawdate, names = c("left", "right"), delim = "T") %>%
      mutate(value = NumericValue1,
             date = left,
             time = right) %>%
      dplyr::select(value, date, time) %>%
      mutate(date_time = as.POSIXct(paste(date, time, sep = " "), format = "%Y-%m-%d %H:%M:%S")) %>%
      dplyr::select(date_time, value)
    
    return(output)
  }
}
