#' Get raw water level data from Aquarius and calculate tidal datums
#'
#' @param Location string. The name of the location in the Aquarius database.
#' @param window number. Size of the time window in hours used to calculate MHW and MLW. Defaults to 11.5 hours.
#' @inheritParams get_timeseries
#' @param print_results TRUE/FALSE. Do you want to print the calculated tidal datums to the console? Defaults to TRUE.
#' @param return_all TRUE/FALSE. Do you want to retun a list of dataframes including all raw data, intermediate data, and tidal datums? Defaults to FALSE.
#'
#' @note Adapted from \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#' @returns A dataframe with tidal datums or list of data frames with all raw data, intermediate data and tidal data.
#' @export
#'
#' @examples 
#' calc_tidal_datums(Location = "COLO_M30_WaterLevel", start_date = "2024-01-01", end_date = "2024-12-31")
#' 
calc_tidal_datums1 <- function(Location, window = 11.5, start_date = NULL, end_date = NULL, print_results = TRUE, return_all = FALSE) {
  
  # Specify window size
  # window size in hours with 4 readings per hour
  # defaults to 11.5 hour window
  window_size <- window * 4
  half_window <- floor(window_size / 2)
  
  if(Location == "ACAD_MCHT_WaterLevel") {
    record_name <- "Depth.Sensor Depth Meters@"
  } else if (Location == "ACAD_SCH_WaterLevel") {
    record_name <- "Depth.ACAD_Schoodic_WaterLevel@"
  } else if (Location == "ACAD_TI_WaterLevel") {
    record_name <- "Depth.ACAD_TI_WaterLevel"
  } else if (Location == "ACAD_BH_WaterLevel") {
    record_name <- "DepthFromWaterPressure.Sensor Depth@"
  } else if (Location %in% c("CACO_BlackfishCreek_WaterLevel", "CACO_HH_Restricted_WaterLevel", "CACO_HH_Unrestricted_WaterLevel")) {
    record_name <- "DepthFromWaterPressure.Water Depth@"
  } else if (Location == "CACO_DH_WaterLevel") {
    record_name <- "WaterLevelNAVD88.18@"
  } else if (Location %in% c("CACO_NausetNorth_WaterLevel", "CACO_NausetSouth_WaterLevel")) {
    record_name <- "Depth.Sensor Depth@"
  } else if (Location == "COLO_M19_WaterLevel") {
    record_name <- "Depth.NAVD88meters@"
  } else if (Location %in% c("COLO_M30_WaterLevel", "GATE_SAHU_WaterLevel")) {
    record_name <- "Depth.Meters NAVD88@"
  } else {
    record_name <- "Depth.meters NAVD88@"
  }
  
  # Load raw time series data from Aquarius
  if (!is.null(start_date) & !is.null(end_date)) {
    if(is.Date(as.Date(start_date)) & is.Date(as.Date(end_date))) {
      raw_record <- Get_timeseries1(record = paste0(record_name, Location), start_date = start_date, end_date = end_date)
    } else {
      warning(" Returning the full record. Both start and end dates must be supplied and must be valid dates formatted as `YYYY-MM-DD` in order to query for a specific time frame.")
      raw_record <- Get_timeseries2(record = paste0(record_name, Location))
    }
  } else if ((is.null(start_date) & !is.null(end_date)) | (!is.null(start_date) & is.null(end_date))) {
    warning(" Returning the full record. Both start and end dates must be supplied and must be valid dates formatted as `YYYY-MM-DD` in order to query for a specific time frame.")
    raw_record <- Get_timeseries2(record = paste0(record_name, Location))
  } else {
    raw_record <- Get_timeseries2(record = paste0(record_name, Location))
  }
  
  # Transform raw data into a simplified format
  df <- TS_simplify(data = raw_record) %>%
    select("datetime" = date_time, "water_level" = value) %>% # select and rename the date and water level columns
    filter(!is.na(water_level)) %>%
    mutate(datetime = lubridate::ymd_hms(datetime, quiet = TRUE))
  
  # Define helper function to find local highs or lows
  find_extremes <- function(x, fun) {
    roll_extreme <- zoo::rollapply(x, width = window_size, FUN = fun, fill = NA, align = "center")
    is_extreme <- x == roll_extreme
    # Remove flat peaks/troughs by ensuring unique max/min in the window
    is_extreme <- purrr::map_lgl(seq_along(x), function(i) {
      if (is.na(is_extreme[i]) || !is_extreme[i]) return(FALSE)
      window_start <- max(1, i - half_window)
      window_end <- min(length(x), i + half_window)
      window_vals <- x[window_start:window_end]
      center_index <- i - window_start + 1
      # Check if the extreme value is unique in the window
      sum(window_vals == x[i]) == 1
    })
    ifelse(is_extreme, x, NA_real_)
  }
  
  # Find high and low extremes
  df_extremes <- df %>%
    mutate(
      highs = find_extremes(water_level, max),
      lows = find_extremes(water_level, min),
      date = lubridate::as_date(datetime)
    )
  
  # Calculate MHW and MHHW from the extremes
  highs <- df_extremes %>%
    filter(!is.na(highs)) %>%
    nest(data = everything()) %>%
    mutate(MHW = purrr::map(data, ~mean(.x$highs, na.rm = TRUE)),
           MHHW = purrr::map(data, ~.x %>%
                               group_by(date) %>%
                               summarise(max_high = max(highs, na.rm = TRUE), .groups = "drop") %>%
                               summarise(mean_high = mean(max_high, na.rm = TRUE)) %>%
                               pull(mean_high)))
  
  # Calculate MLW and MLLW from the extremes
  lows <- df_extremes %>%
    filter(!is.na(lows)) %>%
    nest(data = everything()) %>%
    mutate(MLW = purrr::map(data, ~mean(.x$lows, na.rm = TRUE)),
           MLLW = purrr::map(data, ~.x %>%
                               group_by(date) %>%
                               summarise(min_low = min(lows, na.rm = TRUE), .groups = "drop") %>%
                               summarise(mean_low = mean(min_low, na.rm = TRUE)) %>%
                               pull(mean_low)))
  
  tidal_datums <- data.frame(
    "Location" = Location,
    "record" = record_name,
    "MHW" = highs$MHW[[1]],
    "MLW" = lows$MLW[[1]],
    "MHHW" = highs$MHHW[[1]],
    "MLLW" = lows$MLLW[[1]]
  )
  
  # Do you want to print the results to the console?
  if (print_results) {
    cat(sprintf("Mean High Water (MHW):         %.3f\n", highs$MHW))
    cat(sprintf("Mean Higher High Water (MHHW): %.3f\n", highs$MHHW))
    cat(sprintf("Mean Low Water (MLW):          %.3f\n", lows$MLW))
    cat(sprintf("Mean Lower Low Water (MLLW):   %.3f\n", lows$MLLW))
  }
  
  # Return all raw data, intermediate data, and tidal datums?
  if (return_all) {
    results <- list("raw_record" = raw_record$Points, "simplified_record" =  df, "all_extremes" =  df_extremes, "highs" =  highs, "lows" = lows)
  } else {
    results <- tidal_datums
  }
  
  return(results)
}
