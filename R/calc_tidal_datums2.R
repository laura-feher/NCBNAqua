#' Calculate tidal datums from a user-supplied data frame or saved file of raw water level data
#'
#' @param data data frame. A data frame of raw water level data.
#' @param record string (optional). The name of the data record to be used in the output data frame of tidal datums. Defaults to "water level".
#' @param window number. Size of the time window in hours used to calculate MHW and MLW. Defaults to 11.5 hours.
#' @param print_results TRUE/FALSE. Do you want to print the calculated tidal datums to the console? Defaults to TRUE.
#' @param return_all TRUE/FALSE. Do you want to retun a list of dataframes including all raw data, intermediate data, and tidal datums? Defaults to FALSE.
#' 
#' @import dplyr
#' @import purrr
#' @import zoo
#' @import lubridate
#' 
#' @note Adapted from \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#' @returns A dataframe with tidal datums or list of data frames with all raw data, intermediate data and tidal data.
#' @export
#'
#' @examples
#' calc_tidal_datums2(data = )
calc_tidal_datums2 <- function(data, record = "water level", window = 11.5,  print_results = TRUE, return_all = FALSE) {
  
  # Specify window size
  # window size in hours with 4 readings per hour
  # defaults to 11.5 hour window
  window_size <- window * 4
  half_window <- floor(window_size / 2)
  
  Location <- data$TimeSeries$LocationIdentifier
  record_name <- paste0(record, "@", Location)
  
  # Transform raw data into a simplified format
  df <- TS_simplify(data) %>%
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
  ) %>%
    mutate(MTL = MLW + (MHW-MLW)/2,
           MSL = mean(df$water_level, na.rm = TRUE),
           tidal_range = MHW - MLW)
  
  # Do you want to print the results to the console?
  if (print_results) {
    cat(paste0("Mean High Water (MHW): ", format(round(tidal_datums$MHW, 3), nsmall = 3), " meters NAVD88\n",
           "Mean Higher High Water (MHHW): ", format(round(tidal_datums$MHHW, 3), nsmall = 3), " meters NAVD88\n",
           "Mean Low Water (MLW): ", format(round(tidal_datums$MLW, 3), nsmall = 3), " meters NAVD88\n",
           "Mean Lower Low Water (MLLW): ", format(round(tidal_datums$MLLW, 3), nsmall = 3), " meters NAVD88\n",
           "Mean Tide Level (MTL): ", format(round(tidal_datums$MTL, 3), nsmall = 3), " meters\n",
           "Mean Sea Level (MSL): ", format(round(tidal_datums$MSL, 3), nsmall = 3), " meters NAVD88\n",
           "Tidal Range: ", format(round(tidal_datums$tidal_range, 3), nsmall = 3), " meters"))
  }
  
  # Return all raw data, intermediate data, and tidal datums?
  if (return_all) {
    results <- list("raw_record" = raw_record, "simplified_record" =  df, "all_extremes" =  df_extremes, "highs" =  highs, "lows" = lows)
  } else {
    results <- tidal_datums
  }
  
  return(results)
}
