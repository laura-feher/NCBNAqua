#' Calculate tidal datums from a user-supplied data frame or saved file of raw water level data
#'
#' @param data data frame. A data frame of raw water level data.
#' @param Location string (optional). The name of the location to be used in the output data frame of tidal datums.
#' @param record string (optional). The name of the data record to be used in the output data frame of tidal datums. Defaults to "water level".
#' @inheritParams calc_tidal_datums1
#' @inheritParams get_timeseries
#' 
#' @note Adapted from \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#' @returns A dataframe with tidal datums or list of data frames with all raw data, intermediate data and tidal data.
#' @export
#'
#' @examples
#' calc_tidal_datums2(data = )
calc_tidal_datums2 <- function(data, Location = NULL, record = "water level", window = 11.5,  print_results = TRUE, return_all = FALSE) {
  
  # Specify window size
  # window size in hours with 4 readings per hour
  # defaults to 11.5 hour window
  window_size <- window * 4
  half_window <- floor(window_size / 2)
  
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
    results <- list("raw_record" = raw_record, "simplified_record" =  df, "all_extremes" =  df_extremes, "highs" =  highs, "lows" = lows)
  } else {
    results <- tidal_datums
  }
  
  return(results)
}