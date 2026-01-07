#' Calculate tidal datums (MHW, MHHW, MLW, MLLW) from water level data
#'
#' This function calculates tidal datums from a water level dataset obtained via
#' the function `get_wl_data`. It groups the data to calculate datums for each
#' unique site. Internally, it relies on the function `find_extremes` to
#' identify daily high and low water levels.
#'
#' @param wl_data (required); A data frame containing water level data obtained
#'   via the function `get_wl_data`. It must include columns for 'water_level',
#'   'date', 'Identifier', 'Name', and 'LocationIdentifier'.
#'
#' @returns A data frame containing the calculated tidal datums for each site.
#'   Data frame columns are defined as:
#'   * Name: The site name e.g., Assateague Island NS - M5 -Pope Bay Marsh.
#'   * Identifier: The site name and time series name e.g., Depth.meters NAVD88 ASIS_M5_Waterlevel.
#'   * LocationIdentifier: The time series name e.g., ASIS_M5_Waterlevel.
#'   * park: Four-letter park code.
#'   * site_name: The SET site name e.g., Marsh 5 (Pope Bay).
#'   * MHW: Mean high water - Note the measurement units of the output will be the same as whatever was entered into Aquarius when the data was uploaded (either raw meters or meters referenced to NAVD88).
#'   * MHHW: Mean higher high water.
#'   * MLW: Mean low water.
#'   * MLLW: Mean lower low water.
#'   * first_date: The first date in the water level record.
#'   * last_date: The last date in the water level record.
#'   * record_count: The number of observations in the water level record.
#'
#' @import tidyr
#' @importFrom lubridate as_date
#'
#' @export
#'
#' @note Output units will be the same as whatever was entered into Aquarius
#'   when the data was uploaded (either raw meters or meters referenced to
#'   NAVD88).
#'
#' @seealso [get_wl_data()], [find_extremes()]
#'
#' @examples
#' asis_tidal_datums <- tidal_datums(asis_wl)
#' 
tidal_datums <- function(wl_data) {
  
  # Find high and low extremes
  df_extremes <- wl_data %>%
    group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
    tidyr::nest() %>%
    mutate(highs = map(data, ~find_extremes(.x$water_level, max)),
           lows = map(data, ~find_extremes(.x$water_level, min))) %>%
    tidyr::unnest(cols = c(data, highs, lows)) %>%
    mutate(date_chr = lubridate::as_date(date))
  
  # Calculate MHW and MHHW from the extremes
  highs <- df_extremes %>%
    filter(!is.na(highs)) %>%
    group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
    tidyr::nest() %>%
    mutate(MHW = map_dbl(data, ~mean(.x$highs, na.rm = TRUE)),
           MHHW = map_dbl(data, ~.x %>%
                            group_by(date_chr) %>%
                            summarise(max_high = max(highs, na.rm = TRUE), 
                                      .groups = "drop") %>%
                            summarise(mean_high = mean(max_high, na.rm = TRUE)) %>%
                            pull(mean_high)
           )) %>%
    select(-data)
  
  # Calculate MLW and MLLW from the extremes
  lows <- df_extremes %>%
    filter(!is.na(lows)) %>%
    group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
    tidyr::nest() %>%
    mutate(MLW = map_dbl(data, ~mean(.x$lows, na.rm = TRUE)),
           MLLW = map_dbl(data, ~.x %>%
                            group_by(date_chr) %>%
                            summarise(min_low = min(lows, na.rm = TRUE),
                                      .groups = "drop") %>%
                            summarise(mean_low = mean(min_low, na.rm = TRUE)) %>%
                            pull(mean_low)
           )) %>%
    select(-data)
    
  # get period of record
  wl_dates <- wl_data %>%
    filter(!is.na(water_level)) %>%
    group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
    summarise(first_date = min(date),
              last_date = max(date),
              record_count = n())
  
  suppressMessages(tidal_datums <- full_join(highs, lows) %>%
    left_join(., wl_dates))
  
  return(tidal_datums)
}