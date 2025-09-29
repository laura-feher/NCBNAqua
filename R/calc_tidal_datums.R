calc_tidal_datums <- function(wl_data) {
  
  # Find high and low extremes
  df_extremes <- wl_data %>%
    group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
    nest() %>%
    mutate(highs = map(data, ~find_extremes(.x$water_level, max)),
           lows = map(data, ~find_extremes(.x$water_level, min))) %>%
    unnest(cols = c(data, highs, lows)) %>%
    mutate(date_chr = lubridate::as_date(date))
  
  # Calculate MHW and MHHW from the extremes
  highs <- df_extremes %>%
    filter(!is.na(highs)) %>%
    group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
    nest() %>%
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
    nest() %>%
    mutate(MLW = map_dbl(data, ~mean(.x$lows, na.rm = TRUE)),
           MLLW = map_dbl(data, ~.x %>%
                            group_by(date_chr) %>%
                            summarise(min_low = min(lows, na.rm = TRUE),
                                      .groups = "drop") %>%
                            summarise(mean_low = mean(min_low, na.rm = TRUE)) %>%
                            pull(mean_low)
           )) %>%
    select(-data)
  
  tidal_datums <- full_join(highs, lows)
  
  return(tidal_datums)
}