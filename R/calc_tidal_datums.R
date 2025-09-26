calc_tidal_datums <- function(wl_data, window = 11.5) {
  find_extremes <- function(x, fun, window = window) {
    window_size <- window*4
    half_window <- floor(window_size / 2)
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
  df_extremes <- wl_data %>%
    group_by(Identifier, Name, LocationIdentifier) %>%
    nest(data = -c(Identifier, Name, LocationIdentifier)) %>%
    mutate(highs = map(data, ~find_extremes(.$water_level, max)),
           lows = map(data, ~find_extremes(.$water_level, min))) %>%
    unnest(cols = c(data, highs, lows)) %>%
    mutate(date_chr = lubridate::as_date(date))
  
  # Calculate MHW and MHHW from the extremes
  highs <- df_extremes %>%
    filter(!is.na(highs)) %>%
    group_by(Identifier, Name, LocationIdentifier) %>%
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
    group_by(Identifier, Name, LocationIdentifier) %>%
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
  
  tidal_datums <- full_join(highs, lows, by = join_by(Name, Identifier, LocationIdentifier))
  
  return(tidal_datums)
}