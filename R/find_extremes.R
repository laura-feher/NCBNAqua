find_extremes <- function(x, fun, window = 11.5) {
  
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
