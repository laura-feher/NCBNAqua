#' Find local extremes (peaks or troughs) in a numeric vector within a rolling
#' window
#'
#' This function identifies local maximums or minimums in a vector `x` by
#' comparing each element to a rolling window of its neighbors. It uses
#' `zoo::rollapply` for the initial rolling calculation and ensures that the
#' identified extreme value is unique within its local window to avoid marking
#' entire flat sections as extremes.
#'
#' @param x (required); A numeric vector in which to find the extremes.
#' @param fun string (required); The function to apply within the rolling
#'   window, either `max` for peaks or `min` for troughs.
#' @param window string (required); The approximate width of the window to use
#'   for comparison. The actual number of elements in the window is calculated
#'   as `(window * 4)/2`, rounded down. Default window is 11.5 for a 24 hour
#'   day.
#'
#' @returns A numeric vector of the same length as `x` where extreme values are
#'   preserved and all other values are set to `NA_real_`.
#'
#' @importFrom zoo rollapply
#'
#' @export
#'
#' @examples
#' # Find high and low extremes
#' df_extremes <- asis_wl %>%
#'   group_by(Identifier, Name, LocationIdentifier, .add = TRUE) %>%
#'   nest() %>%
#'   mutate(highs = map(data, ~find_extremes(.x$water_level, max)),
#'          lows = map(data, ~find_extremes(.x$water_level, min))) %>%
#'   unnest(cols = c(data, highs, lows)) %>%
#'   mutate(date_chr = lubridate::as_date(date))
#'   
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
