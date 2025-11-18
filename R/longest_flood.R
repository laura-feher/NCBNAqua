#' Calculate the longest time that the marsh stayed flooded.
#'
#' This function finds the longest consecutive time that the water level
#' remained above the marsh surface.
#'
#' @inheritParams time_flooded
#'
#' @returns A data frame containing the longest flood event at each site. Data
#'   frame columns are defined as:
#'   * Name: The site name e.g., Assateague Island NS - M5 -Pope Bay Marsh.
#'   * Identifier: The site name and time series name e.g., Depth.meters NAVD88 ASIS_M5_Waterlevel.
#'   * LocationIdentifier: The time series name e.g., ASIS_M5_Waterlevel.
#'   * park: Four-letter park code.
#'   * site_name: The SET site name e.g., Marsh 5 (Pope Bay).
#'   * consecutive_group: The flood event group ID for the longest flood event.
#'   * data: A nested data of all flood events at each site.
#'   * consecutive_count: The total number of time steps recorded during the longest flood event.
#'   * min_date_longest_flood: The date that the longest consecutive flood event started.
#'   * max_date_longest_flood: The date that the longest consecutive flood event ended.
#'   * max_consecutive_flood_days: The number of days that the marsh was flooded during the longest consecutive flood event.
#'
#' @export
#'
#' @examples
#' asis_longest_flood <- longest_flood(asis_flood_depths)
#' 
longest_flood <- function(flood_depths_df) {
  
  flood_depths_df %>%
    mutate(consecutive_group = consecutive_id(!is.na(wl_above_marsh)),
           is_flooded = if_else(!is.na(wl_above_marsh), TRUE, FALSE)) %>%
    select(datetime, date, time, consecutive_group, is_flooded) %>%
    group_by(consecutive_group, .add = TRUE) %>%
    nest(data = c(datetime, date, time, is_flooded)) %>%
    mutate(consecutive_count = map_int(data, ~sum(.x$is_flooded))) %>%
    ungroup(consecutive_group) %>%
    slice_max(consecutive_count) %>%
    mutate(min_date_longest_flood = map(data, ~first(.x$datetime)),
           max_date_longest_flood = map(data, ~last(.x$datetime))) %>%
    unnest(c(min_date_longest_flood, max_date_longest_flood)) %>%
    mutate(max_consecutive_flood_days = max_date_longest_flood - min_date_longest_flood)
}