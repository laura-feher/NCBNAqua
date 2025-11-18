#' Calculate the daily mean minimum, maximum, or average daily flood depth
#'
#' This function calculates mean minimum, maximum, or average daily flood depth
#' from a flood depth dataset obtained via the function `flood_depths`. The
#' mean daily flood depth is an indicator of marsh drainage and is calculated as
#' the average difference between the daily minimum, maximum, or average water
#' elevation and the marsh elevation at each site.
#'
#' @param flood_depths_df (required); A data frame containing flood depth data
#'   obtained via the function `flood_depths`. It must include columns for
#'   'flood_depth', 'date', 'Identifier', 'Name', 'LocationIdentifier', and
#'   'elev_navd88' - a column of marsh surface elevation values in meters NAVD88.
#'   .
#' @param summary_metric character (required); The flood depth summary metric to
#'   calculate. One of:
#'   * "min": the daily mean minimum flood depth above the marsh surface.
#'   * "max": the daily mean maximum flood depth above the marsh surface.
#'   * "average": the daily mean average flood depth above the marsh surface.
#'
#' @returns A data frame with the daily mean minimum, maximum, or average flood
#'   depth and flood elevation above the marsh surface at each site. Data frame
#'   columns are defined as:
#'   * Name: The site name e.g., Assateague Island NS - M5 -Pope Bay Marsh.
#'   * Identifier: The site name and time series name e.g., Depth.meters NAVD88 ASIS_M5_Waterlevel.
#'   * LocationIdentifier: The time series name e.g., ASIS_M5_Waterlevel.
#'   * park: Four-letter park code.
#'   * site_name: The SET site name e.g., Marsh 5 (Pope Bay).
#'   * daily_mean_min/max/avg_flood_depth: The daily mean minimum, maximum, or average depth of flooding above the marsh surface in meters.
#'   * elev_navd88: The elevation of the marsh surface in meters NAVD88.
#'   * daily_mean_min/max/avg: The daily mean minimum, maximum, or average water level in meters NAVD88
#'
#' @export
#'
#' @seealso [flood_depths()]
#'
#' @examples
#' asis_daily_mean_min_flood <- daily_mean_flood(flood_depths_df = asis_flood_depths, summary_metric = "min")
#' 
#' asis_daily_mean_avg_flood <- daily_mean_flood(asis_flood_depths, summary_metric = "average")
#' 
daily_mean_flood <- function(flood_depths_df, summary_metric = "mean") {
  
  if (summary_metric == "min") {
    daily_flood_summary <- flood_depths_df %>%
      filter(flood_depth > 0) %>%
      group_by(date, .add = TRUE) %>%
      summarise(daily_min_flood_depth = min(flood_depth, na.rm = TRUE), # daily minimum depth of the water above the marsh in meters
                elev_navd88 = mean(elev_navd88, na.rm = TRUE)) %>%
      ungroup(date) %>%
      summarise(daily_mean_min_flood_depth = mean(daily_min_flood_depth, na.rm = TRUE), # mean of the daily minimum depth of the water above the marsh in meters (or whatever the unit is)
                elev_navd88 = mean(elev_navd88, na.rm = TRUE), 
                .groups = "keep") %>%
      mutate(daily_mean_min_flood_elev = daily_mean_min_flood_depth + elev_navd88) # mean of the daily minimum elevation of the water level in meters navd88 
  }
  
  else if (summary_metric == "average") {
    daily_flood_summary <- flood_depths_df %>%
      filter(flood_depth > 0) %>%
      group_by(date, .add = TRUE) %>%
      summarise(daily_avg_flood_depth = mean(flood_depth, na.rm = TRUE), # daily mean depth of the water above the marsh in meters
                elev_navd88 = mean(elev_navd88, na.rm = TRUE)) %>%
      ungroup(date) %>%
      summarise(daily_mean_avg_flood_depth = mean(daily_avg_flood_depth, na.rm = TRUE), # mean of the daily average depth of the water above the marsh in meters (or whatever the unit is)
                elev_navd88 = mean(elev_navd88, na.rm = TRUE),
                .groups = "keep") %>%
      mutate(daily_mean_avg_flood_elev = daily_mean_avg_flood_depth + elev_navd88) # mean of the daily average elevation of the water level in meters navd88 
  }
  
  else if (summary_metric == "max") {
    daily_flood_summary <- flood_depths_df %>%
      filter(flood_depth > 0) %>%
      group_by(date, .add = TRUE) %>%
      summarise(daily_max_flood_depth = max(flood_depth, na.rm = TRUE), # daily maximum depth of the water above the marsh in meters
                elev_navd88 = mean(elev_navd88, na.rm = TRUE)) %>%
      ungroup(date) %>%
      summarise(daily_mean_max_flood_depth = mean(daily_max_flood_depth, na.rm = TRUE), # mean of the daily maximum depth of the water above the marsh in meters (or whatever the unit is)
                elev_navd88 = mean(elev_navd88, na.rm = TRUE),
                .groups = "keep") %>%
      mutate(daily_mean_max_flood_elev = daily_mean_max_flood_depth + elev_navd88) # mean of the daily maximum elevation of the water level in meters navd88 
  }
  
  return(daily_flood_summary)
}