#' Calculate the percent of time that the water level is above the marsh surface
#' over the course of the period of record.
#'
#' This function calculates 1) the total time that the marsh was flooded over
#' the period of record, and 2) the percentage of time that the marsh was
#' flooded over the period of record.
#'
#' @param flood_depths_df (required); A data frame containing flood depth data
#'   obtained via the function `flood_depths`. It must include columns for
#'   'flood_depth', 'date', 'Identifier', 'Name', 'LocationIdentifier', and
#'   'wl_above_marsh' - a column of the water level above the marsh at each time
#'   step.
#'
#' @returns A data frame with 1 row per site containing the total time recorded,
#'   the total time that the marsh was flooded over the period of record, and
#'   the percentage of time that the marsh was flooded over the period of
#'   record. Data frame columns are defined as:
#'   * Name: The site name e.g., Assateague Island NS - M5 -Pope Bay Marsh.
#'   * Identifier: The site name and time series name e.g., Depth.meters NAVD88 ASIS_M5_Waterlevel.
#'   * LocationIdentifier: The time series name e.g., ASIS_M5_Waterlevel.
#'   * park: Four-letter park code.
#'   * site_name: The SET site name e.g., Marsh 5 (Pope Bay).
#'   * total_time_recorded: The total time recorded.
#'   * flooding_time: The total time that the marsh was flooded over the period of record in hours.
#'   * percent_time_flooded: The percentage time that the marsh was flooded over the period of record.
#'
#' @export
#'
#' @examples
#' asis_time_flooded <- time_flooded(asis_flood_depths)
#' 
time_flooded <- function(flood_depths_df) {
  
  # Data from SFCN sites is in 1-hour intervals (interval_mins = 60).
  
  if(unique(flood_depths_df$park) %in% c("ACAD", "ASIS", "CACO", "COLO", "FIIS", "GATE", "GWMP", "NACE")) {
    interval_mins <- 15 # Data from NCBN & NCRN sites is in 15-minute intervals (interval_mins = 15)
  } else if (unique(flood_depths_df$park %in% c("BISC", "SARI", "VIIS"))) {
    interval_mins <- 60 # Data from SFCN sites is in 1-hour intervals (interval_mins = 60).
  }
  
  flood_depths_df %>%
    summarise(total_time_recorded = sum(!is.na(water_level))*(interval_mins/60), # total time recorded in hours
              flooding_time = sum(!is.na(wl_above_marsh))*(interval_mins/60), # total time with water above the marsh in hours
              .groups = "keep") %>%
    mutate(percent_time_flooded = (flooding_time/total_time_recorded)*100)
}