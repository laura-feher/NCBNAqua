#' Calculate the number of times that the marsh was flooded.
#'
#' This function calculates the events where the water level switched from below the marsh to above the marsh surface on successive timesteps.
#' 
#' @inheritParams time_flooded
#'
#' @returns A data frame with 1 row per site containing the total number of events where the water level over topped the marsh surface over the period of
#'   record. Data frame columns are defined as:
#'   * Name: The site name e.g., Assateague Island NS - M5 -Pope Bay Marsh.
#'   * Identifier: The site name and time series name e.g., Depth.meters NAVD88 ASIS_M5_Waterlevel.
#'   * LocationIdentifier: The time series name e.g., ASIS_M5_Waterlevel.
#'   * park: Four-letter park code.
#'   * site_name: The SET site name e.g., Marsh 5 (Pope Bay).
#'   * total_flood_events: The number of events where the water level over topped the marsh surface.
#'    
#' @export
#'
#' @examples
#' asis_flood_events <- flood_events(flood_depths_df = asis_flood_depths)
#' 
flood_events <- function(flood_depths_df) {
  
  flood_depths_df %>%
    mutate(flood_counter = if_else(!is.na(wl_above_marsh) & is.na(lead(wl_above_marsh)), TRUE, FALSE)) %>%
    summarise(total_flood_events = sum(flood_counter),
              .groups = "keep")
}