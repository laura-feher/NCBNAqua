#' Calculate flooding depth above the marsh surface
#'
#' This function calculates the water depth above the marsh surface at each time
#' step for a water level dataset obtained via the function `get_wl_data` and a
#' user supplied data frame of marsh surface elevations for each site referenced
#' to meters NAVD88.
#'
#' @param wl_data (required); A data frame containing water level data obtained
#'   via the function `get_wl_data`. It must include columns for 'water_level',
#'   'date', 'Identifier', 'Name', and 'LocationIdentifier'.
#' @param elev_df (required); A data frame containing the marsh surface
#'   elevation values in meters NAVD88 for each site. Must also contain either
#'   an 'Identifier', 'Name', or 'LocationIdentifier' column that matches the
#'   sites of interest in the `wl_data` data frame.
#'
#' @returns A data frame showing the water level above the marsh surface at each
#'   time step. Data frame columns are defined as:
#'   * Name: The site name e.g., Assateague Island NS - M5 -Pope Bay Marsh.
#'   * Identifier: The site name and time series name e.g., Depth.meters NAVD88 ASIS_M5_Waterlevel.
#'   * LocationIdentifier: The time series name e.g., ASIS_M5_Waterlevel.
#'   * park: Four-letter park code.
#'   * Unit: The water level measurement unit e.g. m or meters. Note the measurement units of the output will be the same as whatever was entered into Aquarius when the data was uploaded (either raw meters or meters referenced to NAVD88).
#'   * Label: The water level unit label e.g., meters NAVD88.
#'   * datetime: The Posix date/time of the water level measurement (YYYY-MM-DD H:M:S)
#'   * date: ISO date of the water level  measurement (YYYY-MM-DD).
#'   * time: The measurement time in military time format (H:M:S).
#'   * water_level: The water level depth in meters; this is either referenced to NAVD88 or, if not referenced to NAVD88, represents a water level depth relative to the vertical position of the water logger.
#'   * is_navd88: True/False - is the water level depth referenced to NAVD88?
#'   * site_name: The SET site name e.g., Marsh 5 (Pope Bay).
#'   * elev_navd88: The elevation of the marsh surface in meters NAVD88.
#'   * flood_depth: The depth of the water relative to the marsh surface in meters. Negative values indicate the water level was below the marsh surface and positive values indicate the water level was above the marsh surface.
#'   * wl_above_marsh: The water level in meters NAVD88 - Note that time steps where the water level was below the marsh surface are coded as NA's.
#'
#' @export
#'
#' @note This comparison will only work properly if both the water level data
#'   and marsh elevation data are referenced to the same datum e.g. meters
#'   NAVD88.
#'
#' @seealso [get_wl_data()]
#'
#' @examples
#' # data frame of marsh surface elevations in meters NAVD88
#' asis_elev_df <- data.frame(LocationIdentifier = c("ASIS_M5_WaterLevel", "ASIS_M6_WaterLevel", "ASIS_M8_WaterLevel"), elev_navd88 = c(0.145, 0.1325, 0.200))
#' 
#' asis_flood_depths <- flood_depths(wl_data = asis_wl, elev_df = asis_elev_df)
#' 
flood_depths <- function(wl_data, elev_df) {
  suppressMessages(wl_data %>%
    left_join(., elev_df) %>%
    filter(!is.na(elev_navd88)) %>%
    mutate(water_level = round(water_level, 4), # round to 4 decimal places to match the precision/scale of the elevation values
           flood_depth = water_level - elev_navd88, # depth of water relative to the marsh
           wl_above_marsh = if_else(water_level > elev_navd88, water_level - elev_navd88, NA_real_))) # depth of water only when its above the marsh 
  
}