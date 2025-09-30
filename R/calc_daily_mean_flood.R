calc_daily_mean_flood <- function(flood_depths_df, summary_metric = "mean") {
  
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
  
  else if (summary_metric == "mean") {
    daily_flood_summary <- flood_depths_df %>%
      filter(flood_depth > 0) %>%
      group_by(date, .add = TRUE) %>%
      summarise(daily_avg_flood_depth = mean(flood_depth, na.rm = TRUE), # daily mean depth of the water above the marsh in meters
                elev_navd88 = mean(elev_navd88, na.rm = TRUE)) %>%
      ungroup(date) %>%
      summarise(daily_mean_flood_depth = mean(daily_avg_flood_depth, na.rm = TRUE), # mean of the daily mean depth of the water above the marsh in meters (or whatever the unit is)
                elev_navd88 = mean(elev_navd88, na.rm = TRUE),
                .groups = "keep") %>%
      mutate(daily_mean_flood_elev = daily_mean_flood_depth + elev_navd88) # mean of the daily mean elevation of the water level in meters navd88 
  }
  
  return(daily_flood_summary)
}