calc_time_flooded <- function(flood_depths_df) {
  
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