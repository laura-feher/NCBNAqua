calc_flood_events <- function(flood_depths_df) {
  
  flood_depths_df %>%
    mutate(flood_counter = if_else(!is.na(wl_above_marsh) & is.na(lead(wl_above_marsh)), TRUE, FALSE)) %>%
    summarise(total_flood_events = sum(flood_counter),
              .groups = "keep")
}