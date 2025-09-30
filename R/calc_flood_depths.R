calc_flood_depths <- function(wl_data, elev_df) {

  wl_data %>%
    left_join(., elev_df) %>%
    filter(!is.na(elev_navd88)) %>%
    mutate(water_level = round(water_level, 4), # round to 4 decimal places to match the precision/scale of the elevation values
           flood_depth = water_level - elev_navd88, # depth of water relative to the marsh
           wl_above_marsh = if_else(water_level > elev_navd88, water_level - elev_navd88, NA_real_)) # depth of water only when its above the marsh 
  
}