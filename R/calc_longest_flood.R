calc_longest_flood <- function(flood_depths_df) {
  
  flood_depths_df %>%
    mutate(consecutive_group = consecutive_id(!is.na(wl_above_marsh)),
           is_flooded = if_else(!is.na(wl_above_marsh), TRUE, FALSE)) %>%
    select(datetime, date, time, consecutive_group, is_flooded) %>%
    group_by(consecutive_group, .add = TRUE) %>%
    nest(data = c(datetime, date, time, is_flooded)) %>%
    mutate(consecutive_count = map_int(data, ~sum(.x$is_flooded))) %>%
    ungroup(consecutive_group) %>%
    slice_max(consecutive_count) %>%
    mutate(min_date = map(data, ~first(.x$datetime)),
           max_date = map(data, ~last(.x$datetime))) %>%
    unnest(c(min_date, max_date)) %>%
    mutate(max_consecutive_flood_days = max_date - min_date)
}