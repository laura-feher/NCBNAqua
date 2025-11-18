#' Calculate the recent local rate of SLR from a user-supplied data frame or saved file of raw water level data
#'
#' @param data data frame. A data frame of raw water level data.
#' @param record string (optional). The name of the data record to be used in the output data frame of recent local slr. Defaults to "water level".
#' @param marsh_elev number. The elevation of the marsh in meters NAVD88.
#' @param print_results TRUE/FALSE. Do you want to print the calculated recent local SLR rate to the console? Defaults to TRUE.
#' @param return_all TRUE/FALSE. Do you want to retun a list of dataframes including all raw data, intermediate data, and recent local slr rate? Defaults to FALSE.
#' 
#' @import dplyr
#' @import purrr
#' @import lubridate
#' 
#' @returns A dataframe with the recent local slr rate or list of data frames with all raw data, intermediate data and recent local slr rate.
#' @export
#'
#' @examples
#' calc_recent_local_slr2(data = )
calc_recent_local_slr2 <- function(data, record = "water level", print_results = TRUE, return_all = FALSE) {
  
  Location <- data$TimeSeries$LocationIdentifier
  record_name <- paste0(record, "@", Location)
  
  df <- TS_simplify(data = data) %>% 
    select("datetime" = date_time, "water_level" = value) %>% # select and rename the date and water level columns
    filter(!is.na(water_level)) 
  
  water_level_seas_cycle_removed <- df %>% 
    mutate(month = stringr::str_sub(as.character(datetime), 6, 7),
           year = stringr::str_sub(as.character(datetime), 1, 4)
    ) %>% 
    group_by(month) %>%
    mutate(seas_cycle = mean(water_level, na.rm = T),
           water_level_seas_cycle_removed = water_level - seas_cycle # remove the average seasonal cycle from the raw WL data
    ) %>% 
    mutate(date = as.Date(datetime),
           date_num = as.numeric(date - first(date)) / 365.25) %>%
    ungroup()
  
  recent_local_slr <- water_level_seas_cycle_removed %>%
    nest() %>%
    mutate(Location = Location, 
           record = record_name,
           model = purrr::map(data, function(df) summary(lm(water_level_seas_cycle_removed ~ date_num, data = df))),
           recent_slr_rate = purrr::map_dbl(model, function(mod) mod[["coefficients"]][[2]]*1000),
           slr_se = purrr::map_dbl(model, function(mod) mod$coefficients[[2, 2]]*1000)
    ) %>%
    select(Location, record, recent_slr_rate, slr_se) 
  
  if (print_results) {
    cat(paste0("Local SLR rate ", "(",  lubridate::year(min(water_level_seas_cycle_removed$date)), "-", 
               lubridate::year(max(water_level_seas_cycle_removed$date)), "): ",
               format(round(recent_local_slr$recent_slr_rate, 2), nsmall = 2), " mm/yr"))
  }
  
  # Return all raw data, intermediate data, and flood metrics?
  if (return_all) {
    results <- list("raw_record" = raw_record, "simplified_record" =  df, "recent_local_slr" =  recent_local_slr)
  } else {
    results <- recent_local_slr
  }
}
