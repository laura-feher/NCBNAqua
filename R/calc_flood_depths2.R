#' Calculate flooding depths and marsh flooding metrics from a user-supplied data frame or saved file of raw water level data
#'
#' @param data data frame. A data frame of raw water level data.
#' @param record string (optional). The name of the data record to be used in the output data frame of flooding metrics. Defaults to "water level".
#' @param marsh_elev number. The elevation of the marsh in meters NAVD88.
#' @param print_results TRUE/FALSE. Do you want to print the calculated flood metrics to the console? Defaults to TRUE.
#' @param return_all TRUE/FALSE. Do you want to retun a list of dataframes including all raw data, intermediate data, and flood depths? Defaults to FALSE.
#' 
#' @import dplyr
#' @import lubridate
#' 
#' @note Adapted from \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#' @returns A dataframe with flooding depths or list of data frames with all raw data, intermediate data and flooding depth data.
#' @export
#'
#' @examples
#' calc_flood_depths2(data = )
calc_flood_depths2 <- function(data, record = "water level", marsh_elev, print_results = TRUE, return_all = FALSE) {
  
  Location <- data$TimeSeries$LocationIdentifier
  record_name <- paste0(record, "@", Location)
  
  # Transform raw data into a simplified format and calculate flooding depths
  df <- TS_simplify(data) %>%
    select("datetime" = date_time, "water_level" = value) %>% # select and rename the date and water level columns
    filter(!is.na(water_level)) %>%
    mutate(datetime = lubridate::ymd_hms(datetime, quiet = TRUE),
           flood_depth = if_else(water_level > marsh_elev, water_level - marsh_elev, NA_real_))
  
 flood_depths <- df %>%
   summarise(flooding_time = sum(!is.na(flood_depth)) * (15/60),
             total_time = sum(!is.na(water_level)) * (15/60),
             time_flooded = (sum(!is.na(flood_depth))/sum(!is.na(water_level))) * 100,
             avg_depth = mean(flood_depth, na.rm = TRUE) *100
            ) %>%
   mutate(Location = Location,
          record = record_name) %>%
   select(Location, record, flooding_time, total_time, time_flooded, avg_depth)
  
  # Do you want to print the results to the console?
  if (print_results) {
    cat(paste0("Time flooded: ", format(round(flood_depths$time_flooded, 1), nsmall = 1), " %\n",
               "Average depth: ", format(round(flood_depths$avg_depth, 1), nsmall = 1), " cm"))
  }
  
  # Return all raw data, intermediate data, and flood metrics?
  if (return_all) {
    results <- list("raw_record" = raw_record, "simplified_record" =  df, "flood_depths" =  flood_depths)
  } else {
    results <- flood_depths
  }
  
  return(results)
}