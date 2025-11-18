#' Load water level data from NPS Aquarius for a specific park
#'
#' Utilizes functions from the package
#' \link[=https://github.com/nationalparkservice/imd-fetchaquarius]{imd-fetchaquarius}
#' to connect to the NPS Aquarius database.
#'
#' @param park_code string (required); The 4 character park code - must be
#'   capitalized (e.g., "ASIS", "GATE"). Options include ASIS, ACAD, BISC, CACO,
#'   COLO, FIIS, GATE, GWMP, NACE, SARI, or VIIS.
#'
#' @returns A data frame of water level (i.e., water level) data from the
#'   selected park. Data frame columns are defined as: 
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
#'
#' @import fetchaquarius
#'
#' @export
#'
#' @note This function gets water level data for sites that have either "Depth"
#'   or "Water Level" as one of the parameters. Output units will be the same as
#'   whatever was entered into Aquarius when the data was uploaded (either raw
#'   meters or meters referenced to NAVD88).
#'
#' @examples
#' library(tidyverse)
#' library(remotes)
#' library(fetchaquarius)
#' 
#' asis_wl <- get_wl_data(park = "ASIS")
#' 
get_wl_data <- function(park_code) {
  
  if(park_code %in% c("ASIS", "ACAD", "COLO", "CACO", "FIIS", "GATE")) {
    folder <- "National Park Service.Northeast Coastal and Barrier Network.SET_Water_Level_Data"
  } else if(park_code %in% c("GWMP", "NACE")) {
    folder <- "National Park Service.National Capital Region Network"
  } else if (park_code == "BISC") {
    folder <- "National Park Service.South Florida Caribbean Network.BISC"
  } else if (park_code == "SARI") {
    folder <- "National Park Service.South Florida Caribbean Network.SARI"
  } else if (park_code == "VIIS") {
    folder <- "National Park Service.South Florida Caribbean Network.VIIS"
  }
  
  if (park_code == "GWMP") {
   location_ids <- fetchaquarius::getLocationInfo(folder = folder) %>%
      filter(Identifier == "GWMP_DykeMarsh_WL")
  } else if (park_code == "NACE") {
    location_ids <- fetchaquarius::getLocationInfo(folder = folder) %>%
      filter(Identifier == "NACE_KENI_WaterLevel") 
  } else {
    location_ids <- fetchaquarius::getLocationInfo(folder = folder) 
  }
  
  location_ids %>%
    mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
    select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
    unnest(cols = c(parameters)) %>%
    filter(str_detect(Parameter, "Depth") | str_detect(Parameter, "Water Level")) %>% # filter to the water level time series
    filter(str_detect(Identifier, park_code)) %>%
    mutate(wl_timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
    mutate(park = park_code) %>%
    select(Name, Identifier, LocationIdentifier, park, Unit, Label, wl_timeseries) %>%
    mutate(wl_data = map(wl_timeseries, ~.x$Points)) %>%
    select(-wl_timeseries) %>%
    unnest(cols = wl_data) %>%
    unnest(cols = Value) %>%
    rename("water_level" = Numeric, "datetime" = Timestamp) %>%
    as.data.frame %>%
    separate(., col = datetime, into = c("date", "time"), sep = " ", remove = FALSE) %>%
    mutate(time = if_else(is.na(time), "00:00:00", time),
           is_navd88 = if_else(str_detect(Label, "NAVD88") | str_detect(Label, "NAVD 88"), TRUE, FALSE)) %>% # note that output units will be the same as whatever is in Aquarius
    filter(!is.na(water_level)) %>%
    { if(park_code == "VIIS")
      filter(., str_sub(time, -2) == "00") # for VIIS, remove duplicate measurements that occurred at 37 seconds on the hour
      else .
      }
}
