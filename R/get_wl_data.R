#' Load water logger data from NPS Aquarius for a specific park and NCBN
#' protocol
#'
#' Utilizes functions from the package
#' \link[=https://github.com/nationalparkservice/imd-fetchaquarius]{imd-fetchaquarius}
#' to connect to the NPS Aquarius database.
#'
#' @param park_code string (required); The 4 character park code - must be
#'   capitalized (e.g., "ASIS"). For SET sites, options include ASIS, ACAD,
#'   BISC, CACO, COLO, FIIS, GATE, GWMP, NACE, SARI, or VIIS. For ENE Seagrass,
#'   options include ASIS, CACO, or FIIS. For ENE WQ, options include ASIS,
#'   CACO, COLO, FIIS, GATE, or GWMP.
#' 
#' @param protocol string (required); The protocol type that you want to return
#'   data for. Either "SET", "ENE_Seagrass", or "ENE_WQ". Default is "SET".
#'
#' @returns For SET sites, a data frame of water level data from the selected
#'   park. Data frame columns are defined as:
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
#'   For Seagrass or ENE sites, returns a data frame of time series for all
#'   available parameters.
#'
#' @import fetchaquarius
#' @import stringr
#'
#' @export
#'
#' @note For SET sites, this function gets water level data for sites that have
#'   either "Depth" or "Water Level" as one of the parameters. Output units will
#'   be the same as whatever was entered into Aquarius when the data was
#'   uploaded (either raw meters or meters referenced to NAVD88).
#'
#' @examples
#' library(tidyverse)
#' library(remotes)
#' library(fetchaquarius)
#'
#' asis_wl <- get_wl_data(park = "ASIS", protocol = "SET")
#' 
get_wl_data <- function(park_code, protocol = "SET") {
  
  # Set the folder(s) based on park code and protocol
  if (protocol == "SET") {
    if (park_code %in% c("ASIS", "FIIS")) {
      folder <- "National Park Service.Northeast Coastal and Barrier Network.SET_Water_Level_Data"
    } else if (park_code %in% c("ACAD", "COLO", "CACO", "GATE")) {
      folder <- c("National Park Service.Northeast Coastal and Barrier Network.SET_Water_Level_Data",
                  "National Park Service.Northeast Coastal and Barrier Network")
    } else if (park_code %in% c("GWMP", "NACE")) {
      folder <- "National Park Service.National Capital Region Network"
    } else if (park_code == "BISC") {
      folder <- "National Park Service.South Florida Caribbean Network.BISC"
    } else if (park_code == "SARI") {
      folder <- "National Park Service.South Florida Caribbean Network.SARI"
    } else if (park_code == "VIIS") {
      folder <- "National Park Service.South Florida Caribbean Network.VIIS"
    } else {
      stop(paste0("No water level data available for SET at ", park_code))
    }
  } else if (protocol == "ENE_Seagrass") {
    if (park_code %in% c("ASIS", "FIIS")) {
      folder <- c("National Park Service.Northeast Coastal and Barrier Network", 
                  "National Park Service.Northeast Coastal and Barrier Network.ENE_Seagrass_Continuous")
    } else if (park_code == "CACO") {
      folder <- "National Park Service.Northeast Coastal and Barrier Network.ENE_Seagrass_Continuous"
    } else {
      stop(paste0("No water data available for Seagrass at ", park_code))
    }
  } else if (protocol == "ENE_WQ") {
    if (park_code %in% c("ASIS", "CACO", "COLO", "FIIS", "GATE", "GEWA")) {
      folder <- "National Park Service.Northeast Coastal and Barrier Network.ENE_WQ_LoggingStations"
    } else {
      stop(paste0("No water data available for ENE WQ at ", park_code))
    }
  } else {
    stop(paste0("Choose a protocol - must be either SET, ENE_Seagrass, or ENE_WQ."))
  }
  
  # Set location_ids based on park code and protocol
  if (protocol == "SET") {
    if (park_code == "GWMP") {
      location_ids <- fetchaquarius::getLocationInfo(folder = folder) %>%
        filter(Identifier == "GWMP_DykeMarsh_WL")
    } else if (park_code == "COLO") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(Identifier %in% c("COLO_M19_WaterLevel", "COLO_M30_WaterLevel")) %>%
        distinct()
    } else if (park_code == "ACAD") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(Identifier %in% c("ACAD_BH_WaterLevel", "ACAD_MCHT_WaterLevel", "ACAD_SCH_WaterLevel", "ACAD_TI_WaterLevel")) %>%
        distinct()
    } else if (park_code == "CACO") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(Identifier %in% c("CACO_BlackfishCreek_WaterLevel", "CACO_HH_Restricted_WaterLevel", "CACO_HH_Unrestricted_WaterLevel", "CACO_NausetNorth_WaterLevel", "CACO_NausetSouth_WaterLevel")) %>%
        distinct()
    } else if (park_code == "GATE") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(Identifier %in% c("GATE_BB_Waterlevel", "GATE_BE_Waterlevel", "GATE_Joco_Waterlevel", "GATE_SAHU_WaterLevel")) %>%
        distinct()
    } else if (park_code == "NACE") {
      location_ids <- fetchaquarius::getLocationInfo(folder = folder) %>%
        filter(Identifier == "NACE_KENI_WaterLevel") 
    } else {
      location_ids <- fetchaquarius::getLocationInfo(folder = folder) 
    }
  } else if (protocol == "ENE_Seagrass") {
    if (park_code == "ASIS") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(Identifier %in% c("ASIS_Seagrass_Grays_Cove", "ASIS_Seagrass_Wildcat", "ASIS_Seagrass")) %>%
        distinct()
    } else if (park_code == "FIIS") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(Identifier %in% c("FIIS_Seagrass_MB", "FIIS_2009-2011", "FIIS_Seagrass_GSB", "FIIS_GSB_WaterLevel")) %>%
        distinct()
    } else if (park_code == "CACO") {
      location_ids <- fetchaquarius::getLocationInfo(folder = folder) %>%
        filter(Identifier %in% c("CACO_DH_WaterLevel", "CACO_PB_WaterLevel", "CACO_Seagrass_MA20_1", "CACO_Seagrass_MA20_2"))
    }
  } else if (protocol == "ENE_WQ") {
    if (park_code == "ASIS") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(stringr::str_detect(Identifier, "^ASIS")) %>%
        distinct()
    } else if (park_code == "CACO") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(stringr::str_detect(Identifier, "^CACO")) %>%
        filter(!Identifier %in% c("CACO_PB_2007", "CACO_PB_2008", "CACO_PB_2009", "CACO_PB_2010", "CACO_SP_2007", "CACO_SP_2008", "CACO_SP_2009", "CACO_SP_2010", "CACO_NM_2007", "CACO_NM_2008", "CACO_NM_2009", "CACO_NM_2010")) %>%
        distinct()
    } else if (park_code == "COLO") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(stringr::str_detect(Identifier, "^COLO")) %>%
        distinct()
    } else if (park_code == "FIIS") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(stringr::str_detect(Identifier, "^FIIS")) %>%
        distinct()
    } else if (park_code == "GATE") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(stringr::str_detect(Identifier, "^GATE")) %>%
        distinct()
    } else if (park_code == "GEWA") {
      location_ids <- map(folder, ~fetchaquarius::getLocationInfo(folder = .x)) %>%
        bind_rows() %>%
        filter(stringr::str_detect(Identifier, "^GEWA")) %>%
        filter(Identifier != "GEWA_2011") %>%
        distinct()
    }
  }
  
  # Get data and format it based on protocol
  if (protocol == "SET") {
    data <- suppressWarnings(location_ids %>%
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
    )
  } else if (protocol == "ENE_Seagrass") {
    data <- suppressWarnings(location_ids %>%
      mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
      select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
      unnest(cols = c(parameters)) %>%
      mutate(timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
      select(Name, Identifier, LocationIdentifier, Unit, Label, timeseries) %>%
      mutate(data = map(timeseries, ~.x$Points)) %>%
      select(-timeseries) %>%
      unnest(cols = data) %>%
      unnest(cols = Value) %>%
      rename("Value" = Numeric) %>%
      as.data.frame %>%
      separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
      mutate(Time = if_else(is.na(Time), "00:00:00", Time)))
  } else if (protocol == "ENE_WQ") {
    data <- suppressWarnings(location_ids %>%
      mutate(parameters = map(Identifier, ~fetchaquarius::getTimeSeriesInfo(.x))) %>%
      select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
      unnest(cols = c(parameters)) %>%
      mutate(timeseries = map(Identifier, ~fetchaquarius::getTimeSeries(.x))) %>%
      select(Name, Identifier, LocationIdentifier, Unit, Label, timeseries) %>%
      mutate(data = map(timeseries, ~.x$Points),
             data_length = map_dbl(data, ~length(.x))) %>%
      filter(data_length != 0) %>%
      select(-timeseries) %>%
      unnest(cols = data) %>%
      unnest(cols = Value) %>%
      rename("Value" = Numeric) %>%
      as.data.frame %>%
      separate(., col = Timestamp, into = c("Date", "Time"), sep = " ", remove = FALSE) %>%
      mutate(Time = if_else(is.na(Time), "00:00:00", Time)))
  }
  
  return(data)
}
