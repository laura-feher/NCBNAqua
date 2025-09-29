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
   location_ids <- getLocationInfo(folder = folder) %>%
      filter(Identifier == "GWMP_DykeMarsh_WL")
  } else if (park_code == "NACE") {
    location_ids <- getLocationInfo(folder = folder) %>%
      filter(Identifier == "NACE_KENI_WaterLevel") 
  } else {
    location_ids <- getLocationInfo(folder = folder) 
  }
  
  location_ids %>%
    mutate(parameters = map(Identifier, ~getTimeSeriesInfo(.x))) %>%
    select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
    unnest(cols = c(parameters)) %>%
    filter(Unit == "m") %>% # filter to the water level depth time series
    filter(str_detect(Identifier, park_code)) %>%
    mutate(wl_timeseries = map(Identifier, ~getTimeSeries(.x))) %>%
    mutate(park = park_code) %>%
    select(Name, Identifier, LocationIdentifier, park, wl_timeseries) %>%
    mutate(wl_data = map(wl_timeseries, ~.x$Points)) %>%
    select(-wl_timeseries) %>%
    unnest(cols = wl_data) %>%
    unnest(cols = Value) %>%
    rename("water_level" = Numeric, "datetime" = Timestamp) %>%
    as.data.frame %>%
    separate(., col = datetime, into = c("date", "time"), sep = " ", remove = FALSE) %>%
    mutate(time = if_else(is.na(time), "00:00:00", time),
           units = if_else(park %in% c("ASIS", "ACAD", "COLO", "CACO", "FIIS", "GATE", "GWMP", "NACE"), "meters NAVD88", "meters")) %>%
    filter(!is.na(water_level))
}
