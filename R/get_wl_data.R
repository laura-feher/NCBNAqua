get_wl_data <- function(park) {
  if(park %in% c("ASIS", "ACAD", "COLO", "CACO", "FIIS", "GATE")) {
    folder <- "National Park Service.Northeast Coastal and Barrier Network.SET_Water_Level_Data"
  } else if (park %in% c("GWMP", "NACE")) {
    # need to fix this because the data for Dyke Marsh is in 2 different folders
    folder <- "National Park Service.National Capital Region Network"
    folder <- "National Park Service.National Capital Region Network.Dyke Marsh"
  } else if (park == "BISC") {
    folder <- "National Park Service.South Florida Caribbean Network.BISC"
  } else if (park == "SARI") {
    folder <- "National Park Service.South Florida Caribbean Network.SARI"
  } else if (park == "VIIS") {
    folder <- "National Park Service.South Florida Caribbean Network.VIIS"
  }
    
getLocationInfo(folder = folder) %>%
  filter(str_detect(Identifier, park)) %>%
  mutate(parameters = map(Identifier, ~getTimeSeriesInfo(.x))) %>%
  select(-c(Identifier, UniqueId, UtcOffset, LastModified, Publish, Tags)) %>%
  unnest(cols = c(parameters)) %>%
  filter(Unit == "m") %>%
  mutate(wl_timeseries = map(Identifier, ~getTimeSeries(.x))) %>%
  select(Name, Identifier, LocationIdentifier, wl_timeseries) %>%
  mutate(wl_data = map(wl_timeseries, ~.x$Points)) %>%
  select(-wl_timeseries) %>%
  unnest(cols = wl_data) %>%
  unnest(cols = Value) %>%
  rename("water_level" = Numeric, "datetime" = Timestamp) %>%
  as.data.frame %>%
  separate(., col = datetime, into = c("date", "time"), sep = " ", remove = FALSE) %>%
  mutate(time = if_else(is.na(time), "00:00:00", time)) %>%
  filter(!is.na(water_level))
}
