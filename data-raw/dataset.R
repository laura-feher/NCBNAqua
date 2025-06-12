# timeseries = timeseriesClient()
# timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
# publishapiurl = 'https://aquarius.nps.gov/aquarius/Publish/v2'

ncbn_wl_sites <- Print_Locations(Network = "Northeast Coastal and Barrier Network") %>%
  filter(stringr::str_like(Identifier, "%WaterLevel"))

saveRDS(ncbn_wl_sites, file = here::here("data", "ncbn_wl_sites.Rds"))


### A list of data frames from Get_timeseries()

asis_2024_aq_data <- Get_timeseries1(record = "Depth.meters NAVD88@ASIS_M5_WaterLevel", start_date = "2024-01-01", end_date = "2024-12-31")

saveRDS(asis_2024_aq_data, file = here::here("data", "asis_2024_aq_data.Rds")) 


### Just the "Points" dataframe from the list produced by Get_timeseries with columns renamed 

asis_2024_aq_points <- asis_2024_aq_data$Points %>%
  rename("time" = Timestamp,
         "wl" = NumericValue1)

saveRDS(asis_2024_aq_points, file = here::here("data", "asis_2024_aq_points.Rds"))
readr::write_csv(asis_2024_aq_points, file = here::here("data", "asis_2024_aq_points.csv"))
