#' Download Aquarius time series data
#'
#' @param record string. The concatenated name of a time series record + a
#'   location name - e.g., "Depth.meters NAVD88@@ASIS_M5_WaterLevel".
#' @param start_date string (optional). Filter time series to data after a
#'   specific date.
#' @param end_date string (optional). Filter time series to data before a
#'   specific date.
#'
#' @details The object "Points" from the output is the data that is used to
#'   calculate tidal datums and is used by all other functions in the package.
#'
#' @note Adapted from
#'   \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#'
#' @returns A list of objects containing raw time series water lavel data and
#'   other metadata including:
#' \itemize{
#'  \item TimeSeries - data record IDs, location IDs, parameters, units.
#'  \item TimeRange - the start and end time of the data record.
#'  \item NumPoints - the number of observations in the record.
#'  \item \strong{Points} - the raw time series water level data. *This is the
#'  data used by all other functions in the package.
#'  \item ResponseVersion
#'  \item ResponseTime - elapsed time for the API call.
#'  \item Summary - info source description and disclaimer.
#'  }
#'  
#' @name get_timeseries
#' 
#' @export
#'
#' @examples
#' ## global variables for api connection - only run once per session
#' timeseries = timeseriesClient()
#' timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
#' publishapiurl = 'https://aquarius.nps.gov/aquarius/Publish/v2'
#' 
#' # Get water level data for COLO M30 for 2024 
#' Get_timeseries1(record = "Depth.Meters NAVD88@@COLO_M30_WaterLevel", start_date = "2024-01-01", end_date = "2024-12-31")
#' 
#' # Get water level data for COLO M30 entire record
#' Get_timeseries(record = "Depth.Meters NAVD88@@COLO_M30_WaterLevel")
#' 
#' # Extract the data frame of raw water level data 
#' Get_timeseries1(record = "Depth.Meters NAVD88@@COLO_M30_WaterLevel", start_date = "2024-01-01", end_date = "2024-12-31") %>%
#'   purrr::pluck("Points")
#' 
#' @rdname get_timeseries
Get_timeseries1 <- function(record , start_date, end_date){
  tsdata = timeseries$getTimeSeriesData(record,
                                        queryFrom = start_date,
                                        queryTo   = end_date)
  return(tsdata)
}
#' 
#' @rdname get_timeseries
Get_timeseries2 <- function(record){
  tsdata = timeseries$getTimeSeriesData(record,
                                        queryFrom = "1900-01-01T00:00:00-05:00",
                                        queryTo   = "2029-01-01T00:00:00-05:00")
  return(tsdata)
}