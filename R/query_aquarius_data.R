#' Query info about Aquarius locations and datasets
#'
#' @description Query the Aquarius database to see available locations,
#'   datasets, and metadata.
#' \itemize{
#'  \item `Print_Locations()` shows all locations with available data in a
#'  specific I&M network.
#'  \item `Print_datasets()` shows all data sets available for a specific
#'  location.
#'  \item `Print_metadata()` shows metadata for a specific location.
#'  }
#'
#' @param Network string. The full name of an NPS I&M network e.g. "Northeast
#'   Coastal and Barrier Network".
#' @param Location string. A site name from the Aquarius database e.g.
#'   "ASIS_M5_WaterLevel".
#'
#' @note Adapted from
#'   \url{https://github.com/AndrewBirchHydro/albAquariusTools}.
#'
#' @name query_aquarius
#'
#' @export
#'
#' @examples
#' ## global variables for api connection - only run once per session
#' timeseries = timeseriesClient()
#' timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
#' publishapiurl = 'https://aquarius.nps.gov/aquarius/Publish/v2'
#'
#' Print_Locations(Network = "Northeast Coastal and Barrier Network")
#'
#' Print_datasets(Location = "COLO_M30_WaterLevel")
#'
#' Print_metadata(Location = "COLO_M30_WaterLevel")
#'
#' ## can also save results to a list or dataframe
#' ncbn_locations <- Print_Locations(Network = "Northeast Coastal and Barrier Network")
#'
#' COLO_M30_metadata <- Print_metadata(Location = "COLO_M30_WaterLevel")
#'
#' @rdname query_aquarius
Print_Locations <- function(Network){
  locdata <- httr::GET(paste0(publishapiurl,'/GetLocationDescriptionList',sep=''))
  alllocs <- jsonlite::fromJSON(content(locdata,"text"))$LocationDescriptions
  # Filter Locations to just include those in specified folder
  Locs<-alllocs[grep(Network,alllocs$PrimaryFolder),]
  return(Locs)
}
#' 
#' @rdname query_aquarius
Print_datasets <- function(Location){
  getlocds <- httr::GET(paste0(publishapiurl,'/GetTimeSeriesDescriptionList?LocationIdentifier=',Location,sep=''))
  locsds <- jsonlite::fromJSON(content(getlocds,"text"))$TimeSeriesDescriptions
  print(locsds)
  return(locsds)
}
#' 
#' @rdname query_aquarius
Print_metadata <- function(Location){
  locationinfo <- timeseries$getLocationData(Location)
  return(locationinfo)
}