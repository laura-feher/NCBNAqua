
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NCBNAqua

<!-- badges: start -->

<!-- badges: end -->

The goal of NCBNAqua is to …

## Installation

You can install the development version of NCBNAqua from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("laura-feher/NCBNAqua")
```

``` r
library(httr)
library(jsonlite)
## global variables for api connection 
# only run once per session
# https://katemmiller.github.io/IMD_R_Training_Advanced/
# timeseries = timeseriesClient()
# timeseries$connect("https://aquarius.nps.gov/aquarius", "aqreadonly", "aqreadonly")
# publishapiurl = 'https://aquarius.nps.gov/aquarius/Publish/v2'
```

``` r
## View all locations with water data in a specific I&M Network
#Print_Locations(Network = "Northeast Coastal and Barrier Network") 
```

``` r
# View sites associated with the NCBN SETs
# Print_Locations(Network = "Northeast Coastal and Barrier Network") %>%
#   filter(stringr::str_detect(Identifier, "WaterLevel$")) # select sites with the suffix "WaterLevel"
```

``` r
#enter the location of interest to get a list of datasets:
# Print_datasets(Location = "ASIS_M5_WaterLevel")
```

``` r
#you can also print available metadata for a given location based on it's identifier:
# Print_metadata(Location = "ASIS_M5_WaterLevel")
```

``` r
# Pull the raw dataset from Aquarius
# raw_record <- Get_timeseries2(record = "Depth.meters NAVD88@ASIS_M5_WaterLevel") 

# head(raw_record$Points)
```

``` r
#this function will "clean it up"
# temp_data <- TS_simplify(data = raw_record)

# head(temp_data)
```
