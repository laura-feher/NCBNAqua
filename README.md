
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NCBNAqua

NCBNAqua provides functions for downloading and working with water data
for the NCBN protocols from the NPS Aquarius database.This package
utilizes the National Park Service
[fetchaquarius](https://github.com/nationalparkservice/imd-fetchaquarius)
package for the API connection to the database.

## Installation

You can install the development version of NCBNAqua from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("laura-feher/NCBNAqua")
```

## Set & Store API credentials

The first time you use the package, you’ll need to set and store
credentials for the Aquarius API using the
[keyring](https://keyring.r-lib.org/) package. Running the function
keyring::key_set() will open a dialog box that will prompt you for a
password. Note that this only needs to be done once. Contact laura_feher
at nps.gov for more info.

``` r
library(keyring)

# keyring::key_set("aquarius", "aqreadonly")
```

## Establish a Connection to Aquarius

``` r
# establish connection to Aquarius
fetchaquarius::connectToAquarius("aqreadonly")
```

## Download water data for a specific NCBN park and protocol

The function get_wl_data() downloads the data from Aquarius and formats
it into a single data frame.

``` r
library(tidyverse)
library(NCBNAqua)

# Download water level data from the SET sites at ASIS
asis_set_wl <- get_wl_data(park_code = "ASIS", protocol = "SET")

head(asis_set_wl)
#>                                        Name
#> 1 Assateague Island NS - M5 -Pope Bay Marsh
#> 2 Assateague Island NS - M5 -Pope Bay Marsh
#> 3 Assateague Island NS - M5 -Pope Bay Marsh
#> 4 Assateague Island NS - M5 -Pope Bay Marsh
#> 5 Assateague Island NS - M5 -Pope Bay Marsh
#> 6 Assateague Island NS - M5 -Pope Bay Marsh
#>                               Identifier LocationIdentifier park Unit
#> 1 Depth.meters NAVD88@ASIS_M5_WaterLevel ASIS_M5_WaterLevel ASIS    m
#> 2 Depth.meters NAVD88@ASIS_M5_WaterLevel ASIS_M5_WaterLevel ASIS    m
#> 3 Depth.meters NAVD88@ASIS_M5_WaterLevel ASIS_M5_WaterLevel ASIS    m
#> 4 Depth.meters NAVD88@ASIS_M5_WaterLevel ASIS_M5_WaterLevel ASIS    m
#> 5 Depth.meters NAVD88@ASIS_M5_WaterLevel ASIS_M5_WaterLevel ASIS    m
#> 6 Depth.meters NAVD88@ASIS_M5_WaterLevel ASIS_M5_WaterLevel ASIS    m
#>           Label            datetime       date     time water_level is_navd88
#> 1 meters NAVD88 2012-03-07 17:00:00 2012-03-07 17:00:00      -0.110      TRUE
#> 2 meters NAVD88 2012-03-07 17:15:00 2012-03-07 17:15:00      -0.112      TRUE
#> 3 meters NAVD88 2012-03-07 17:30:00 2012-03-07 17:30:00      -0.122      TRUE
#> 4 meters NAVD88 2012-03-07 17:45:00 2012-03-07 17:45:00      -0.126      TRUE
#> 5 meters NAVD88 2012-03-07 18:00:00 2012-03-07 18:00:00      -0.132      TRUE
#> 6 meters NAVD88 2012-03-07 18:15:00 2012-03-07 18:15:00      -0.138      TRUE
```

``` r
# Download seagrass water data (all parameters) from CACO
caco_seagrass_wl <- get_wl_data(park_code = "CACO", protocol = "ENE_Seagrass")

head(caco_seagrass_wl)
#>          Name                               Identifier LocationIdentifier Unit
#> 1 Duck Harbor Absolute Pressure.kPa@CACO_DH_WaterLevel CACO_DH_WaterLevel  kPa
#> 2 Duck Harbor Absolute Pressure.kPa@CACO_DH_WaterLevel CACO_DH_WaterLevel  kPa
#> 3 Duck Harbor Absolute Pressure.kPa@CACO_DH_WaterLevel CACO_DH_WaterLevel  kPa
#> 4 Duck Harbor Absolute Pressure.kPa@CACO_DH_WaterLevel CACO_DH_WaterLevel  kPa
#> 5 Duck Harbor Absolute Pressure.kPa@CACO_DH_WaterLevel CACO_DH_WaterLevel  kPa
#> 6 Duck Harbor Absolute Pressure.kPa@CACO_DH_WaterLevel CACO_DH_WaterLevel  kPa
#>   Label           Timestamp       Date     Time   Value
#> 1   kPa 2019-07-30 20:15:03 2019-07-30 20:15:03 101.548
#> 2   kPa 2019-07-30 20:21:03 2019-07-30 20:21:03 101.556
#> 3   kPa 2019-07-30 20:27:03 2019-07-30 20:27:03 101.549
#> 4   kPa 2019-07-30 20:33:03 2019-07-30 20:33:03 101.538
#> 5   kPa 2019-07-30 20:39:03 2019-07-30 20:39:03 114.778
#> 6   kPa 2019-07-30 20:45:03 2019-07-30 20:45:03 114.916
```

``` r
# Download ENE water quality data (all parameters) from GEWA and filter to just Chlorophyll 
gewa_wq_wl <- get_wl_data(park_code = "GEWA", protocol = "ENE_WQ") %>%
  filter(Label %in% c("Chl", "Chlorophyll µg/L"))

head(gewa_wq_wl)
#>          Name                Identifier LocationIdentifier Unit Label
#> 1 Popes Creek Chlorophyll.Chl@GEWA_2009          GEWA_2009 ug/l   Chl
#> 2 Popes Creek Chlorophyll.Chl@GEWA_2009          GEWA_2009 ug/l   Chl
#> 3 Popes Creek Chlorophyll.Chl@GEWA_2009          GEWA_2009 ug/l   Chl
#> 4 Popes Creek Chlorophyll.Chl@GEWA_2009          GEWA_2009 ug/l   Chl
#> 5 Popes Creek Chlorophyll.Chl@GEWA_2009          GEWA_2009 ug/l   Chl
#> 6 Popes Creek Chlorophyll.Chl@GEWA_2009          GEWA_2009 ug/l   Chl
#>             Timestamp       Date     Time Value data_length
#> 1 2009-07-21 14:16:00 2009-07-21 14:16:00  36.2           2
#> 2 2009-07-21 14:31:00 2009-07-21 14:31:00  55.2           2
#> 3 2009-07-21 14:46:00 2009-07-21 14:46:00  62.9           2
#> 4 2009-07-21 15:01:00 2009-07-21 15:01:00  57.9           2
#> 5 2009-07-21 15:16:00 2009-07-21 15:16:00  59.2           2
#> 6 2009-07-21 15:31:00 2009-07-21 15:31:00  45.5           2
```

## Working with Water Level data

The remaining functions in the package are meant to be used only with
water level data referenced to NAVD88 from the SET sites.

``` r
tidal_datums(wl_data = asis_set_wl)
#> # A tibble: 3 × 10
#> # Groups:   Identifier, Name, LocationIdentifier [3]
#>   Name      Identifier LocationIdentifier   MHW  MHHW     MLW    MLLW first_date
#>   <chr>     <chr>      <chr>              <dbl> <dbl>   <dbl>   <dbl> <chr>     
#> 1 Assateag… Depth.met… ASIS_M5_WaterLevel 0.156 0.173 -0.0420 -0.0600 2012-03-07
#> 2 Assateag… Depth.met… ASIS_M6_WaterLevel 0.156 0.177 -0.0519 -0.0741 2012-03-07
#> 3 Assateag… Depth.met… ASIS_M8_WaterLevel 0.126 0.145 -0.0686 -0.0867 2012-03-08
#> # ℹ 2 more variables: last_date <chr>, record_count <int>
```
