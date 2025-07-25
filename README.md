
# rnpn

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![CRAN
status](https://www.r-pkg.org/badges/version/rnpn)](https://CRAN.R-project.org/package=rnpn)
[![R-CMD-check](https://github.com/usa-npn/rnpn/actions/workflows/R-CMD-check.yaml/badge.svg?branch=master)](https://github.com/usa-npn/rnpn/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/usa-npn/rnpn/graph/badge.svg)](https://app.codecov.io/gh/usa-npn/rnpn)
<!-- badges: end -->

`rnpn` is an R client for interacting with the USA National Phenology
Network data web services. These services include access to a rich set
of observer-contributed, point-based phenology records as well as
geospatial data products including gridded phenological model and
climatological data.

Documentation is available for the National Phenology Network [API
documentation](https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit?hl=en_US),
which describes the full set of REST services this package wraps.

There is no need for an API key to grab data from the National Phenology
Network but users are required to self identify, on an honor system,
against requests that may draw upon larger datasets. For functions that
require it, simply populate the request_source parameter with your name
or the name of your institution.

## Installation

CRAN version

``` r
install.packages("rnpn")
```

Development version:

``` r
install.packages("devtools")
library('devtools')
devtools::install_github("usa-npn/rnpn")
```

``` r
library('rnpn')
```

This package has dependencies on both curl and gdal. Some Linux based
systems may require additional system dependencies for those required
packages, and accordingly this package, to install correctly. For
example, on Ubuntu:

``` r
sudo apt install libcurl4-openssl-dev
sudo apt install libproj-dev libgdal-dev
```

## The Basics

Many of the functions to search for data require knowing the internal
unique identifiers of some of the database entities to filter the data
down efficiently. For example, if you want to search by species, then
you must know the internal identifier of the species. To get a list of
all available species use the following:

``` r
species_list <- npn_species()
```

Similarly, for phenophases:

``` r
phenophases <- npn_phenophases()
```

### Getting Observational Data

There are four main functions for accessing observational data, at
various levels of aggregation. At the most basic level you can download
the raw status and intensity data.

``` r
some_data <- npn_download_status_data(
  request_source = 'Your Name or Org Here',
  years = c(2015),
  species_id = c(35),
  states = c('AZ', 'IL')
)
```

Note that through this API, data can only be filtered chronologically by
full calendar years. You can specify any number of years in each API
call. Also note that request_source is a required parameter and should
be populated with your name or the name of the organization you
represent. All other parameters are optional but it is highly
recommended that you filter your data search further.

### Getting Geospatial Data

This package wraps around standard WCS endpoints to facilitate the
transfer of raster data. Generally, this package does not focus on
interacting with WMS services, although they are available. To get a
list of all available data layers, use the following:

``` r
layers <- npn_get_layer_details()
```

You can then use the name of the layers to select and download
geospatial data as a raster.

``` r
npn_download_geospatial(
  coverage_id = 'si-x:lilac_leaf_ncep_historic',
  date = '2016-12-31',
  format = 'geotiff',
  output_path = './six-test-raster.tiff'
)
```

## Example of combined observational and geospatial data

For more details see Vignette VII

<img src="vignettes/figures/7-plot.png" width="70%" />

## What’s Next

Please read and review the vignettes for this package to get further
information about the full scope of functionality available.

## Acknowledgments

This code was developed, in part, as part of the integrated
[Pheno-Synthesis Software Suite
(PS3)](https://git.earthdata.nasa.gov/projects/APIS/repos/pheno-synthesis-software-suite/browse).
The authors acknowledge funding for this work through NASA’s AIST
program (80NSSC17K0582, 80NSSC17K0435, 80NSSC17K0538, and
80GSFC18T0003). The University of Arizona and the USA National Phenology
Network’s efforts with this package are supported in part by US
Geological Survey (G14AC00405, G18AC00135) and the US Fish and Wildlife
Service (F16AC01075 and F19AC00168).

## Meta

- Please [report any issues or
  bugs](https://github.com/usa-npn/rnpn/issues).
- License: MIT
- Get citation information for `rnpn` in R doing
  `citation(package = 'rnpn')`
- Please note that this package is released with a [Contributor Code of
  Conduct](https://ropensci.org/code-of-conduct/). By contributing to
  this project, you agree to abide by its terms.
