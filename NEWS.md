# rnpn (development version)

* `nnpn` no longer depends on the `sp` or `raster` packages
* `terra` is now a suggested dependency and users will be prompted to install it only when it is needed

# rnpn 1.2.9 (2024-08-18)

### NEW FEATURES

* Fixed failed tests due vignette calling geoserver directly

# rnpn 1.2.8 (2024-02-08)

### NEW FEATURES

* Fixed failed tests due to server migration

# rnpn 1.2.7 (2024-01-23)

### NEW FEATURES

* Migrate back end to cloud instances

# rnpn 1.2.6 (2023-08-28)

### NEW FEATURES

* Remove rgdal dependencies
* Update maintainer to Jeff Switzer

# rnpn 1.2.5 (2022-04-20)

### NEW FEATURES

* New vignette, #8 on Data Cleaning
* Changes to reflect repository being transferred from ropensci to usa-npn in github

# rnpn 1.2.4 (2021-11-10)

### NEW FEATURES

* Skipping more API-dependent unit tests.

# rnpn 1.2.3 (2021-10-22)

### NEW FEATURES

* Skipping failing API-dependent unit tests and improved documentation.

# rnpn 1.2.2 (2021-10-04)

### NEW FEATURES

* Fixing failing unit tests

# rnpn 1.2.1 (2021-04-05)

### NEW FEATURES

* Fixing failing unit tests

# rnpn 1.2.0 (2021-03-19)

### NEW FEATURES

* Graceful fails when NPN data services are unavailable

# rnpn 1.1.1 (2020-10-27)

### NEW FEATURES

* Total overhaul of the rNPN package
* Added functions for directly downloading different observation record data types
* Added additional utility and lookup type functions
* Added functions for downloading USA-NPN raster data and geospatial values by latitude/longitude
* Deprecated the following functions: lookup_names, npn_allobssp, npn_indsatstations, npn_indspatstations, npn_species_comm, npn_species_itis, npn_species_sci, npn_stationsbystate, npn_stationswithspp

# rnpn 0.1.0

### NEW FEATURES

* released to CRAN
