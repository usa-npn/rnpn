rnpn
========



[![Build Status](https://api.travis-ci.org/ropensci/rnpn.png)](https://travis-ci.org/ropensci/rnpn)
[![Build status](https://ci.appveyor.com/api/projects/status/es65utr5jmfmcsrg/branch/master)](https://ci.appveyor.com/project/sckott/rnpn/branch/master)

`rnpn` is an R client for the US National Phenology Network API.

National Phenology Network [API documentation](https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit?hl=en_US).

Note that there is no need for an API key to grab data from the National Phenology Network, but I think there is for writing data through the API. Currently, functions in this package only allow getting data, but may at some point allow posting data to the USNPN.

## Quick start

### Installation

Note: Windows users installing from GitHub should get Rtools - can be installed from http://www.murdoch-sutherland.com/Rtools/


```r
install.packages("devtools")
devtools::install_github("ropensci/rnpn")
```


```r
library('rnpn')
```

### Lookup names

You can lookup taxon names. This is not actually an API call to the web. The function simply searches for matches in a dataset stored in the package. You can then use the speciesid output in other functions.


```r
lookup_names(name='Pinus', type='genus')
#>     species_id                  common_name genus    epithet itis_tsn
#> 82         967                  Bishop pine Pinus   muricata   183359
#> 312         53           eastern white pine Pinus    strobus   183385
#> 370        219 Great Basin bristlecone pine Pinus   longaeva   183352
#> 458        220                  limber pine Pinus   flexilis   183343
#> 461         54                loblolly pine Pinus      taeda    18037
#> 462        762               lodgepole pine Pinus   contorta   183327
#> 465         52                longleaf pine Pinus  palustris    18038
#> 479        965               Mexican pinyon Pinus cembroides   183321
#> 584         25               ponderosa pine Pinus  ponderosa   183365
#> 618        968                     red pine Pinus   resinosa   183375
#> 698         51            singleleaf pinyon Pinus monophylla   183353
#> 704        295                   slash pine Pinus  elliottii    18036
#> 794         50             twoneedle pinyon Pinus     edulis   183336
#> 836        966           western white pine Pinus  monticola   183356
#>        genus_epithet
#> 82    Pinus muricata
#> 312    Pinus strobus
#> 370   Pinus longaeva
#> 458   Pinus flexilis
#> 461      Pinus taeda
#> 462   Pinus contorta
#> 465  Pinus palustris
#> 479 Pinus cembroides
#> 584  Pinus ponderosa
#> 618   Pinus resinosa
#> 698 Pinus monophylla
#> 704  Pinus elliottii
#> 794     Pinus edulis
#> 836  Pinus monticola
```

### Search

Search for a single species, specifying a start and end date. You can also pass a vector to the speciesid parameter.


```r
(out <- npn_allobssp(speciesid = 52, startdate='2008-01-01', enddate='2010-12-31'))
#> An object of class "npn"
#> Slot "taxa":
#>     species_id genus   epithet   genus_epithet
#> 465         52 Pinus palustris Pinus palustris
#> 
#> Slot "stations":
#>   station_id           station_name  latitude  longitude
#> 1       4881 Possum Branch Preserve 28.045185 -82.706299
#> 
#> Slot "phenophase":
#>   phenophase_id phenophase_name  color
#> 1           393 Ripe seed cones Green3
#> 
#> Slot "data":
#>                   date station_id species_id phenophase_id phen_seq
#> 1  2009-09-03 00:00:00       4881         52           393      300
#> 2  2009-09-10 00:00:00       4881         52           393      300
#> 3  2010-07-30 00:00:00       4881         52           393      300
#> 4  2010-08-13 00:00:00       4881         52           393      300
#> 5  2010-08-20 00:00:00       4881         52           393      300
#> 6  2010-09-03 00:00:00       4881         52           393      300
#> 7  2010-09-10 00:00:00       4881         52           393      300
#> 8  2010-09-17 00:00:00       4881         52           393      300
#> 9  2010-09-24 00:00:00       4881         52           393      300
#> 10 2010-10-08 00:00:00       4881         52           393      300
#> 11 2010-10-15 00:00:00       4881         52           393      300
#> 12 2010-10-22 00:00:00       4881         52           393      300
#> 13 2010-10-29 00:00:00       4881         52           393      300
#> 14 2010-11-05 00:00:00       4881         52           393      300
#> 15 2010-11-12 00:00:00       4881         52           393      300
#> 16 2010-11-19 00:00:00       4881         52           393      300
#> 17 2010-12-05 00:00:00       4881         52           393      300
#> 18 2010-12-11 00:00:00       4881         52           393      300
#> 19 2010-12-17 00:00:00       4881         52           393      300
#> 20 2010-12-30 00:00:00       4881         52           393      300
```

Coerce data to a data.frame that has most all data.


```r
npn_todf(out)
#> An object of class "npnsp"
#> Slot "data":
#>            sciname  latitude  longitude           station_name
#> 1  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 2  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 3  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 4  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 5  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 6  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 7  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 8  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 9  Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 10 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 11 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 12 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 13 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 14 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 15 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 16 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 17 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 18 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 19 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#> 20 Pinus palustris 28.045185 -82.706299 Possum Branch Preserve
#>                   date phen_seq genus   epithet   genus_epithet
#> 1  2009-09-03 00:00:00      300 Pinus palustris Pinus palustris
#> 2  2009-09-10 00:00:00      300 Pinus palustris Pinus palustris
#> 3  2010-07-30 00:00:00      300 Pinus palustris Pinus palustris
#> 4  2010-08-13 00:00:00      300 Pinus palustris Pinus palustris
#> 5  2010-08-20 00:00:00      300 Pinus palustris Pinus palustris
#> 6  2010-09-03 00:00:00      300 Pinus palustris Pinus palustris
#> 7  2010-09-10 00:00:00      300 Pinus palustris Pinus palustris
#> 8  2010-09-17 00:00:00      300 Pinus palustris Pinus palustris
#> 9  2010-09-24 00:00:00      300 Pinus palustris Pinus palustris
#> 10 2010-10-08 00:00:00      300 Pinus palustris Pinus palustris
#> 11 2010-10-15 00:00:00      300 Pinus palustris Pinus palustris
#> 12 2010-10-22 00:00:00      300 Pinus palustris Pinus palustris
#> 13 2010-10-29 00:00:00      300 Pinus palustris Pinus palustris
#> 14 2010-11-05 00:00:00      300 Pinus palustris Pinus palustris
#> 15 2010-11-12 00:00:00      300 Pinus palustris Pinus palustris
#> 16 2010-11-19 00:00:00      300 Pinus palustris Pinus palustris
#> 17 2010-12-05 00:00:00      300 Pinus palustris Pinus palustris
#> 18 2010-12-11 00:00:00      300 Pinus palustris Pinus palustris
#> 19 2010-12-17 00:00:00      300 Pinus palustris Pinus palustris
#> 20 2010-12-30 00:00:00      300 Pinus palustris Pinus palustris
#>    phenophase_name  color
#> 1  Ripe seed cones Green3
#> 2  Ripe seed cones Green3
#> 3  Ripe seed cones Green3
#> 4  Ripe seed cones Green3
#> 5  Ripe seed cones Green3
#> 6  Ripe seed cones Green3
#> 7  Ripe seed cones Green3
#> 8  Ripe seed cones Green3
#> 9  Ripe seed cones Green3
#> 10 Ripe seed cones Green3
#> 11 Ripe seed cones Green3
#> 12 Ripe seed cones Green3
#> 13 Ripe seed cones Green3
#> 14 Ripe seed cones Green3
#> 15 Ripe seed cones Green3
#> 16 Ripe seed cones Green3
#> 17 Ripe seed cones Green3
#> 18 Ripe seed cones Green3
#> 19 Ripe seed cones Green3
#> 20 Ripe seed cones Green3
```

### List stations with xyz

Get a list of all stations which have an individual whom is a member of a set of species.


```r
head( npn_stationswithspp(speciesid = 53) )
#>    latitude  longitude      station_name station_id
#> 1 44.340950 -72.461220  Frizzle Mountain        637
#> 2 42.173855 -85.892418              Home       1447
#> 3 44.588772 -93.004623              home       1572
#> 4 48.051636 -92.766304   Wolfhaunt Creek       1598
#> 5 48.051586 -92.764305 Wolfhaunt Prairie       1599
#> 6 39.973316 -82.802826              Home       1841
```

### Stations by state

Number of stations by state.


```r
head( npn_stationsbystate() )
#>   state number_stations
#> 1    CA            1508
#> 2    AZ             753
#> 3    ME             720
#> 4    VA             720
#> 5    CO             696
#> 6    IL             587
```

### Observations by day

Get observations by day for a particular species or set of species.


```r
library('plyr')
out <- npn_obsspbyday(speciesid=c(357, 359, 1108), startdate='2010-04-01', enddate='2013-09-31')
names(out) <- comnames
#> Error in eval(expr, envir, enclos): object 'comnames' not found
df <- ldply(out)
df$date <- as.Date(df$date)

library('ggplot2')
ggplot(df, aes(date, count)) +
 geom_line() +
 theme_grey(base_size=20) +
 facet_grid(.id ~.)
```

![plot of chunk unnamed-chunk-9](inst/img/unnamed-chunk-9-1.png) 

### Search for species

All species


```r
head( npn_species() )
#> Error in function (type, msg, asError = TRUE) : Avoided giant realloc for header (max is 102400)!
```

By ITIS taxonomic serial number


```r
npn_species_itis(ids = 27806)
#>         common_name  genus species species_id
#> 1 flowering dogwood Cornus florida         12
```

By USNPN id


```r
npn_species_id(ids = 3)
#>   common_name genus species itis_taxonomic_sn
#> 1   red maple  Acer  rubrum             28728
```

By state (and optionally kingdom)


```r
head( npn_species_state(state = "HI", kingdom = "Plantae") )
#>   species_id       common_name        genus        species
#> 1        120      'ohi'a lehua Metrosideros     polymorpha
#> 2        174           alfalfa     Medicago         sativa
#> 3        145    annual ragweed     Ambrosia artemisiifolia
#> 4        124           avocado       Persea      americana
#> 5        898           bayhops      Ipomoea     pes-caprae
#> 6        870 beach strawberry      Fragaria     chiloensis
#>   itis_taxonomic_sn
#> 1             27259
#> 2            183623
#> 3             36496
#> 4             18154
#> 5             30787
#> 6             24625
```

By scientific name


```r
npn_species_sci(genus = "Clintonia", species = "borealis")
#>   common_name itis_taxonomic_sn species_id
#> 1    bluebead             42903          9
```

By common name


```r
npn_species_comm(name = "thickleaved wild strawberry")
#>      genus itis_taxonomic_sn    species species_id
#> 1 Fragaria             24639 virginiana         17
```

Filter by network, group, year, or station


```r
head( npn_species_search(groups = 3, year = 2010) )
#>              common_name        genus        species species_id
#> 1                alfalfa     Medicago         sativa        174
#> 2         annual ragweed     Ambrosia artemisiifolia        145
#> 3 arctic sweet coltsfoot    Petasites       frigidus        434
#> 4              bloodroot  Sanguinaria     canadensis       1016
#> 5               bluebead    Clintonia       borealis          9
#> 6             bluejacket Tradescantia       ohiensis        190
#>   number_observations
#> 1                  33
#> 2                  19
#> 3                  13
#> 4                   1
#> 5                 129
#> 6                  19
```

## Meta

* [Please report any issues or bugs](https://github.com/ropensci/rnpn/issues).
* License: MIT
* Get citation information for `rnpn` in R doing `citation(package = 'rnpn')`

[![](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org)
