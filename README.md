rnpn
========

`rnpn` is a set of functions/package is an R interface to the US National Phenology Network API. 

National Phenology Network API documentation here: 
https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit?hl=en_US

Note that there is no need for an API key to grab data from the National Phenology Network, but I think there is for writing data through the API. Currently, functions in this package only allow getting data, but will soon allow posting data to the USNPN endpoints.

Note to Windows users when installing using install_github in Hadley's devtools package:

* Rtools is required, and can be installed from this site (http://www.murdoch-sutherland.com/Rtools/).  After installation the following should install `rnpn`:

### Install

```coffee
install.packages("devtools")
require(devtools)
install_github("rnpn", "ropensci")
require(rnpn)
```

### Quick start

You can lookup taxon names. This is not actually an API call to the web. The function simply searches for matches in a dataset stored in the package. You can then use the speciesid output in other functions.

```coffee
lookup_names(name='Pinus', type='genus')
```

```coffee
    species_id                  common_name genus    species itis_taxonomic_sn
82         967                  Bishop pine Pinus   muricata            183359
312         53           eastern white pine Pinus    strobus            183385
370        219 Great Basin bristlecone pine Pinus   longaeva            183352
458        220                  limber pine Pinus   flexilis            183343
461         54                loblolly pine Pinus      taeda             18037
462        762               lodgepole pine Pinus   contorta            183327
465         52                longleaf pine Pinus  palustris             18038
479        965               Mexican pinyon Pinus cembroides            183321
584         25               ponderosa pine Pinus  ponderosa            183365
618        968                     red pine Pinus   resinosa            183375
698         51            singleleaf pinyon Pinus monophylla            183353
704        295                   slash pine Pinus  elliottii             18036
794         50             twoneedle pinyon Pinus     edulis            183336
836        966           western white pine Pinus  monticola            183356
```

Search for a single species, specifying a start and end date. You can also pass a vector to the speciesid parameter.

```coffee
(out <- getallobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31'))
```

```coffee
An object of class "npn"
Slot "taxa":
    species_id genus   species
465         52 Pinus palustris

Slot "stations":
  station_id                                  station_name  latitude  longitude
1       4881                        Possum Branch Preserve 28.045185 -82.706299
2       5470                                            11 34.852619 -82.394012
3       5758 University of South Florida Botanical Gardens 28.057789 -82.424065
4       5116                                             9 34.928726 -79.782715
5       6162                                          home 29.947870 -90.119652

Slot "phenophase":
  phenophase_id          phenophase_name  color
1           393          Ripe seed cones Green3
2           503           Pollen release Green2
3           496         Emerging needles Green1
4           486            Young needles Green1
5           490             Pollen cones Green2
6           495        Open pollen cones Green2
7           392        Unripe seed cones Green3
8           491 Recent cone or seed drop Green3

Slot "data":
                   date station_id species_id phenophase_id phen_seq
1   2009-09-03 00:00:00       4881         52           393      300
2   2009-09-10 00:00:00       4881         52           393      300
3   2010-07-30 00:00:00       4881         52           393      300
4   2010-08-13 00:00:00       4881         52           393      300
5   2010-08-20 00:00:00       4881         52           393      300
6   2010-09-03 00:00:00       4881         52           393      300
7   2010-09-10 00:00:00       4881         52           393      300
8   2010-09-17 00:00:00       4881         52           393      300
9   2010-09-24 00:00:00       4881         52           393      300
10  2010-10-08 00:00:00       4881         52           393      300
11  2010-10-15 00:00:00       4881         52           393      300
```

Coerce data to a data.frame that has most all data. 

```coffee
npn_todf(out)
```

```coffee
some stuff...
```