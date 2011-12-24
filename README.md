# `rnpn`

`rnpn` is a set of functions/package is an R interface to the US National Phenology Network API. 

National Phenology Network API documentation here: 
https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit?hl=en_US

Note that there is no need for an API key to grab data from the National Phenology Network, but I think there is for writing data through the API. Currently, functions in this package only allow getting data, but will soon allow posting data to the USNPN endpoints.

Note to Windows users when installing using install_github in Hadley's devtools package:

* Rtools is required, and can be installed from this site (http://www.murdoch-sutherland.com/Rtools/).  After installation the following should install `rnpn`:

```R 
install.packages("devtools")
require(devtools)
install_github("rnpn", "ropensci")
require(rnpn)
```