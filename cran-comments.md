I have read and agree to the the CRAN policies at
http://cran.r-project.org/web/packages/policies.html


## Test environments

R CMD CHECK passed on local Windows 10 and Ubuntu 18 using R 4.0.2 
Also passed checks on Github Actions on macOS, Windows with R 4.0
and ubuntu 16 using R 3.5.

## R CMD check results

There were no ERRORs or WARNINGs.

There is one note: "Namespace in Imports field not imported from: ‘rgdal’"
While rgdal isn't used directly, the raster package which is directly used
has dependencies on rgdal that will causes tests to fail if the package is
not installed on the system.

## Downstream dependencies

There were no downstream dependencies.

## Other Notes

This version is a response to a test failure that occurred when a remote resource went down. While the package does provide a helpful error message when the resource is not available, the particular test did not properly account for how a negative response from the server is handled, so this change merely fixes that unit test.


Thanks! 
