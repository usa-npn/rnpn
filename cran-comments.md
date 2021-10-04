I have read and agree to the the CRAN policies at
http://cran.r-project.org/web/packages/policies.html

## Resubmission

This is a resubmission to address a NOTE pertaining to an usused
LazyData directive in the DESCRIPTION


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

This version updates the maintainer from Lee Marsh to Alyssa Rosemartin and fixes a unit test failure issue that occurs when a remote resource is down.

Thanks! 
