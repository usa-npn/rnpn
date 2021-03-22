I have read and agree to the the CRAN policies at
http://cran.r-project.org/web/packages/policies.html

## Resubmission

This is a re-submission. In this iteration I am providing additional unit tests and remote resource checks to provide graceful fails when remote resources are unavailble. This is in addition to providing that functionality for the tests that were already failing in 1.1.1.


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

This version is a response to a test failure that occurred when a remote resource went down. It became apparent that the package didn't gracefully fail in these cases. This release adds actual graceful failing mechanisms for end users and prevents the tests from outright failing when the resource is down.


Thanks! 
