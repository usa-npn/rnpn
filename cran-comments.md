I have read and agree to the the CRAN policies at
http://cran.r-project.org/web/packages/policies.html

## Resubmission

This is a resubmission to address a ERROR related to the tests on linux environments.

## Test environments

rhub::check_for_cran passed.

## R CMD check results

There were no ERRORs or WARNINGs.

There is one note: "Namespace in Imports field not imported from: ‘rgdal’"
While rgdal isn't used directly, the raster package which is directly used
has dependencies on rgdal that will causes tests to fail if the package is
not installed on the system.

## Downstream dependencies

There were no downstream dependencies.

## Other Notes

I am not able to reproduce the recent CRAN failures using rhub (see https://github.com/r-hub/rhub/issues/489). In this version I skipped tests reliant on geoserver API calls using skip_on_cran. We will continue to run all tests in multiple environments using other tools.

Thanks! 
