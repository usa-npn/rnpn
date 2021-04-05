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

This version is a response to a test failure that occurred when a remote resource went down. While the package does provide a helpful error message when the resource is not available, the particular test did not properly account for how a negative response from the server is handled, so this change merely fixes that unit test.

This is the second release of this package recently. My understanding of events are this: 
1) On 3/14 I received an email from CRAN letting me know of a failed test and the risk of having the package revoked on 3/29. This issue was on account of an outage on our development network
2) The issue was fixed and version 1.2.0 of this package was submitted and accepted on 3/22
3) Sometime after 3/22 and before 3/29 another development system suffered an outage which caused a different test to fail for a different reason. No subsequent warning email was sent to me because it was within the same window of time as the first warning.
4) I am now submitting this fix on 4/5 which presumably should fix all outstanding issues.


Thanks! 
