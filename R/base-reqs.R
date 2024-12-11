#base url for NPN Portal
base_portal_url <- "https://services.usanpn.org/npn_portal/"
base_geoserver_url <- "http://geoserver.usanpn.org/geoserver/"
#question for Jeff: what to call this one? Where is the documentation?
# "https://services.usanpn.org/geo-services/v1/"


#TODO consider adding retry and rate-limiting, maybe caching?
base_req <-
  httr2::request(base_portal_url) |>
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")


base_req_geoserver <-
  httr2::request(base_geoserver_url) %>%
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")

