#base url for NPN Portal
base_portal_url <- "https://services.usanpn.org/npn_portal/"
#base url for geoserver
base_geoserver_url <- "http://geoserver.usanpn.org/geoserver/"
#base url for geoservices
base_geoservices_url <- "https://services.usanpn.org/geo-services/v1/"

#TODO consider adding retry and rate-limiting, maybe caching?
base_req <-
  httr2::request(base_portal_url) %>%
  httr2::req_retry(max_tries = 3) %>%
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")

base_req_geoserver <-
  httr2::request(base_geoserver_url) %>%
  httr2::req_retry(max_tries = 3) %>%
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")

base_req_geoservices <-
  httr2::request(base_geoservices_url) %>%
  httr2::req_retry(max_tries = 3) %>%
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")
