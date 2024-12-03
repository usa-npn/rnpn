library(httr2)

# Requests are composable.  For example, you could set a bunch of options in a base request used by many other functions
base_req <-
  request("https://services.usanpn.org/npn_portal/") |>
  req_retry(max_tries = 3, retry_on_failure = TRUE) |> #retry on errors
  req_progress(type = "down") |> #display progress bar for large downloads
  req_throttle(rate = 30/60) |> #limit request rate to 30 requests per minute
  req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")

base_req

# Then build on the base request by adding specific endpoints and queries
req_species_by_id <-
  base_req |>
  req_url_path_append("species", "getSpeciesById.json") |>
  req_url_query(species_id = 3)

req_species_by_id

# Perform the request
resp <- req_perform(req_species_by_id)
resp

# And get the body of the response as a list
result <-
  resp |>
  resp_body_json()

result
as.data.frame(result)
