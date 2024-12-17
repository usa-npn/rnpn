library(httr2)
library(tidyverse)

#attempting to replicate this test:
some_data <- npn_download_status_data(
  "Unit Test",
  c(2013),
  species_ids = c(6)
)


url <- "https://services.usanpn.org/npn_portal/observations/getObservations.ndjson"

#this gets put into the query with curl::handle_setform()—I'm not sure what that does
query <- list(request_src = "Unit%20Test", climate_data = "0", `species_id[1]` = "6",
              start_date = "2013-01-01", end_date = "2013-12-31", customrequest = "POST")
download_path <- NULL
always_append <- FALSE
six_leaf_raster <- NULL
six_bloom_raster <- NULL
agdd_layer <- NULL
additional_layers <- NULL

req <- request(url) |>
  req_method("POST") %>%
  req_body_form(!!!query)

con <- req_perform_connection(req)

continue <- TRUE
out <- tibble::tibble()
while(isTRUE(continue)) {
  resp <- resp_stream_lines(con, lines = 5000) #works if I stream 1 line at a time, but not 10
  continue <- length(resp) > 0

  if (continue) {
    df_line <-
      purrr::map(resp, \(resp_line) {
        yyjsonr::read_ndjson_str(resp_line, type = "df") %>%
          mutate(across(everything(), \(x) ifelse (x == -9999, NA, x)))
      }) %>% purrr::list_rbind()

    out <- dplyr::bind_rows(out, df_line)
  }
}
dim(out)
close(con)
