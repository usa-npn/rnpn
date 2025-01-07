library(httr2)
library(tidyverse)

#attempting to replicate this test:
some_data <- npn_download_status_data(
  "Unit Test",
  c(2013),
  species_ids = c(6)
)


url <- "https://services.usanpn.org/npn_portal/observations/getObservations.ndjson"

#this gets put into the query with curl::handle_setform()—httr2 equivalent is req_body_form()
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
  resp <- resp_stream_lines(con, lines = 5000)
  continue <- length(resp) > 0

  if (continue) {
    df_chunk <-
      #paste lines into single string
      paste0(resp[nzchar(resp) != 0], collapse = "\n") %>%
      #default to character when mixed numeric and character
      yyjsonr::read_ndjson_str(type = "df", nprobe = -1, promote_num_to_string = TRUE) %>%
      #replace missing data indicator with NA
      mutate(across(where(is.numeric), \(x) ifelse(x == -9999, NA_real_, x)))
    out <- dplyr::bind_rows(out, df_chunk)
  }
}
dim(out)
close(con)
