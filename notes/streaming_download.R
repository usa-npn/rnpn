library(httr2)
library(tidyverse)

#attempting to replicate this test:
# some_data <- npn_download_status_data(
#   "Unit Test",
#   c(2013),
#   species_ids = c(6)
# )


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

wrangle_resp_chunk <- function(resp) {
  #paste lines into single string
  paste0(resp[nzchar(resp) != 0], collapse = "\n") %>%
    #default to character when mixed numeric and character
    yyjsonr::read_ndjson_str(type = "df", nprobe = -1, promote_num_to_string = TRUE) %>%
    #replace missing data indicator with NA
    dplyr::mutate(dplyr::across(where(is.numeric), \(x) ifelse(x == -9999, NA_real_, x))) %>%
    dplyr::mutate(dplyr::across(where(is.character), \(x) ifelse(x == "-9999", NA_character_, x)))
}

req <- httr2::request(url) |>
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)") %>%
  httr2::req_method("POST") %>%
  httr2::req_body_form(!!!query)

con <- httr2::req_perform_connection(req)

continue <- TRUE
out <- tibble::tibble()
while (isTRUE(continue)) {
  resp <- httr2::resp_stream_lines(con, lines = 5000)
  df_chunk <- wrangle_resp_chunk(resp)
  out <- dplyr::bind_rows(out, df_chunk)
  continue <- length(resp) > 0
}
dim(out)
close(con)



