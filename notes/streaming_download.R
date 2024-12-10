library(httr2)
library(tidyverse)
url <- "https://services.usanpn.org/npn_portal/observations/getObservations.ndjson"

query <- list(request_src = "Unit Test", climate_data = 0, species_id = 6, start_date = "2013-01-01", end_date = "2013-12-31")

req <- request(url) |>
  req_url_query(!!!query)

con <- req_perform_connection(req)

continue <- TRUE
out <- tibble::tibble()
while(isTRUE(continue)) {
  resp <- resp_stream_lines(con, lines = 5000)
  continue <- length(resp) > 0

  if (continue) {
    df_line <-
      yyjsonr::read_ndjson_str(resp, type = "df") |>
      mutate(across(everything(), \(x) ifelse(x == -9999, NA, x)))



    out <- dplyr::bind_rows(out, df_line)
  }
}
length(out)
close(con)

out |> View()


show_bytes <- function(x) {
  cat("Got ", length(x), " bytes\n", sep = "")
  TRUE
}

resp <- req_perform_stream(req, callback = \(x) rawToChar(x), buffer_kb = 8000)
resp <- req_perform(req)
