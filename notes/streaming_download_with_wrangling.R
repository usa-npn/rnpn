load_all()
url <- "https://services.usanpn.org/npn_portal//observations/getObservations.ndjson?"
query <- list(request_src = "Unit%20Test", climate_data = "0", `species_id[1]` = "6",
              start_date = "2016-01-01", end_date = "2016-12-31")
# download_path <- NULL #but also try with a path
download_path <- "test.csv"
always_append <- FALSE
six_leaf_raster <- terra::rast("notes/six_leaf_raster.tiff")
six_bloom_raster <- terra::rast("notes/six_bloom_raster.tiff")
agdd_layer <- "gdd:agdd"
additional_layers <- structure(list(name = "si-x:30yr_avg_4k_leaf", param = "365",
                                    raster = list(terra::rast("notes/additional_layer.tiff"))), row.names = c(NA, -1L), class = "data.frame")

check <- npn_get_data(
  url = url,
  query = query,
  download_path = download_path,
  always_append = always_append,
  six_leaf_raster = six_leaf_raster,
  six_bloom_raster = six_bloom_raster,
  agdd_layer = agdd_layer,
  additional_layers = additional_layers
)


req <- httr2::request(url) %>%
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)") %>%
  httr2::req_method("POST") %>%
  httr2::req_body_form(!!!query)

con <- httr2::req_perform_connection(req)
on.exit(close(con), add = TRUE)

continue <- TRUE
dtm <- tibble::tibble()
i <- 0
while (isTRUE(continue)) {
  resp <- httr2::resp_stream_lines(con, lines = 5000)
  continue <- length(resp) > 0

  if (isFALSE(continue)) break

  df <-
    #paste lines into single string
    paste0(resp[nzchar(resp) != 0], collapse = "\n") %>%
    #default to character when mixed numeric and character
    yyjsonr::read_ndjson_str(type = "df", nprobe = -1, promote_num_to_string = TRUE) %>%
    tibble::as_tibble() %>%
    #replace missing data indicator with NA
    dplyr::mutate(dplyr::across(where(is.numeric), function(x) ifelse(x == -9999, NA_real_, x))) %>%
    dplyr::mutate(dplyr::across(where(is.character), function(x) ifelse(x == "-9999", NA_character_, x)))

  # Reconcile all the points in the frame with the SIX leaf raster,
  # if it's been requested.

  if (!is.null(six_leaf_raster)) {
    df <- npn_merge_geo_data(six_leaf_raster, "SI-x_Leaf_Value", df)
  }

  # Reconcile all the points in the frame with the SIX bloom raster,
  # if it's been requested.
  if (!is.null(six_bloom_raster)) {
    df <- npn_merge_geo_data(six_bloom_raster, "SI-x_Bloom_Value", df)
  }

  if (!is.null(additional_layers)) {
    for (j in rownames(additional_layers)) {
      df <- npn_merge_geo_data(
        additional_layers[j, ][['raster']][[1]],
        as.character(additional_layers[j, ][['name']][[1]]),
        df
      )
    }
  }

  # Reconcile the AGDD point values with the data points if that
  # was requested.
  if (!is.null(agdd_layer)) {
    date_col <- NULL

    if ("observation_date" %in% colnames(df)) {
      date_col <- "observation_date"
    } else if ("mean_first_yes_doy" %in% colnames(df)) {
      df$cal_date <-
        as.Date(df[, "mean_first_yes_doy"],
                origin = paste0(df[, "mean_first_yes_year"], "-01-01")) - 1
      date_col <- "cal_date"
    } else if ("first_yes_day" %in% colnames(df)) {
      df$cal_date <-
        as.Date(df[, "first_yes_doy"],
                origin = paste0(df[, "first_yes_year"], "-01-01")) - 1
      date_col <- "cal_date"
    }

    pt_values <-
      apply(df[, c('latitude', 'longitude', date_col)], 1,
            function(x) {
              rnpn::npn_get_agdd_point_data(
                layer = agdd_layer,
                lat = as.numeric(x['latitude']),
                long = as.numeric(x['longitude']),
                date = x[date_col]
              )
            })

    pt_values <- t(as.data.frame(pt_values))
    colnames(pt_values) <- agdd_layer
    df <- cbind(df, pt_values)

    if ("cal_date" %in% colnames(df)) {
      df$cal_date <- NULL
    }
  }

  if (is.null(download_path)) {
    dtm <- dplyr::bind_rows(dtm, df)
  } else {
    if (nrow(df) > 0) {
      write.table(
        df,
        download_path,
        append = !(i == 0 && isFALSE(always_append)),
        sep = ",",
        eol = "\n",
        row.names = FALSE,
        col.names = i == 0 && isFALSE(always_append)
      )
    }
  }
  i <- i + 1
}
#return
dtm
#or file path
download_path
