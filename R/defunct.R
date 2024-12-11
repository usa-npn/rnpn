#' This function is defunct.
#' @export
#' @rdname npn_obsspbyday-defunct
#' @keywords internal
npn_obsspbyday <- function(...) {
  .Defunct(package = "rnpn",
           msg = "the npn_obsspbyday() function no longer works, removed. Try using npn_download_status_data instead.")
}

#' This function is defunct.
#' @export
#' @rdname npn_allobssp-defunct
#' @keywords internal
npn_allobssp <- function(...) {
  .Defunct(package = "rnpn",
           msg = "the npn_allobssp() function no longer works, removed. Try using npn_download_status_data instead.")
}

#' This function is defunct.
#' @export
#' @rdname npn_indspatstations-defunct
#' @keywords internal
npn_indspatstations <- function(...) {
  .Defunct(package = "rnpn",
           msg = "the npn_indspatstations() function no longer works, removed. Try using npn_download_status_data instead.")
}

#' This function is defunct.
#' @export
#' @rdname npn_indsatstations-defunct
#' @keywords internal
npn_indsatstations <- function(...) {
  .Defunct(package = "rnpn",
           msg = "the npn_indsatstations() function no longer works, removed. Try using npn_download_status_data instead.")
}

#' This function renamed to be consistent with other package function names.
#' @export
#' @rdname npn_stationsbystate-defunct
#' @keywords internal
npn_stationsbystate <- function (...){
  .Defunct(package = "rnpn",
    msg = "the npn_stationsbystate() function no longer works - renamed. Use npn_stations_by_state instead.")
}


#' This function renamed to be consistent with other package function names.
#' @export
#' @rdname npn_stationswithspp-defunct
#' @keywords internal
npn_stationswithspp <- function (...){
  .Defunct(package = "rnpn",
           msg = "the npn_stationswithspp() function no longer works - renamed. Use npn_stations_with_spp instead.")
}



#' Defunct functions in rnpn
#'
#' \itemize{
#'  \item [npn_obsspbyday()]: Removed.
#'  \item [npn_allobssp()]: Removed.
#'  \item [npn_indspatstations()]: Removed.
#'  \item [npn_indsatstations()]: Removed.
#'  \item [npn_stationsbystate()]: Removed.
#'  \item [npn_stationswithspp()]: Removed.
#' }
#'
#' @name rnpn-defunct
NULL
