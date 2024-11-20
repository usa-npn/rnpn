#' Interface to the National Phenology Network API
#'
#' @description This package allows for easy access to the National Phenology Network's Data API. To learn more, take a look at the vignettes.
#' @importFrom data.table rbindlist
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom plyr rbind.fill
#' @importFrom magrittr "%>%"
#' @importFrom utils URLencode download.file object.size write.table
#' @name rnpn-package
#' @aliases rnpn
#' @keywords package
"_PACKAGE"

