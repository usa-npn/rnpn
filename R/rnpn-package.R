#' Interface to the National Phenology Network API
#'
#' @description This package allows for easy access to the National Phenology Network's Data API. To learn more, take a look at the vignettes.
#' events that occur at specific times.
#' @importFrom stats setNames
#' @importFrom data.table rbindlist setDF
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom plyr llply ldply ddply summarise rbind.fill
#' @importFrom magrittr "%>%"
#' @importFrom utils URLencode download.file object.size write.table
#' @name rnpn-package
#' @aliases rnpn
#' @docType package
#' @keywords package
NULL

