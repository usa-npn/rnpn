#' Interface to the National Phenology Network API
#'
#' @description Programmatic interface to the
#' Web Service methods provided by the National 'Phenology' Network
#' (<https://usanpn.org/>), which includes data on various life history
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

