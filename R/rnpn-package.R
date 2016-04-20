#' Interface to the National Phenology Network API
#'
#' @importFrom data.table rbindlist setDF
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom plyr llply ldply ddply summarise rbind.fill
#' @importFrom stringr str_replace
#' @name rnpn-package
#' @aliases rnpn
#' @docType package
#' @keywords package
NULL

#' Get data from the US National Phenology Network.
#'
#' Lookup-table for IDs of species and common names
#' @name taxonlist
#' @docType data
#' @keywords data
NULL
