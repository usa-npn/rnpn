


#' Get Partner Groups
#'
#' Returns a list of all groups participating in the NPN's data collection program. These details can be used to further filter
#' other service endpoints' results.
#'
#' @param use_hierarchy Boolean indicating whether or not the list of networks should be represented in a hierarchy. Defaults to FALSE
#' @template curl
#'
#' @return List of parnter groups, including ID and name
#' @export
npn_groups <- function(use_hierarchy=FALSE, ...) {
  end_point <- NULL

  if(use_hierarchy){
    end_point <- 'networks/getNetworkTree.json'
  }else{
    end_point <- 'networks/getPartnerNetworks.json'
  }

  tibble::as_data_frame(
    npn_GET(paste0(base(), end_point), list(), TRUE, ...)
  )

}
