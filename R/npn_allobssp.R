#' Get all observations for a particular species or set of species.
#'
#' @export
#'
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param ... Optional additional curl options (debugging tools mostly)
#' @return An S4 object of class npn with slots for taxa, stations, phenophase (metadata),
#'    and data.
#'
#' @examples \donttest{
#' # Lookup names
#' lookup_names(name='Pinus', type='genus')
#'
#' # Get data on one species
#' npn_allobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31')
#'
#' # Get data on two species
#' npn_allobssp(speciesid = c(52, 53), startdate='2008-01-01', enddate='2011-12-31')
#'
#' # Get data on one species, convert to a single data.frame
#' out <- npn_allobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31')
#' npn_todf(out)
#' }

npn_allobssp <- function(speciesid, startdate = NULL, enddate = NULL, ...)
{
  taxa <- taxonlist[as.numeric(as.character(taxonlist[,"species_id"])) %in% speciesid,c("species_id","genus","epithet","genus_epithet")]
  taxa$species_id <- as.numeric(as.character(taxa$species_id))

  args <- npnc(list(start_date = startdate, end_date = enddate))
  for (i in seq_along(speciesid)) {
    args[paste('species_id[',i,']',sep = '')] <- speciesid[i]
  }
  tt <- npn_GET(paste0(base(), 'observations/getAllObservationsForSpecies.json'), args, ...)
  station_list <- data.frame(rbindlist(lapply(tt$station_list, data.frame), fill = TRUE))
  phenophase_list <- lapply(tt$phenophase_list, function(x){ x[sapply(x, is.null)] <- "none"; x})
  phenophase_list <- data.frame(rbindlist(lapply(phenophase_list, data.frame), fill = TRUE))
  foo <- function(x){
    tmp <- list(date = x$date,
                station_id = x$stations[[1]]$station_id,
                species_id = x$stations[[1]]$species_ids[[1]]$species_id,
#                 phenophase=ldply(x$stations[[1]]$species_ids[[1]]$phenophases, data.frame),
                phenophase_id = x$stations[[1]]$species_ids[[1]]$phenophases[[1]][[1]],
                phen_seq = x$stations[[1]]$species_ids[[1]]$phenophases[[1]][[2]])
    data.frame(tmp)
  }
  temp <- lapply(tt$observation_list, foo)
  data <- data.frame(do.call(rbind, temp))
#   names(data)[4:5] <- c("phenophase_id","phen_seq")

  new("npn", taxa = taxa, stations = station_list, phenophase = phenophase_list, data = data)
}

setClass("npn", slots = list(taxa = "data.frame",
                           stations = "data.frame",
                           phenophase = "data.frame",
                           data = "data.frame"))

#' Coerce elements of an object of class npn to a single data.frame
#'
#' @param input An object of class npn
#' @param minimal Default is FALSE
#' @return An object of class npnsp (for npn spatial), containing just a data.frame
#'    with the following fields if minimal=TRUE
#' \enumerate{
#'   \item sciname
#'   \item latitude
#'   \item longitude
#' }
#'
#' and the following fields if minimal=FALSE
#' \enumerate{
#'   \item sciname
#'   \item latitude
#'   \item longitude
#'   \item asdfas
#'   \item asdfdh
#'   \item asdfawer
#'   \item asdfssawe
#'   \item asdfad
#'   \item asdfwe
#'   \item adsfcwer
#' }
#' @export
npn_todf <- function(input, minimal=FALSE) {
  if (!is(input,"npn")) {
    stop("Input object must be of class npn", call. = FALSE)
  }

  dat <- merge(input@stations, input@data, by = "station_id")
  dat <- merge(dat, input@taxa, by = "species_id")[,-c(1, 2)]
  dat <- merge(dat, input@phenophase, by = "phenophase_id")[,-1]
  dat <- data.frame(sciname = dat$genus_epithet, latitude = dat$latitude, longitude = dat$longitude,
               dat[,!names(dat) %in% c("sciname","latitude","longitude")])
  if (minimal) {
    dat <- dat[,c("sciname","latitude","longitude")]
  }

  new("npnsp", data = dat)
}

setClass("npnsp", slots = list(data = "data.frame"))


addmissing <- function(x){
  names_ <- names(x[[which.max(sapply(x, length))]])

  bbb <- function(x){
    if (identical(names_[!names_ %in% names(x)], character(0))) {
      x
    } else {
      xx <- rep("na", length(names_[!names_ %in% names(x)]))
      names(xx) <- names_[!names_ %in% names(x)]
      c(x, xx)
    }
  }
  lapply(x, bbb)
}
