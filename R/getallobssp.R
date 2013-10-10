#' Get all observations for a particular species or set of species.
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom plyr compact
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param callopts Optional additional curl options (debugging tools mostly)
#' @return An object of class npn with slots for taxa, stations, phenophase (metadata), 
#'    and data.
#' @export
#' @seealso \code{\link{npn_todf}}
#' @examples \dontrun{
#' # Lookup names
#' lookup_names(name='Pinus', type='genus')
#' 
#' # Get data on one species
#' getallobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31')
#' 
#' # Get data on two species
#' getallobssp(speciesid = c(52, 53), startdate='2008-01-01', enddate='2011-12-31')
#' 
#' # Get data on one species, convert to X
#' out <- getallobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31')
#' npn_todf(out)
#' }
getallobssp <- function(speciesid = NULL, startdate = NULL, enddate = NULL, callopts=list())
{
  if(is.null(speciesid))
    stop("You must provide a speciesid")

  taxa <- taxonlist[as.numeric(as.character(taxonlist[,"species_id"])) %in% speciesid,c("species_id","genus","species")]
  taxa$species_id <- as.numeric(as.character(taxa$species_id))
  
  url = 'https://www.usanpn.org/npn_portal/observations/getAllObservationsForSpecies.json'
  args <- compact(list(start_date=startdate, end_date=enddate))
  for(i in seq_along(speciesid)){
    args[paste('species_id[',i,']',sep='')] <- speciesid[i]
  }
  out <- GET(url, query=args, callopts)
  stop_for_status(out)
  tt <- content(out)
  station_list <- data.frame(do.call(rbind, tt$station_list))
  phenophase_list <- data.frame(do.call(rbind, tt$phenophase_list))
  foo <- function(x){
    tmp <- list(date=x$date, 
                station_id=x$stations[[1]]$station_id,
                species_id=x$stations[[1]]$species_ids[[1]]$species_id,
                phenophase=data.frame(do.call(rbind, x$stations[[1]]$species_ids[[1]]$phenophases)))
    do.call(data.frame, tmp)
  }
  temp <- lapply(tt$observation_list, foo)
  data <- do.call(rbind, temp)
  names(data)[4:5] <- c("phenophase_id","phen_seq")
  
  new("npn", taxa=taxa, stations=station_list, phenophase = phenophase_list, data = data)
}

setClass("npn", slots=list(taxa="data.frame",
                           stations="data.frame", 
                           phenophase="data.frame", 
                           data="data.frame"))

#' Coerce elements of an object of class npn to a single data.frame
#' 
#' @param x An object of class npn
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
npn_todf <- function(input, minimal=FALSE)
{
  if(!is(input,"npn"))
    stop("Input object must be of class npn")
  
  dat <- merge(input@stations, input@data, by="station_id")
  dat <- merge(dat, input@taxa, by="species_id")[,-c(1,2)]
  dat <- merge(dat, input@phenophase, by="phenophase_id")[,-1]
  dat <- transform(dat, sciname = paste(genus, species, sep=" "))
  dat <- data.frame(sciname=dat$sciname, latitude=dat$latitude, longitude=dat$longitude, 
               dat[,!names(dat)%in%c("sciname","latitude","longitude")])
  if(minimal)
    dat <- dat[,c("sciname","latitude","longitude")]
  
  new("npnsp", data=dat)
}

setClass("npnsp", slots=list(data="data.frame"))