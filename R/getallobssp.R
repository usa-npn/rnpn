#' Get all observations for a particular species or set of species.
#'
#' @import RJSONIO RCurl stringr plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Date when article was published.
#' @export
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
#   taxa <- split(taxa, taxa$species_id)
#   taxa <- lapply(taxa, function(x) list(species_id=as.numeric(as.character(x$species_id)), name=paste(x$genus,x$species,sep=" ")))
  
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

#' Coerce elements of output from a call to occ to a single data.frame
#' @param x An object of class occdat
#' @param minimal Default is FALSE
#' @return An object of class npnsp (for npn spatial)
#' @export
npn_todf <- function(x, minimal=FALSE)
{
  if(!is(x,"npn"))
    stop("Input object must be of class npn")
  
  dat <- merge(x@stations, x@data, by="station_id")
  dat <- merge(dat, x@taxa, by="species_id")[,-c(1,2)]
  dat <- merge(dat, x@phenophase, by="phenophase_id")[,-1]
  dat <- transform(dat, sciname = paste(genus, species, sep=" "))
  dat <- data.frame(sciname=dat$sciname, latitude=dat$latitude, longitude=dat$longitude, 
               dat[,!names(dat)%in%c("sciname","latitude","longitude")])
  if(minimal)
    dat <- dat[,c("sciname","latitude","longitude")]
  
  new("npnsp", data=dat)
}

setClass("npnsp", slots=list(data="data.frame"))
