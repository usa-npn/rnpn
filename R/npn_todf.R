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
