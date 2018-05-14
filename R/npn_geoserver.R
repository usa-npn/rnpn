
#' @export
npn_download_geospatial <- function (
  coverage_id,
  date,
  format = "geotiff",
  output_path = NULL
){

  z = NULL

  if(is.null(output_path)){
    z <- tempfile()
  }

  url <- paste(base_geoserver(), "coverageId=", coverage_id, "&SUBSET=time(\"", date, "T00:00:00.000Z\")&format=", format, sep="")
  print (url)
  if(is.null(output_path)){
    download.file(url,z,method="libcurl", mode="wb")

    ras <- raster::raster(z)


  }else{
    download.file(url,destfile=output_path,method="libcurl", mode="wb")
  }

}

# Checks in the global variable "point values"
# to see if the exact data point being requested
# has already been asked for and returns the value
# if it's already saved.
npn_check_point_cached <- function(
  layer,lat,long,date
){
  val = NULL
  if(exists("point_values")){
    val <- point_values[point_values$layer == layer & point_values$lat == lat & point_values$long == long & point_values$date == date,]['value']
    if(!is.null(val) && nrow(val) == 0){
      val <- NULL
    }
  }
  return(val)
}



# This function is for requested AGDD point values. Because the NPN has a separate
# data service that can provide AGDD values which is more accurate than Geoserver
# this function is ideal when requested point AGDD point values.
#' @export
npn_get_agdd_point_data <- function(
  layer,
  lat,
  long,
  date,
  store_data=TRUE){

  # If we already have this value stored in global memory then
  # pull it from there.
  cached_value <- npn_check_point_cached(layer,lat,long,date)
  if(!is.null(cached_value)){
    return(cached_value)
  }

  url <- paste0(base(), "stations/getTimeSeries.json?latitude=", lat, "&longitude=", long, "&start_date=", as.Date(date) - 1, "&end_date=", date, "&layer=", layer)
  data = httr::GET(url,
                   query = list(),
                   httr::progress())


  json_data <- tryCatch({
    jsonlite::fromJSON(httr::content(data, as = "text"))
  },error=function(msg){
    print(paste("Failed:", url))
    return(-9999)
  })

  v <- tryCatch({
    as.numeric(json_data[json_data$date==date,"point_value"])
  },error=function(msg){
    print(paste("Failed:", url))
    return(-9999)
  })

  # Once the value is known, then cache it in global memory so the script doesn't try to ask for the save
  # data point more than once.
  #
  # TODO: Break this into it's own function
  if(store_data){
    if(!exists("point_values")){
      point_values <<- data.frame(layer=layer,lat=lat,long=long,date=date,value=v)
    }else{
      point_values <<- rbind(point_values, data.frame(layer=layer,lat=lat,long=long,date=date,value=v))
    }
  }

  return(v)

}






# This function can get point data about any layer, not just AGDD layers. It pulls this from
# the NPN's WCS service so the data may not be totally precise.
#' @export
npn_get_point_data <- function(
  layer,
  lat,
  long,
  date,
  store_data=TRUE){

  cached_value <- npn_check_point_cached(layer,lat,long,date)
  if(!is.null(cached_value)){
    return(cached_value)
  }

  url <- paste0(base_geoserver(), "coverageId=",layer,"&format=application/gml+xml&subset=http://www.opengis.net/def/axis/OGC/0/Long(",long,")&subset=http://www.opengis.net/def/axis/OGC/0/Lat(",lat,")&subset=http://www.opengis.net/def/axis/OGC/0/time(\"",date,"T00:00:00.000Z\")")
  data = httr::GET(url,
                   query = list(),
                   httr::progress())
  #Download the data as XML and store it as an XML doc
  xml_data <- httr::content(data, as = "text")
  doc <- XML::xmlInternalTreeParse(xml_data)

  df <- XML::xmlToDataFrame(XML::xpathApply(doc, "//gml:RectifiedGridCoverage/gml:rangeSet/gml:DataBlock/tupleList"))

  v <- as.numeric(as.list(strsplit(gsub("\n","",df[1,"text"]),' ')[[1]])[1])

  if(store_data){
    if(!exists("point_values")){
      point_values <<- data.frame(layer=layer,lat=lat,long=long,date=date,value=v)
    }else{
      point_values <<- rbind(point_values, data.frame(layer=layer,lat=lat,long=long,date=date,value=v))
    }
  }

  return(v)

}



npn_merge_geo_data <- function(
  raster,
  col_label,
  df
){

  coords <- data.frame(lon=df[,"longitude"],lat=df[,"latitude"])
  sp::coordinates(coords)

  values <- raster::extract(x=raster,y=coords)

  df <- cbind(df,values)
  names(df)[names(df) == "values"] <- col_label

  return(df)
}


resolve_agdd_raster <- function(
  agdd_layer
){

  if(!is.null(agdd_layer)){
    if(agdd_layer == 32){
      agdd_layer <- "gdd:agdd"
    }else if(agdd_layer == 50){
      agdd_layer <- "gdd:agdd_50f"
    }
  }

}


resolve_six_raster <- function(
  year,
  phenophase = "leaf",
  sub_model = NULL
){
  current_year <- as.numeric(format(Sys.Date(), '%Y'))
  num_year <- as.numeric(year)
  src <- NULL
  date <- NULL

  if(num_year < current_year - 1){
    src <- "prism"
    date <- paste0(year,"-01-01")
  }else{
    src <- "ncep"
    if(num_year != current_year){
      date <- paste0(year,"-12-29")
    }else{
      date <- Sys.Date()
    }
  }

  if(is.null(sub_model)){
    sub_model = "average"
  }

  if(is.null(phenophase) || (phenophase != 'leaf'  && phenophase != 'bloom')){
    phenophase = 'leaf'
  }

  layer_name = paste0("si-x:", sub_model, "_", phenophase, "_", src)

  raster <- npn_download_geospatial(layer_name, date,"tiff")
}



