##
##  Get geolocation info
##
##  Created by Daniel Rodríguez Pérez on 21/7/2013.
##  Copyright (c) 2013 Daniel Rodríguez Pérez. All rights reserved.
##

#' Get location
#' 
#' Returns the location of an IP using. If an IP is indicated the function
#' returns it's estimated location, otherwise it will use the IP of the
#' system.
#' 
#' @param ip an IP address for geolocation (optional default uses the computer)
#' 
#' @usage getLocation(ip = getExternalIP())
#' 
#' @examples
#' # Get computer location
#' location <- getLocation()
#' 
#' # Get '10.10.10.10' location
#' location <- getLocation('10.10.10.10')
#' 
#' @rdname getLocation
#' @export getLocation
#' @aliases getLocation
getLocation <- function(ip = getExternalIP()) {
  return(getLocation.freegeoip(ip))
}

# Returns the location of an IP using the URL
getLocation.url <- function(url, ip) {
  if (is.null(ip)) {
    result <- NULL
  } else {
    url    <- paste(url, ip, sep = '')
    result <- fromJSON(readLines(url, warn=FALSE))
  }
  
  return(result)
}

#' @usage getLocation.freegeoip(ip = getExternalIP())
#' 
#' @rdname getLocation
#' @export getLocation.freegeoip
#' @aliases getLocation.freegeoip
getLocation.freegeoip <- function(ip = getExternalIP()) {
  result <- getLocation.url('http://freegeoip.net/json/', ip)
  return(result)
}

#' @usage getLocation.hostip(ip = getExternalIP())
#' 
#' @rdname getLocation
#' @export getLocation.hostip
#' @aliases getLocation.hostip
getLocation.hostip <- function(ip = getExternalIP()) {
  result <- getLocation.url('http://api.hostip.info/get_json.php?position=true&ip=', ip)
  return(result)
}

#' Resolve the reverse geocoding of the coordinates
#' 
#' Obtains a readable address form the point location (latitude, longitude)
#' using an external service.
#' 
#' @param latitude the coordinates longitude
#' @param longitude the coordinates longitude
#' 
#' @usage getGeocoding(latitude, longitude)
#' 
#' @examples
#' # Get information from diferent coordinates
#' info <- getGeocoding(42.8806027, -8.5445684)
#' 
#' @rdname getGeocoding
#' @export getGeocoding
#' @aliases getGeocoding
getGeocoding <- function(latitude, longitude) {
  result <- getGeocoding.google(latitude, longitude)
  return(result)
}

#' @usage getGeocoding.google(latitude, longitude)
#' 
#' @rdname getGeocoding
#' @export getGeocoding.google
#' @aliases getGeocoding.google
getGeocoding.google <- function(latitude, longitude) {
  json_file <- paste('http://maps.googleapis.com/maps/api/geocode/json?latlng=',
                     latitude, ',', longitude, '&sensor=false', sep ='')
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  
  address   <- tryCatch(json_data$results[[1]]$address_components,
                        error = function(e) NULL)
  country       <- ''
  level_1       <- ''
  level_2       <- ''
  locality      <- ''
  street        <- ''
  street_number <- ''
  postal_code   <- ''
  
  if (is.null(address)) {
    return(NULL)
  } else {
    for (i in 1:length(address)) {
      if (any(address[[i]]$types == "street_number"))
        street_number <- address[[i]]$short_name
      else if (any(address[[i]]$types == "route"))
        street <- address[[i]]$short_name
      else if (any(address[[i]]$types == "locality"))
        locality <- address[[i]]$short_name
      else if (any(address[[i]]$types == "administrative_area_level_2"))
        level_2 <- address[[i]]$short_name
      else if (any(address[[i]]$types == "administrative_area_level_1"))
        level_1 <- address[[i]]$short_name
      else if (any(address[[i]]$types == "country"))
        country <- address[[i]]$short_name
      else if (any(address[[i]]$types == "postal_code"))
        postal_code <- address[[i]]$short_name
    }
  }
  
  result <- list(country       = country,
                 level_1       = level_1,
                 level_2       = level_2,
                 locality      = locality,
                 street        = street,
                 street_number = street_number,
                 postal_code   = postal_code)
}

