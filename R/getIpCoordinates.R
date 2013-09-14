##
##  Get geolocation coordinates from IP address
##
##  Created by Daniel Rodríguez Pérez on 27/7/2013.
##
##  Copyright (c) 2013 Daniel Rodríguez Pérez.
##
##  This program is free software: you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation, either version 3 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program.  If not, see <http://www.gnu.org/licenses/>
## 

#' Get location coordinates from an IP
#' 
#' Returns the location coordinates of an IP using an external service. If an
#' IP is indicated the function returns it's estimated location, otherwise it
#' will use the IP of the system.
#' 
#' The list of services available are:
#'    * Freegeoip
#'    * Hostip
#'
#' @param ip an IP address for geolocation (optional default uses the computer)
#' @param service the optional service used to obtain the coordinates
#' 
#' @rdname getIpCoordinates
#' @export getIpCoordinates
#' @aliases getIpCoordinates
getIpCoordinates <- function(ip = getExternalIP(), service = 'freegeoip') {
  switch(tolower(service),
         freegeoip = getIpCoordinates_freegeoip(ip),
         hostip    = getIpCoordinates_hostip(ip),
         stop(sprintf('The service "%s" is not supported', service)))
}

# Returns the location of an IP using the URL
getIpCoordinates.url <- function(url, ip) {
  if (is.null(ip)) {
    result <- NULL
  } else {
    url    <- paste(url, ip, sep = '')
    result <- fromJSON(readLines(url, warn=FALSE))
  }
  
  return(result)
}

# Implements access to freegeoip IP coordinates service
getIpCoordinates_freegeoip <- function(ip = getExternalIP()) {
  result <- getIpCoordinates.url('http://freegeoip.net/json/', ip)
  result <- Coordinate(result$latitude, result$longitude)
  return(result)
}

# Implements access to Hostip IP coordinates service
getIpCoordinates_hostip <- function(ip = getExternalIP()) {
  result <- getIpCoordinates.url('http://api.hostip.info/get_json.php?position=true&ip=', ip)
  result <- Coordinate(result$lat, result$lng)
  return(result)
}
