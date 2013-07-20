##
##  Check internet connection and network information
##
##  Created by Daniel Rodríguez Pérez on 26/4/2013.
##  Copyright (c) 2013 Daniel Rodríguez Pérez. All rights reserved.
##

#' Check is the computer has access to google
#' 
#' Return TRUE where the computer can access to Google
#' 
#' @examples
#' if (is.connected()) {
#'   message('The computer can access to Google')
#' }
#' 
#' @export is.connected
#' @aliases is.connected is.connected
is.connected <- function() {
  if (is.windows()) {
    hasping <- as.logical(system('ping -n 1 www.google.com',
                                 show.output.on.console = FALSE))
  } else {
    hasping <- as.logical(system('ping -c 1 www.google.com',
                                 ignore.stdout = TRUE,
                                 ignore.stderr = TRUE))
  }
  
  return(!hasping)
}

#' Get the external IP of the computer
#'
#' Returns the external IP of the computer if it is connected to internet,
#' otherwise returns NULL
#' 
#' @examples
#' IP <- getExternalIP()
#' 
#' @export getExternalIP
#' @aliases getExternalIP getExternalIP
getExternalIP <- function() {
  if (is.connected()) {
    ip <- getURL('http://bot.whatismyipaddress.com')
  } else {
    ip <- NULL
  }
  
  return(ip)
}

#' Get location from freegeoip.net
#' 
#' Returns the location of an IP using freegeoip.net public RESTful web
#' service. If an IP is indicated the function returns it's estimated location,
#' otherwise it will use the IP of the system
#' 
#'  @param ip an IP address for geolocation (optional default uses the computer)
#'  
#' @examples
#' # Get computer location
#' location <- getLocationFromFreegeoip()
#' 
#' # Get '10.10.10.10' location
#' location <- getLocationFromFreegeoip('10.10.10.10')
#' 
#' @export getLocationFromFreegeoip
#' @aliases getLocationFromFreegeoip getLocationFromFreegeoip
getLocationFromFreegeoip <- function(ip = getExternalIP()) {
  if (is.null(ip)) {
    result <- NULL
  } else {
    url    <- paste('http://freegeoip.net/json/', ip, sep = '')
    result <- fromJSON(readLines(url, warn=FALSE))
  }
  
  return(result)
}
