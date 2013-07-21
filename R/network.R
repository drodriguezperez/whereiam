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
#' @usage is.connected()
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
#' @usage getExternalIP()
#' 
#' @examples
#' IP <- getExternalIP()
#' 
#' @rdname getExternalIP
#' @export getExternalIP
#' @aliases getExternalIP getExternalIP
getExternalIP <- function() {
  return(getExternalIP.whatismyipaddress())
}

# Get the external ip from a server indicated in the URL
getExternalIP.url <- function(url) {
  if (is.connected()) {
    ip <- getURL(url)
    ip <- gsub('\n', '', ip)
  } else {
    ip <- NULL
  }
  
  return(ip)
}

#' @usage getExternalIP.whatismyipaddress()
#' 
#' @rdname getExternalIP
#' @export getExternalIP.whatismyipaddress
#' @aliases getExternalIP.whatismyipaddress getExternalIP.whatismyipaddress
getExternalIP.whatismyipaddress <- function() {
  ip <- getExternalIP.url('http://bot.whatismyipaddress.com')
  return(ip)
}

#' @usage getExternalIP.ifconfig()
#' 
#' @rdname getExternalIP
#' @export getExternalIP.ifconfig
#' @aliases getExternalIP.ifconfig getExternalIP.ifconfig
getExternalIP.ifconfig <- function() {
  ip <- getExternalIP.url('http://ifconfig.me/ip')
  return(ip)
}

#' @usage getExternalIP.icanhazip()
#' 
#' @rdname getExternalIP
#' @export getExternalIP.icanhazip
#' @aliases getExternalIP.icanhazip getExternalIP.icanhazip
getExternalIP.icanhazip <- function() {
  ip <- getExternalIP.url('http://icanhazip.com')
  return(ip)
}

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
#' @aliases getLocation getLocation
getLocation <- function(ip = getExternalIP()) {
  return(getLocation.freegeoip(ip))
}

#' @usage getLocation.freegeoip(ip = getExternalIP())
#' 
#' @rdname getLocation
#' @export getLocation.freegeoip
#' @aliases getLocation getLocation.freegeoip
getLocation.freegeoip <- function(ip = getExternalIP()) {
  if (is.null(ip)) {
    result <- NULL
  } else {
    url    <- paste('http://freegeoip.net/json/', ip, sep = '')
    result <- fromJSON(readLines(url, warn=FALSE))
  }
  
  return(result)
}
