##
##  Check internet connection and network information
##
##  Created by Daniel Rodríguez Pérez on 26/4/2013.
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
#' @description
#' Returns the external IP of the computer if it is connected to internet,
#' otherwise returns NULL
#' 
#' The list of services available are: whatismyipaddress, ifconfig, icanhazip,
#' ipecho and appspot   
#' 
#' @param service the optional service used to obtain the external IP
#' 
#' @rdname getExternalIP
#' @export getExternalIP
#' @aliases getExternalIP getExternalIP
getExternalIP <- function(service = 'whatismyipaddress') {
  switch(tolower(service),
         whatismyipaddress = getExternalIP_whatismyipaddress(),
         ifconfig          = getExternalIP_ifconfig(),
         icanhazip         = getExternalIP_icanhazip(),
         ipecho            = getExternalIP_ipecho(),
         appspot           = getExternalIP_appspot(),
         stop(sprintf('The service "%s" is not supported', service)))
}

# Get the external ip from a server indicated in the URL
getExternalIP_url <- function(url) {
  if (is.connected()) {
    ip <- getURL(url)
    ip <- gsub('\n', '', ip)
  } else {
    ip <- NULL
  }
  
  return(ip)
}

# Get the external ip from whatismyipaddress
getExternalIP_whatismyipaddress <- function() {
  ip <- getExternalIP_url('http://bot.whatismyipaddress.com')
  return(ip)
}

# Get the external ip from ifconfig
getExternalIP_ifconfig <- function() {
  ip <- getExternalIP_url('http://ifconfig.me/ip')
  return(ip)
}

# Get the external ip from icanhazip
getExternalIP_icanhazip <- function() {
  ip <- getExternalIP_url('http://icanhazip.com')
  return(ip)
}

# Get the external ip from ipecho
getExternalIP_ipecho <- function() {
  ip <- getExternalIP_url('http://ipecho.net/plain')
  return(ip)
}

# Get the external ip from ip.appspot.com
getExternalIP_appspot <- function() {
  ip <- getExternalIP_url('http://ip.appspot.com')
  return(ip)
}
