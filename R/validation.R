##
##  Validation functions
##
##  Created by Daniel Rodríguez Pérez on 1/8/2013.
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

#' Validate longitude values
#' 
#' Set the input longitude values between -180 and +180 degrees
#' 
#' @param longitude a longitude coordinate
#' 
#' @rdname validate.Longitude
#' @export validate.Longitude
#' @aliases validate.Longitude
validate.Longitude <- function(longitude) {
  longitude <- reduce360(longitude)
  
  if (is.null(longitude)) {
    longitude <- NULL
  } else {
    if (longitude > 180) {
      longitude <- longitude - 180
    } else if (longitude < -180) {
      longitude <- 180 + longitude
    }
  }
  
  return(longitude)
}

#' Validate latitude values
#' 
#' Set the input latitude values between -90 and +90 degrees
#' 
#' @param latitude a latitude coordinate
#' 
#' @rdname validate.Latitude
#' @export validate.Latitude
#' @aliases validate.Latitude
validate.Latitude <- function(latitude) {
  latitude <- reduce360(latitude)
  
  if (is.null(latitude)) {
    latitude <- NULL
  } else {
    if (latitude > 180 || latitude < - 180) {
      latitude <- - (latitude - 180)
    }
    
    if (latitude > 90) {
      latitude <- 180 - latitude
    } else if (latitude < -90) {
      latitude <- - (180 + latitude)
    }    
  }
  
  return(latitude)
}

# Reduce an angle to 360
reduce360 <- function(deg) {
  if (is.numeric(deg)) {
    if (deg > 360) {
      deg <- deg - 360 * floor(deg / 360)
    } else if (deg < -360) {
      deg <- deg - 360 * ceiling(deg / 360)
    }
  } else {
    deg <- NULL
  }

  return(deg)
}
