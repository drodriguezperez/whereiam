##
##  Trigonometry functions
##
##  Created by Daniel Rodríguez Pérez on 29/7/2013.
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

#' Convert degrees to radians
#' 
#' Convert between degrees and radians
#' 
#' @param deg degrees to convert to radians
#' 
#' @return the value converted in radians
#' 
#' @rdname deg2rad
#' @export deg2rad
#' @aliases deg2rad
deg2rad <- function(deg) {
  result <- deg * pi / 180
  return(result)
}

#' Convert degrees, minutas and seconds to degrees
#' 
#' Transform the values in degrees, minutes and seconds to degrees in decimal
#' format
#' 
#' @param degrees degrees
#' @param minutes minutes
#' @param seconds seconds
#' 
#' @return the position using degrees in decimal format
#' 
#' @rdname dms2deg
#' @export dms2deg
#' @aliases dms2deg
dms2deg <- function(degrees, minutes, seconds) {
  result <- degrees + minutes / 60 + seconds / 3600
  return(result)
}

#' Convert degrees, minutas and seconds to radians
#' 
#' Transform the values in degrees, minutes and seconds to radians
#' 
#' @param degrees degrees
#' @param minutes minutes
#' @param seconds seconds
#' 
#' @return the position using degrees in radians
#' 
#' @rdname dms2rad
#' @export dms2rad
#' @aliases dms2rad
dms2rad <- function(degrees, minutes, seconds) {
  result <- deg2rad(dms2deg(degrees, minutes, seconds))
  return(result)
}

#' Convert radians to degrees
#' 
#' Convert between radians and degrees
#' 
#' @param rad radians to convert to degrees
#' 
#' @return the value converted in degrees
#' 
#' @rdname rad2deg
#' @export rad2deg
#' @aliases rad2deg
rad2deg <- function(rad) {
  result <- 180 * rad / pi
  return(result)
}

#' Convert to a radians or degrees
#' 
#' Convert from degrees to radians or degrees
#' 
#' @param coordinate degrees to convert
#' @param units a string with the units (degrees or radians)
#' 
#' @rdname deg2any
#' @export deg2any
#' @aliases deg2any
deg2any <- function(coordinate,
                    units = 'degrees') {
  result <- switch(tolower(units),
                   degrees = coordinate,
                   radians = deg2rad(coordinate),
                   NULL)
  return(result)
}
