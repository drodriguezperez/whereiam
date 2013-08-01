##
##  GreatCircle S3 class
##
##  Created by Daniel Rodríguez Pérez on 30/7/2013.
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

#' Create a GreatCircle object
#' 
#' The method create a new GreatCircle object type using the indicated
#' latitude and longitude and bearing
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param brg the initial bearing
#' @param coordinate a coordinate class
#' @param ... other argument
#' 
#' @rdname GreatCircle
#' @export GreatCircle
#' @aliases GreatCircle
GreatCircle <- function(...){
  UseMethod("GreatCircle")
}

#' @rdname GreatCircle
#' @method GreatCircle default
#' @S3method GreatCircle default
GreatCircle.default <- function(latitude, longitude, brg, ...) {
  coordinate <- Coordinate(latitude, longitude)
  obj        <- GreatCircle(coordinate, brg)
  return(obj)
}

#' @rdname GreatCircle
#' @method GreatCircle Coordinate
#' @S3method GreatCircle Coordinate
GreatCircle.Coordinate <- function(coordinate, brg, ...) {
  obj        <- list(coordinate = coordinate,
                     bearing    = brg)
  class(obj) <- 'GreatCircle'
  return(obj)
}

#' @rdname getLatitude
#' @method getLatitude GreatCircle
#' @S3method getLatitude GreatCircle
getLatitude.GreatCircle <- function(coordinate,
                                    units = 'degrees') {
  result <- getLatitude(coordinate$coordinate, units = units)
  return(result)
}

#' @rdname getLongitude
#' @method getLongitude GreatCircle
#' @S3method getLongitude GreatCircle
getLongitude.GreatCircle <- function(coordinate,
                                     units = 'degrees') {
  result <- getLongitude(coordinate$coordinate, units = units)
  return(result)
}
