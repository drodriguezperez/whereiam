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
#' @references 
#' http://en.wikipedia.org/wiki/Great-circle_distance
#' 
#' @rdname GreatCircle
#' @export GreatCircle
#' @aliases GreatCircle
GreatCircle <- function(...){
  UseMethod("GreatCircle")
}

#' @rdname GreatCircle
#' @method GreatCircle default
#' @export
GreatCircle.default <- function(latitude, longitude, brg, ...) {
  coordinate <- Coordinate(latitude, longitude)
  obj        <- GreatCircle(coordinate, brg)
  return(obj)
}

#' @rdname GreatCircle
#' @method GreatCircle Coordinate
#' @export
GreatCircle.Coordinate <- function(coordinate, brg, ...) {
  obj        <- list(coordinate = coordinate,
                     bearing    = brg)
  class(obj) <- 'GreatCircle'
  return(obj)
}

#' @rdname getLatitude
#' @method getLatitude GreatCircle
#' @export
getLatitude.GreatCircle <- function(coordinate,
                                    units = 'degrees') {
  result <- getLatitude(coordinate$coordinate, units = units)
  return(result)
}

#' @rdname getLongitude
#' @method getLongitude GreatCircle
#' @export
getLongitude.GreatCircle <- function(coordinate,
                                     units = 'degrees') {
  result <- getLongitude(coordinate$coordinate, units = units)
  return(result)
}

#' Extract bearing from a GreatCircle
#' 
#' Get the bearing from a GreatCircle object in the indicated units (degrees
#' or radians)
#' 
#' @param coordinate a GreatCircle class
#' @param units a string with the units (degrees or radians)
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname getBearing
#' @export getBearing
getBearing <- function(coordinate,
                       units = 'degrees') {
  UseMethod("getBearing")
}

#' @rdname getBearing
#' @method getBearing default
#' @export
getBearing.default <- function(coordinate,
                               units = 'degrees') {
  result <- deg2any(coordinate, units = units)
  return(result)
}

#' @rdname getBearing
#' @method getBearing GreatCircle
#' @export
getBearing.GreatCircle <- function(coordinate,
                                   units = 'degrees') {
  result <- getLongitude(coordinate$bearing, units = units)
  return(result)
}

#' Intersection of two great circle
#'
#' Calculate of intersection of two great circle paths
#' 
#' @param greatCircle1 a GreatCircle class
#' @param greatCircle2 a GreatCircle class
#' @param ... other argument
#' 
#' @references
#' http://www.movable-type.co.uk/scripts/latlong.html
#' 
#' @rdname intersection
#' @export intersection
intersection <- function(...){
  UseMethod("intersection")
}

#' @rdname intersection
#' @method intersection GreatCircle
#' @export
intersection.GreatCircle <- function(greatCircle1, greatCircle2, ...) {
  latitude1  <- getLatitude(greatCircle1, units='radians')
  longitude1 <- getLongitude(greatCircle1, units='radians')
  brg1       <- getBearing(greatCircle1, units='radians')
  latitude2  <- getLatitude(greatCircle2, units='radians')
  longitude2 <- getLongitude(greatCircle2, units='radians')
  brg2       <- getBearing(greatCircle2, units='radians')
  
  dLat <- latitude2 - latitude1
  dLon <- longitude2 - longitude1
  
  dist12 <- 2 * asin(sqrt(sin(dLat/2) * sin(dLat/2) +
                            cos(latitude1) * cos(latitude2) * sin(dLon/2) * sin(dLon/2)))
  if (dist12 == 0) {
    return(NULL)
  }
  
  brngA <- acos((sin(latitude2) - sin(latitude1) * cos(dist12)) /
                  (sin(dist12) * cos(latitude1)))
  if (is.na(brngA)) {
    brngA = 0
  }
  
  brngB <- acos((sin(latitude1) - sin(latitude2) * cos(dist12)) / (sin(dist12) * cos(latitude2)))
  
  if (sin(longitude2 - longitude1) > 0) {
    brng12 = brngA
    brng21 = 2 * pi - brngB
  } else {
    brng12 = 2 * pi - brngA
    brng21 = brngB
  }
  
  alpha1 = (brg1 - brng12 + pi) %% (2 * pi) - pi
  alpha2 = (brng21 - brg2 + pi) %% (2 * pi) - pi
  
  if (sin(alpha1) == 0 && sin(alpha2) == 0) {
    return(NULL)
  }
  
  if (sin(alpha1) * sin(alpha2) < 0) {
    return(NULL)
  }
  
  alpha3 <- acos(-cos(alpha1) * cos(alpha2) + sin(alpha1) * sin(alpha2) * cos(dist12))
  dist13 <- atan2(sin(dist12) * sin(alpha1) * sin(alpha2),
                  cos(alpha2) + cos(alpha1) * cos(alpha3))
  lat3   <- asin(sin(latitude1) * cos(dist13) + cos(latitude1) * sin(dist13) * cos(brg1))
  dlongitude13 <- atan2(sin(brg1) * sin(dist13) * cos(latitude1),
                        cos(dist13) - sin(latitude1) * sin(lat3))
  lon3   <- longitude1 + dlongitude13;
  lon3   <- (lon3 + 3 * pi) %% (2 * pi) - pi;  
  
  result <- Coordinate(rad2deg(lat3), rad2deg(lon3))
  return(result)
}
