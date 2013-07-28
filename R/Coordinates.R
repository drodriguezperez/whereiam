##
##  Coordinates S3 class
##
##  Created by Daniel Rodríguez Pérez on 28/7/2013.
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

# *****************************************************************************
#  Coordinate constans
# *****************************************************************************
EARTH_DIAMETER_KM  <- 12756.2732 
KM_TO_MILES        <- 0.621371192

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
}

#' Validate a latitude value
#' 
#' Return a TRUE value is the input parameter is a valid latitude value, FALSE
#' otherwise
#' 
#' @param latitude a latitude coordinate value
#' 
#' @examples
#' # return a true value
#' is.latitude(40)
#' 
#' # return a false value
#' is.latitude(100)
#' 
#' @rdname is.latitude
#' @export is.latitude
#' @aliases is.latitude
is.latitude <- function(latitude) {
  if (is.numeric(latitude)) {
    if(latitude >= -90 && latitude <= 90) {
      return(TRUE)
      } else {
        return(FALSE)
      }
  } else {
    return(FALSE)
  }
}

#' Validate a longitude value
#' 
#' Return a TRUE value is the input parameter is a valid longitude value,
#' FALSE otherwise
#' 
#' @param longitude a longitude coordinate value
#' 
#' @examples
#' #' # return a true value
#' is.longitude(40)
#' 
#' # return a false value
#' is.longitude(200)
#' 
#' @rdname is.longitude
#' @export is.longitude
#' @aliases is.longitude
is.longitude <- function(longitude) {
  if (is.numeric(longitude)) {
    if(longitude >= -180 && longitude <= 180) {
      return(TRUE)
      } else {
        return(FALSE)
      }
  } else {
    return(FALSE)
  }
}

#' Create a Coordinates object
#' 
#' The method create a new Coordinates object type using the indicated
#' latitude and longitude
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' 
#' @rdname Coordinates
#' @export Coordinates
#' @aliases Coordinates
Coordinates <- function(latitude, longitude) {
  if (is.latitude(latitude) && is.longitude(longitude)) {
    obj <- list(latitude  = latitude,
                longitude = longitude)
    class(obj) <- 'Coordinates'
    return(obj)
  } else {
    if (!is.latitude(latitude) && !is.longitude(longitude)) {
      stop('The latitude and longitude are not valid value')
    } else if (!is.latitude(latitude)) {
      stop('The latitude is not a valid value')
    } else {
      stop('The longitude is not a valid value')
    }
  }
}

#' Add distance in the latitude direction to a coordinate
#' 
#' Add distance in the latitude direction to the indicate coordinate. The
#' distance can be indicated on kilometers or miles
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param distance the distance to add in km or miles
#' @param units a string with the distance units (default kilometers)
#' @param coordinate a coordinate class
#' @param ... other arguments
#' 
#' @return a Coordinates object with the new coordinates
#' 
#' @examples
#' # Add 10 kilometers to the north to a position
#' cord <- Coordinates(43, -8)
#' cord <- addDistanceLatitude(cord, 10)
#' 
#' @rdname addDistanceLatitude
#' @export addDistanceLatitude
addDistanceLatitude <- function(...){
  UseMethod("addDistanceLatitude")
}

#' @rdname addDistanceLatitude
#' @method addDistanceLatitude default
#' @S3method addDistanceLatitude default
addDistanceLatitude.default <- function(latitude, longitude, distance, units = 'km', ...) {
  if (tolower(units) == 'miles') {
    distance <- distance / KM_TO_MILES
  }
  
  latitude <- latitude + (distance * 360 / (EARTH_DIAMETER_KM * pi))
  
  result <- Coordinates(latitude, longitude)
  
  return(result)
}

#' @rdname addDistanceLatitude
#' @method addDistanceLatitude Coordinates
#' @S3method addDistanceLatitude Coordinates
addDistanceLatitude.Coordinates <- function(coordinate, distance, units = 'km', ...) {
  addDistanceLatitude(coordinate$latitude, coordinate$longitude, distance, units = units)
}

#' Add distance in the longitude direction to a coordinate
#' 
#' Add distance in the longitude direction to the indicate coordinate. The
#' distance can be indicated on kilometers or miles
#' 
#' @param latitude a latitude coordinate
#' @param longitude a longitude coordinate
#' @param distance the distance to add in km or miles
#' @param units a string with the distance units (default kilometers)
#' @param coordinate a coordinate class
#' @param ... other arguments
#' 
#' @return a Coordinates object with the new coordinates
#' 
#' @examples
#' # Add 10 kilometers to the east to a position
#' cord <- Coordinates(43, -8)
#' cord <- addDistanceLongitude(cord, 10)
#' 
#' @rdname addDistanceLongitude
#' @export addDistanceLongitude
addDistanceLongitude <- function(...){
  UseMethod("addDistanceLongitude")
}

#' @rdname addDistanceLongitude
#' @method addDistanceLongitude default
#' @S3method addDistanceLongitude default
addDistanceLongitude.default <- function(latitude, longitude, distance, units = 'km', ...) {
  if (tolower(units) == 'miles') {
    distance <- distance / KM_TO_MILES
  }
  
  longitude <- longitude +
    (distance * 360 / (EARTH_DIAMETER_KM * pi * cos(deg2rad(latitude))))
  
  result <- Coordinates(latitude, longitude)
  
  return(result)
}

#' @rdname addDistanceLongitude
#' @method addDistanceLongitude Coordinates
#' @S3method addDistanceLongitude Coordinates
addDistanceLongitude.Coordinates <- function(coordinate, distance, units = 'km', ...) {
  addDistanceLongitude(coordinate$latitude, coordinate$longitude, distance, units = units)
}

#' Calculate distance between two points
#' 
#' The function calculates great-circle distances between the two points using
#' the "Haversine" formula. This is the shortest distance over the earth's
#' surface ignoring the geographic shape like hills and vales.
#' 
#' @param latitude1 the first latitude coordinate
#' @param longitude1 the first longitude coordinate
#' @param latitude2 the second latitude coordinate
#' @param longitude2 the second longitude coordinate
#' @param units a string with the distance units (default kilometers)
#' @param coordinate1 the first coordinate class variable
#' @param coordinate2 the second coordinate class variable
#' @param ... other arguments
#' 
#' @examples
#' cord1    <- Coordinates(43, -8)
#' cord2    <- Coordinates(42, -7)
#' distance <- getDistance(cord1, cord2)
#' 
#' @rdname getDistance
#' @export getDistance
getDistance <- function(...) {
  UseMethod("getDistance")
}

#' @rdname getDistance
#' @method getDistance default
#' @S3method getDistance default
getDistance.default <- function(latitude1, longitude1, latitude2, longitude2, units = 'km', ...) {
  incLatitude  <- deg2rad(latitude2 - latitude1)
  incLongitude <- deg2rad(longitude2 - longitude1)
  
  a <- sin(incLatitude/2)^2 + sin(incLongitude/2)^2 *
    cos(deg2rad(latitude1)) * cos(deg2rad(latitude2))
  
  result <- EARTH_DIAMETER_KM * atan2(sqrt(a), sqrt(1-a))
  
  if (tolower(units) == 'miles') {
    result <- result * KM_TO_MILES
  }
  
  return(result)
}

#' @rdname getDistance
#' @method getDistance Coordinates
#' @S3method getDistance Coordinates
getDistance.Coordinates <- function(coordinate1, coordinate2, units = 'km', ...) {
  getDistance(coordinate1$latitude, coordinate1$longitude, 
              coordinate2$latitude, coordinate2$longitude, units = units)
}
