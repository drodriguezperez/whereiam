##
##  Reverse geocoding functions
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

#' Resolve the reverse geocoding of the coordinates
#' 
#' @description
#' Obtains a readable address form the point location (latitude, longitude)
#' using an external service.
#' 
#' The list of services available are: Google
#' 
#' @param latitude the coordinates longitude
#' @param longitude the coordinates longitude
#' @param coordinate a coordinate class
#' @param service the optional service used to obtain the information
#' @param ... other arguments
#' 
#' @rdname reverseGeocoding
#' @export reverseGeocoding
reverseGeocoding <- function(...) {
  UseMethod("reverseGeocoding")
}

#' @rdname reverseGeocoding
#' @method reverseGeocoding default
#' @export
reverseGeocoding.default <- function(latitude, longitude,
                                     service = 'google', ...) {
  switch(tolower(service),
         google = reverseGeocoding_Google(latitude, longitude),
         stop(sprintf('The service "%s" is not supported', service)))
}

#' @rdname reverseGeocoding
#' @method reverseGeocoding Coordinate
#' @export
reverseGeocoding.Coordinate <- function(coordinate,
                                        service = 'google', ...) {
  result <- reverseGeocoding(getLatitude(coordinate),
                             getLongitude(coordinate),
                             service=service)
  return(result)
}

# Implements Google reverse geocoding service
reverseGeocoding_Google <- function(latitude, longitude) {
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
