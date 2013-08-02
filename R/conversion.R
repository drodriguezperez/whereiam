##
##  Units conversion functions
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

units2Kilometers <- function(values, units) {
  result <- switch(tolower(units),
                   km          = values,
                   meters      = values / 1000,
                   centimeters = values / 100000,
                   cm          = values / 100000,
                   miles       = values / KM_TO_MILES,
                   feets       = values / KM_TO_FEETS,
                   inches      = values / KM_TO_INCHES,
                   stop('Allowed units are km, meters, cm, miles, feets or inches'))
  return(result)
}

kilometers2Units <- function(values, units) {
  result <- switch(tolower(units),
                   km          = values,
                   meters      = values * 1000,
                   centimeters = values * 100000,
                   cm          = values * 100000,
                   miles       = values * KM_TO_MILES,
                   feets       = values * KM_TO_FEETS,
                   inches      = values * KM_TO_INCHES,
                   stop('Allowed units are km, meters, cm, miles, feets or inches'))
  return(result)
}
