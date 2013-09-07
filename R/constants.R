##
##  Constans used on the package
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

# *****************************************************************************
#  Earth data
# *****************************************************************************
EARTH_DIAMETER_KM  <- 12742

# *****************************************************************************
#  WGS 84 information
# *****************************************************************************
SEMI_MAJOR_AXIS <- 6378137
SEMI_MINOR_AXIS <- 6356752.314245
FLATTENING      <- 1 / 298.257223563

# *****************************************************************************
#  Units conversions
# *****************************************************************************
KM_TO_MILES  <- 0.621371192
KM_TO_FEETS  <- 3280.8399
KM_TO_INCHES <- 39370.0787

# *****************************************************************************
#  Getter for the constans
# *****************************************************************************

#' Returns the diameter of the earth
#' 
#' Returns the value used for the diameter of the earth in the calculations
#' 
#' @param units a string with the distance units (default kilometers)
#' 
#' @export earthDiameter
#' @aliases earthDiameter earthDiameter
earthDiameter <- function(units = 'km'){
  return(kilometers2Units(EARTH_DIAMETER_KM, units = units))
}
