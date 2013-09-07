##
##  Test getters for the constans
##
##  Created by Daniel Rodríguez Pérez on 31/8/2013.
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

context("Units conversion functions test")

test_that("get EARTH_DIAMETER_KM", {
  expect_that(earthDiameter(), 
              equals(EARTH_DIAMETER_KM))
  expect_that(earthDiameter('km'), 
              equals(EARTH_DIAMETER_KM))
  expect_that(earthDiameter('meters'),
              equals(1000 * EARTH_DIAMETER_KM))
  expect_that(earthDiameter('centimeters'),
              equals(100000 * EARTH_DIAMETER_KM))
  expect_that(earthDiameter('cm'),
              equals(100000 * EARTH_DIAMETER_KM))
  
  expect_that(earthDiameter('miles'), 
              equals(kilometers2Units(EARTH_DIAMETER_KM, 'miles')))
  expect_that(earthDiameter('feets'), 
              equals(kilometers2Units(EARTH_DIAMETER_KM, 'feets')))
  expect_that(earthDiameter('inches'), 
              equals(kilometers2Units(EARTH_DIAMETER_KM, 'inches')))
})
