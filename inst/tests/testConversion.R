##
##  Units conversion function test
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

context("Units conversion functions test")

MAXERROR <- 1e-6

test_that("conversion from km erros", {
  expect_that(kilometers2Units(10, 'armstrong'),
              throws_error('Allowed units are km, meters, cm, miles, feets or inches'))
})

test_that("conversion to km erros", {
  expect_that(units2Kilometers(10, 'armstrong'),
              throws_error('Allowed units are km, meters, cm, miles, feets or inches'))
})

test_that("conversion from km", {
  expect_that(kilometers2Units(10, 'km'),
              equals(10, tolerance = MAXERROR))
  expect_that(kilometers2Units(10, 'meters'),
              equals(10000, tolerance = MAXERROR))
  expect_that(kilometers2Units(10, 'centimeters'),
              equals(1000000, tolerance = MAXERROR))
  expect_that(kilometers2Units(10, 'cm'),
              equals(1000000, tolerance = MAXERROR))
  
  expect_that(kilometers2Units(10, 'miles'),
              equals(6.21371192, tolerance = MAXERROR))  
  expect_that(kilometers2Units(10, 'feets'),
              equals(32808.399, tolerance = MAXERROR))  
  expect_that(kilometers2Units(10, 'inches'),
              equals(393700.787, tolerance = MAXERROR))
})

test_that("conversion to km", {
  expect_that(units2Kilometers(10, 'km'),
              equals(10, tolerance = MAXERROR))
  expect_that(units2Kilometers(10, 'meters'),
              equals(0.01, tolerance = MAXERROR))
  expect_that(units2Kilometers(10, 'centimeters'),
              equals(0.0001, tolerance = MAXERROR))
  expect_that(units2Kilometers(10, 'cm'),
              equals(0.0001, tolerance = MAXERROR))
  
  expect_that(units2Kilometers(10, 'miles'),
              equals(16.09344, tolerance = MAXERROR))  
  expect_that(units2Kilometers(10, 'feets'),
              equals(0.003048, tolerance = MAXERROR))  
  expect_that(units2Kilometers(10, 'inches'),
              equals(0.000254, tolerance = MAXERROR))
})
