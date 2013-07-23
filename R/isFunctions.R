##
##  List of is.operating_system functions
##
##  Created by Daniel Rodríguez Pérez on 10/4/2013.
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


#' Check is the computer is runing OS X
#' 
#' Returns TRUE for computers runing OS X (Macintosh)
#' 
#' @usage is.mac()
#' 
#' @examples
#' if (is.mac()) {
#'   message('The computer is running OS X')
#' }
#' 
#' @export is.mac
#' @aliases is.mac is.mac
is.mac <- function() {
  if (tolower(Sys.info()['sysname']) == 'darwin')
    return(TRUE)
  else
    return(FALSE)
}

#' Check is the computer is runing Windows
#' 
#' Returns TRUE for computers runing Windows OS
#' 
#' @usage is.windows()
#' 
#' @examples
#' if (is.windows()) {
#'   message('The computer is running Windows OS')
#' }
#' 
#' @export is.windows
#' @aliases is.windows is.windows
is.windows <- function() {
  if (tolower(Sys.info()['sysname']) == 'windows')
    return(TRUE)
  else
    return(FALSE)
}

#' Check is the computer is runing Linux
#' 
#' Returns TRUE for computers runing Linux OS
#' 
#' @usage is.linux()
#' 
#' @examples
#' if (is.linux()) {
#'   message('The computer is running GNU/Linux OS')
#' }
#'  
#' @export is.linux
#' @aliases is.linux is.linux
is.linux <- function() {
  if (tolower(Sys.info()['sysname']) == 'linux')
    return(TRUE)
  else
    return(FALSE)
}
