##
##  List of is.operating_system functions
##
##  Created by Daniel Rodríguez Pérez on 10/4/2013.
##  Copyright (c) 2013 Daniel Rodríguez Pérez. All rights reserved.
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
