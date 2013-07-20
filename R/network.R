##
##  Check internet connection
##
##  Created by Daniel Rodríguez Pérez on 26/4/2013.
##  Copyright (c) 2013 Daniel Rodríguez Pérez. All rights reserved.
##

#' Check is the computer has access to google
#' 
#' Return TRUE where the computer can access to Google
#' 
#' @examples
#' if (is.connected()) {
#'   message('The computer can access to Google')
#' }
#' 
#' @export is.connected
#' @aliases is.connected is.connected
is.connected <- function() {
  if (is.windows()) {
    hasping <- as.logical(system('ping -n 1 www.google.com',
                                 show.output.on.console = FALSE))
  } else {
    hasping <- as.logical(system('ping -c 1 www.google.com',
                                 ignore.stdout = TRUE,
                                 ignore.stderr = TRUE))
  }
  
  return(!hasping)
}
