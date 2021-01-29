#' Run RQDA
#'
#' Start up RQDA in a given session.
#' 
#' @export
run <- function() {
  if (!require(RQDA, quietly = TRUE))
    stop("RQDA is not installed")
}