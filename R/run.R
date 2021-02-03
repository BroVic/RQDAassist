#' Run RQDA
#'
#' Start up RQDA in a given session.
#'
#' @export
run <- function() {
  p <- "RQDA"
  if (!requireNamespace(p, quietly = TRUE))
    stop(paste(p, "is not installed"))
  if(!p %in% .packages()) return()
  attachNamespace(p)
}
