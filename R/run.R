#' Run RQDA
#'
#' Start up RQDA in a given session.
#'
#' @note Also attaches RSQLite package to the search path
#'
#' @export
start <- function() {
  .f <- function(p) {
    if (!requireNamespace(p, quietly = TRUE)) {
      msg <- paste(p, "is not installed")
      if (p == "RQDA")
        msg <- sprintf("%s. Install it with RQDAassist::install()")
      stop(msg, call. = FALSE)
    }
    if (p %in% .packages())
      return()
    attachNamespace(p)
  }

  pkgs <- c("RSQLite", "RQDA")
  invisible(lapply(pkgs, .f))
}
