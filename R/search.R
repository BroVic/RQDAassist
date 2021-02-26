globalVariables("codename")
#' Search for codes using a given term
#'
#' @param searchterm A string.
#' @param codes.only Logical; whether to fetch only the codes or a table with
#' other related variables. Defaults to \code{TRUE}.
#'
#' @return An object of class \code{data.frame}, which is subset of the coding
#' table, or in the case of \code{codes.only == TRUE}, a list of matching
#' codes.
#'
#' @export
search_codes <- function(searchterm, codes.only = TRUE) {
  if (!is.character(searchterm) && length(searchterm) != 1L)
    stop("Expected a string as argument")
  if (!nchar(searchterm))
    stop("Search cannot be conducted with an empty string")
  start()
  cdt <- eval(call("getCodingTable"))
  sub <- subset(cdt, grepl(searchterm, codename, ignore.case = TRUE))
  if (codes.only)
    return(unique(sub$codename))
  sub
}
