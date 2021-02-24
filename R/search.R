

#' Search for codes using a given term
#'
#' @importFrom RQDA getCodingTable
#'
#' @param searchterm A string.
#'
#' @return An object of class \code{data.frame}, which is subset of the coding
#' table.
#'
#' @export
search_codes <- function(searchterm) {
  if (!is.character(searchterm) && length(searchterm) != 1L)
    stop("Expected a string as argument")
  if (!nchar(searchterm))
    stop("Search cannot be conducted with an empty string")
  cdt <- getCodingTable()
  subset(cdt, grepl(searchterm, codename, ignore.case = TRUE))
}
