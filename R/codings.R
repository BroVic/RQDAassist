globalVariables(c("cid", "codecat"))
#' Retrieve Tabulated Codings
#'
#' Fetches a table that has the codings as well as other details
#' related to codings as stored in the database.
#'
#' @param proj Path to an RQDA project i.e. must have the file extension
#' \emph{.rqda}.
#' @param query A string; an SQL query.
#'
#' @details The default query selects the following columns from the RQDA
#' file: \code{cid}  from the \code{treecode} table and \code{name} from
#' the \code{codecat} table. (For details on the database refer to
#' \code{RQDA::RQDATables})
#'
#' @importFrom dplyr `%>%`
#' @importFrom dplyr count
#' @importFrom dplyr group_by
#'
#' @return A data frame containing details about codings, including the
#' code name, filenames, starting and ending index, and code categories (if
#' applicable).
#'
#' @export
retrieve_codingtable <- function(proj, query = NULL) {
  RQDA::openProject(proj)
  on.exit(RQDA::closeProject())

  tb <- if (is.null(query)) {
    qry <- "sELECT treecode.cid AS cid,
            codecat.name AS codecat
            FROM treecode, codecat
            WHERE treecode.catid=codecat.catid AND codecat.status=1;"

    cdt <- RQDA::getCodingTable()

    cats <- RQDA::RQDAQuery(qry) %>%
      group_by(cid) %>%
      count(codecat)

    cdt$codecat <- NA

    for (i in seq_len(nrow(cats))) {
      ind <- which(cdt$cid %in% cats$cid[[i]])
      cdt$codecat[ind] <- cats$codecat[i]
    }
    cdt
  }
  else
    RQDA::RQDAQuery(query)
}
