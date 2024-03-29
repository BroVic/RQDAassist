% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codings.R
\name{retrieve_codingtable}
\alias{retrieve_codingtable}
\title{Retrieve Tabulated Codings}
\usage{
retrieve_codingtable(proj, query = NULL)
}
\arguments{
\item{proj}{Path to an RQDA project i.e. must have the file extension
\emph{.rqda}.}

\item{query}{A string; an SQL query.}
}
\value{
A data frame containing details about codings, including the
code name, filenames, starting and ending index, and code categories (if
applicable).
}
\description{
Fetches a table that has the codings as well as other details
related to codings as stored in the database.
}
\details{
The default query selects the following columns from the RQDA
file: \code{cid}  from the \code{treecode} table and \code{name} from
the \code{codecat} table. (For details on the database refer to
\code{RQDA::RQDATables})
}
