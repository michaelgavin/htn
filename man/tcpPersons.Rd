% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tcpPersons.R
\docType{data}
\name{tcpPersons}
\alias{tcpPersons}
\title{Data - Personography for EEBO-TCP data}
\format{A dataframe with 15,582 observations and 2 variables.}
\usage{
tcpPersons
}
\description{
A dataframe containing a personography drawn from the EEBO-TCP
data.
}
\section{What it holds}{

The \code{tcpPersons} data was generated algorithmically by disambiguating
author references in the EEBO-TCP catalogue and by drawing stationers' names
from the \code{tcpEdges} data.

As of the initial release, the personography includes only the name and role
of each node.
}
\examples{
# Load the collection, then setup on your computer
data(tcpPersons)
View(tcpPersons)
}
\keyword{datasets}

