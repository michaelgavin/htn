#' Data - Personography for EEBO-TCP data
#' 
#' A dataframe containing a personography drawn from the EEBO-TCP
#' data.
#' 
#' @format A dataframe with 15,582 observations and 2 variables. 
#' 
#' @section What it holds:
#' The \code{tcpPersons} data was generated algorithmically by disambiguating
#' author references in the EEBO-TCP catalogue and by drawing stationers' names
#' from the \code{tcpEdges} data.
#' 
#' As of the initial release, the personography includes only the name and role
#' of each node.
#' 
#' @examples
#' # Load the collection, then setup on your computer
#' data(tcpPersons)
#' View(tcpPersons)
"tcpPersons"
