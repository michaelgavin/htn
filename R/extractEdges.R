#' Gets network data from EEBO-TCP index
#'
#'  A function that extracts edges from an index.
#'  The edges are derived from the data included with
#'  this package. For whichever TCP files are included in your
#'  \code{docNetwork} index, this function finds the corresponding
#'  edges from the total data.
#'  
#'  @param dnet  A \code{docNetwork} object that contains an
#'               \code{index} that has \code{TCP} numbers.
#'  
#'  @return edges A \code{data.frame} that contains edge data
#'                in the form: \code{name}, \code{TCP}.
extractEdges <- function(dnet) {
  tcps = unique(dnet@index$TCP)
  data(tcpEdges)
  edges = tcpEdges[which(tcpEdges$TCP %in% tcps),]
  return(edges)
}

extractPersons <- function(dnet) {
  data(tcpPersons)
  ids = unique(c(as.character(dnet@edges$SOURCE), as.character(dnet@edges$TARGET)))
  persons = tcpPersons[which(tcpPersons$ID %in% ids),]
  return(persons)
}