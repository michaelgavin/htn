#'
#'  A function that extracts edges from a dnet object's index.
#'  The edges are derived from the sample data included with
#'  this package and based upon the \code{TCP} numbers included
#'  in the \code{docNetwork}'s \code{index}.
#'  
#'  @param dnet  A \code{docNetwork} object that contains an
#'               \code{index} that has \code{TCP} numbers.
#'  
#'  @return edges A \code{data.frame} that contains edge data
#'                in the form: \code{name}, \code{TCP}.
#'  
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