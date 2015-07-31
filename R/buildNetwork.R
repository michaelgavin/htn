#' Build network
#' 
#' 
#' @section How to use it:
#' For each of the parameters (other than the docList), the user can either 
#' provide a data.frame, a character string that connects to a .csv file, 
#' or let the default function extract the names.
#'  
#'  
#' @export
buildNetwork <- function(dl, names = extractNames(dl), edges = NULL, persons = NULL) {
  dnet <- docNetwork()
  dnet@directory <- dl@directory
  dnet@index <- dl@index
  
  if (class(names) == "character" & length(names) == 1) { # If the parameter is set to a string, read csv
    names <- read.csv(names)
  } else {
    dnet@names <- names # otherwise just pass the value
  }
  if (class(edges) == "character" & length(edges) == 1) {
    edges <- read.csv(edges)
  } else {
    dnet@edges <- extractEdges(dnet)
  }
  
  if (class(persons) == "character" & length(persons) == 1) {
    persons <- read.csv(persons)
  } else {
    dnet@persons <- data.frame()
  }
  dnet@graph <- buildGraph(dnet = dnet)
  return(dnet)
}
