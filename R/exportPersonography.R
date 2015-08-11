#' Exports a personography from graph data.  The data is both returned
#' and written to a file with \code{filename}.
#' 
#' @param g         An igraph object.  The graph that the data should be extracted from.
#' @param filename  A string that gives the name of the file to store the
#'                  personography in.  The file will be saved relative to
#'                  the \code{directory} stored in your \code{docNetwork}
#'                  object. If you do not pass a \code{docNetwork} object
#'                  \code{filename} must be the full path for the file.
#' @param dnet      An optional reference to your \code{docNetwork} object
#'                  from which the \code{directory} to store \code{filename}
#'                  will be derived.
#'                  
#' @return personography A data frame that holds the names and tcp numbers of the
#'                       people extracted from the \code{g} object.
#'                       
#' @examples
#' 
#' personography = exportPersonography(g=g, filename="persons.csv", dnet=dnet)
#' 
exportPersonography <- function(g, filename="personography.csv", dnet=NULL) {
  persons = V(g)
  names = c()
  for(i in 1:length(persons)){
    names[i] = persons[i]$name
  }
  if(!(is.null(dnet))) {
    write.csv(names, paste(dnet@directory, filename, sep="/"))
  } else {
    write.csv(names, filename)
  }
  names= data.frame(names)
  return(names)
}