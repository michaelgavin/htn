#' Import a personography into a \code{docNetwork} object.
#' File must be in the format: name,tcp
#' 
#' @param   dnet a \code{docNetwork} object to add the data to.
#' 
#' @param   path A string that gives the full path to the csv
#'               formatted data file.
#'             
#' @return dnet The modified dnet object.
#' 
#' @examples
#' 
#' dnet = importPersonography(dnet, "path/to/file/data.csv")
#'
importPersonography <- function(dnet,path ="") {
  dnet@persons = read.csv(path)
  return(dnet)
}