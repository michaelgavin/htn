#' Subset a network by community
#' 
#' A function that allows a user to extract a \code{docNetwork} from a larger
#' \code{docNetwork} .
#' 
#' @param dnet         The \code{docNetwork} object to modify.
#' 
#' @param community    Integer. The membership number of the community you want data about.
#'                     
#' @param method       Either "all" or "internal". If "all", results will include all
#'                     books attributed to all members of the community. If "internal",
#'                     results will include only books that are attributed to two or more
#'                     members of the community.
#'              
#' @return dnet    A subsetted \code{docNetwork} for all nodes in a community.
#' 
#' @examples
#' data(dnet)
#' df = communitySubnetwork(dnet, community = 3, method = "all")
#' 
#' 
#' @export
communitySubnetwork = function(dnet, community, method = "internal") {
  subnet = docNetwork()
  subnet@directory <- dnet@directory
  
  if (method %in% c("all", "internal") == F) {
    stop("Method must be set to 'all' or 'internal'.")
  }
  if(is.null(dnet@communities)) {
    stop("Your network does not have communities detected. Please check @communities for data, and run detectCommunities().")
  }
  comms = dnet@communities$membership
  hits = which(comms == community)
  
  g = dnet@graph
  
  subnet@graph = suppressWarnings(subgraph(g, V(g)[hits]))
  subnet@index <- communityIndex(dnet= dnet, community = community, method = method)
  return(subnet)
}