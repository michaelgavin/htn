#' Get index of documents from a community
#' 
#' A function that allows a user to extract bibliographic data from
#' communities within a larger network graph. Returns a subset of
#' the index that shows which documents make up a selected community.
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
#' @return df    A subset of \code{dnet}'s \code{index} for all nodes in a community.
#' 
#' @examples
#' data(dnet)
#' df = communityIndex(dnet, community = 3, method = "all")
#' 
#' 
#' @export
communityIndex <- function(dnet, community=NULL, method = "internal") {
  # This will take edgelist data and reduce it down to dl@index format, so 
  # users can filter books by network data, or authors / persons.
  if (method %in% c("all", "internal") == F) {
    stop("Method must be set to 'all' or 'internal'.")
  }
  if(is.null(dnet@communities)) {
    stop("Your network does not have communities detected. Please check @communities for data, and run detectCommunities().")
  }
  comms = dnet@communities$membership
  hits = which(comms == community)
  if (length(hits) == 0) {
    stop("There aren't any communities with that membership number in your dnet@communities object.")
  }
  if (method == "internal") {
    subg = suppressWarnings(subgraph(dnet@graph, v=V(dnet@graph)[hits]))
    tcps = sort(unique(E(subg)$TCP))
    df = dnet@index[which(dnet@index$TCP %in% tcps),]
    return(df)
  }
  if (method == "all") {
    tcps = c()
    for (i in 1:length(hits)) {
      tcp = incident(graph = dnet@graph,v = hits[i],mode = "all")$TCP
      tcps = c(tcps,tcp)
    }
    tcps = sort(unique(tcps))
    df = dnet@index[which(dnet@index$TCP %in% tcps),]
    return(df)
  }
}