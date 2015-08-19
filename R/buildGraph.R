# 
#' Build the graph for a \code{docNetwork} object.
#' 
#' Converts a \code{docNetwork} object into an \code{igraph} object. This step
#' 
#' @param dnet A docNetwork object
#' 
#' @param draw Logical. If TRUE, will also draw the sociogram.
#' 
#' @return g An igraph object.
#' 
#' @examples
#' 
#' g = buildGraph(dnet, draw=T)
#' g = buildGraph(dnet)
#' 
#' @export
buildGraph <- function(dnet, draw = F, byVar=F) {
  tcps = dnet@index$TCP
#   edges = data(tcpEdges)
#   dnet@edges = edges[which(edges$TCP %in% tcps),]
  if(byVar == F) {
    edges = persToPersEdges(dnet@edges)
  } else {
    edges = dnet@edges
  }
  if(length(dnet@persons) == 0){
    g <- graph.data.frame(edges, directed = F)
  } else {
    g <- graph.data.frame(edges, dnet@persons, directed = F)
  }
  if(!draw){
    return(g)
  } else {
    drawGraph(dnet)
  }
}

persToPersEdges <- function(edges) {
  tcps = unique(as.character(edges$TCP))
  namesList = list()
#   for(i in 1:length(tcps)) {
#     sub = edges$name[which(edges$TCP == tcps[i])]
#     peeps = unique(sub)
#     peeps = as.character(peeps)
#     namesList[[i]] = peeps
#   }
  #browser()
  el = list()
  for(i in 1:length(tcps)){
    sorce = c()
    target = c()
    type = c()
    TCP = c()
    peeps = as.character(unique(edges$name[which(edges$TCP == tcps[i])]))
    for(j in 1:length(peeps)){
      noti = peeps[-j]
      sorce = c(sorce,rep(peeps[j],length(noti)))
      if(length(noti) >= 1){
        target = c(target,noti)
      } else {
        target = c(target, "NA")
      }
      type = c(type, rep("undirected", length(noti)))
      TCP = c(TCP, rep(as.character(tcps[i]), length(noti)))
    }
    
#     all = cbind(sorce, target)
#     rev = cbind(target, sorce)
#     all = rbind(all, rev)
#     final = as.data.frame(unique(all), stringsAsFactors = F)
#     #browser()
#     sorce = final$sorce
#     target = final$target
    if(!(length(sorce) < 1)) {
      mat = cbind(sorce,target,type,TCP)
      el[[i]] = mat
    }
    
  }
  el = do.call(rbind, el)
  el = data.frame(el)
  el$weight = 1
  el = aggregate(el$weight,by=as.list(el[,1:4]),FUN=sum)
  names(el) = c("source","target","type", "TCP", "weight")
  return(el)
}