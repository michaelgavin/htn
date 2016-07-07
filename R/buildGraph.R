#' Build the graph for a \code{docNetwork} object.
#' 
#' Converts a \code{docNetwork} object into an \code{igraph} object. This step
#' 
#' @param dnet A docNetwork object
#' 
#' 
#' @return g An igraph object.
#' 
#' @examples
#' 
#' g = buildGraph(dnet, draw=T)
#' g = buildGraph(dnet)
#' 
#' @export
buildGraph <- function(dnet) {
  edges = dnet@edges
  if(length(dnet@persons) == 0){
    g <- graph.data.frame(edges, directed = F)
  } else {
    g <- graph.data.frame(edges, dnet@persons, directed = F)
  }
  return(g)
}

persToPersEdges <- function(edges) {
  tcps = unique(as.character(edges$TCP))
  namesList = list()
  el = list()
  for(i in 1:length(tcps)){
    sorce = c()
    target = c()
    type = c()
    TCP = c()
    hits = which(edges$TCP == tcps[i])
    peeps = unique(c(as.character(edges$SOURCE[hits]), as.character(edges$TARGET[hits])))
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
    
    if(!(length(sorce) < 1)) {
      mat = cbind(sorce,target,type,TCP)
      el[[i]] = mat
    }
    
  }
  el = do.call(rbind, el)
  el = data.frame(el)
  el$WEIGHT = 1
  names(el) = c("SOURCE","TARGET","TYPE", "TCP", "WEIGHT")
  for (i in 1:nrow(el)) {
    print(i)
    peeps = el[i,1:2]
    peeps = sort(peeps)
    el[i,1:2] = peeps
  }
  el = aggregate(el$WEIGHT, by = as.list(el[,1:4]), FUN = sum)
  el$WEIGHT = 1
  return(el)
}



