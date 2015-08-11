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
  namesList = list()
  data(tcpEdges)
  edges = tcpEdges[which(tcpEdges$TCP %in% tcps),]
  # For each item in dnet@names, distribute into undirected edgelist
#   if(class(dnet) == "docList") {
#     names = extractNames(dnet)
#     dnet = docNetwork()
#     dnet@names = names
#   }
  
#   for(i in 1:length(tcps)) {
#     sub = dnet@names$name[which(dnet@names$tcp == tcps[i])]
#     peeps = unique(sub)
#     #peeps = as.character(peeps)
#     namesList[[i]] = peeps
#   }
#   el = list()
#   for(i in 1:length(namesList)){
#     sorce = c()
#     target = c()
#     type = c()
#     TCP = c()
#     peeps = as.character(namesList[[i]])
#     for(j in 1:length(peeps)){
#       noti = peeps[-j]
#       sorce = c(sorce,rep(peeps[j],length(noti)))
#       if(length(noti) >= 1){
#         target = c(target,noti)
#       } else {
#         target = "NA"
#       }
#       type = c(type, rep("undirected", length(noti)))
#       TCP = c(TCP, rep(dnet@index$TCP[i], length(noti)))
#     }
#     if(!(length(sorce) < 1)) {
#       mat = cbind(sorce,target,type,TCP)
#       el[[i]] = mat
#     }
#     
#   }
#   el = do.call(rbind, el)
#   el = data.frame(el)
#   el$weight = 1
#   el = aggregate(el$weight,by=as.list(el[,1:4]),FUN=sum)
#   names(el) = c("source","target","type", "TCP", "weight")
  return(edges)
}

