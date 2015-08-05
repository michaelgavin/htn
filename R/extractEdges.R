extractEdges <- function(dnet) {
  # For each item in dnet@names, distribute into undirected edgelist
#   if(class(dnet) == "docList") {
#     names = extractNames(dnet)
#     dnet = docNetwork()
#     dnet@names = names
#   }
  tcps = unique(dnet@index$TCP)
  namesList = list()
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
  return(el)
}

