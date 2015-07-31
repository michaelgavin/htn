viewCommunities <- function(dnet) {
  comms = unique(dnet@communities$membership)
  names = c()
  comm = c()
  for(i in 1:length(comms)) {
    temp = names(membership(dnet@communities))[which(membership(dnet@communities) == comms[i])]
    names = c(names, temp)
    comm = c(comm, rep(comms[i], length(temp)))
  }
  Communities = data.frame(names, comm)
  colnames(Communities) = c("Name", "Community")
  View(Communities)
}