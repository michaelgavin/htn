# For each of the parameters (other than the docList), the user can either provide a data.frame, a character
# string that connects to a .csv file, or let the default function extract the names.

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
  return(dnet)
}

# Not sure how you'd like to pass parameters into extractNames(). 
extractNames <- function(dl, colNum = 0) {
  index   <- dl@index
  if(colNum == 0) {
    print("Column number not defined, trying dl@index$Author for authors.")
    authors <- index$Author # May need to create a condition. Needs warning message.
  } else {
    authors <- index[colNum]
  }
  names   <- data.frame() # Create df
  for (i in 1:length(authors)) {
    if(i > 1) {
      names = rbind(names, buildAuths(authors[i], index$TCP[i]))
    } else {
      names = buildAuths(authors[i], index$TCP[i])
    }
  }
  return(names)
}

trim.leading <- function (x)  sub("^\\s+", "", x)
buildAuths = function(auths, tcp) {
  names = c()
  tcps = c()
  if(auths != "") {
    for(i in 1:length(auths)) {
      newAuths = as.character(auths[i])
      newAuths = strsplit(newAuths, split=';')
      for(j in 1:length(newAuths[[1]])) {
        auth = newAuths[[1]][j]
        auth = strsplit(auth, split=',')
        auth = unlist(auth)
        auth = trim.leading(auth)
        #print(auth)
        #print(auth[3])
        name = paste(auth[2], auth[1], paste('(', auth[3], ')', sep=""), sep=" ")
        #print(auth)
        if(length(name) >0) {
          names[[j]] = name
        } else {
          names[j] = NA
        }
      }
    }
    #names(names) = tcp
    tcps = rep(tcp, length(names))
    df = data.frame(names, tcps)
    names(df) = c("name", "tcp")
    ##browser()
    return(df)
  } else {
    names = NA
    tcps = tcp
    df = data.frame(names, tcp)
    names(df) = c("name", "tcp")
    ##browser()
    return(df)
  }
}


extractEdges <- function(dnet) {
  # For each item in dnet@names, distribute into undirected edgelist
  if(class(dnet) == "docList") {
    names = extractNames(dnet)
    dnet = docNetwork()
    dnet@names = names
  }
  tcps = unique(dnet@names$tcp)
  namesList = list()
  for(i in 1:length(tcps)) {
    sub = dnet@names$name[which(dnet@names$tcp == tcps[i])]
    ##browser()
    peeps = unique(sub)
    #peeps = as.character(peeps)
    namesList[[i]] = peeps
  }
  #browser()
  el = list()
  for(i in 1:length(namesList)){
    sorce = c()
    target = c()
    type = c()
    TCP = c()
    peeps = as.character(namesList[[i]])
    for(j in 1:length(peeps)){
      noti = peeps[-j]
      ##browser()
      sorce = c(sorce,rep(peeps[j],length(noti)))
      if(length(noti) >= 1){
        target = c(target,noti)
      } else {
        target = "NA"
      }
      ##browser()
      type = c(type, rep("undirected", length(noti)))
      TCP = c(TCP, rep(dnet@index$TCP[i], length(noti)))
    }
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
  ##browser()
  return(el)
}

extractPersons <- function(g, filename="personography.csv") {
  persons = V(g)
  names = c()
  for(i in 1:length(persons)){
    names[i] = persons[i]$name
  }
  write.csv(names, paste(dnet@directory, filename, sep="/"))
  names= data.frame(names)
  return(names)
}

addNames <- function(dnet, file, append = T, delim = "; ") {
  nameData <- read.csv(file)
  newNames <- list()
  for (i in 1:nrow(nameData)) {
    newNames[[i]] <- unlist(strsplit(nameData[i,1], sep = delim))
  }
  if (append == T) {
    dnet@names <- c(dnet@names, newNames)
  } else {
    dnet@names <- newNames
  }
  return(dnet)
}

# Adding and removing data from objs

importPersonography <- function(dnet,path ="") {
  dnet@persons = read.csv(path)
}

addData <- function() {
  # Passes data from a data.frame to either dnet@edges or dnet@persons
  # For example, distribute results from MALLET across the edgelist
}


extractData <- function() {
  # This will take edgelist data and reduce it down to dl@index format, so 
  # users can filter books by network data, or authors / persons.
}

buildGraph <- function(dnet, plot=F) {
  if(length(dnet@persons) == 0){
    g <- graph.data.frame(dnet@edges, directed = F)
  } else {
    g <- graph.data.frame(dnet@edges, dnet@persons, directed = F)
  }
  dnet@graph = g
  if(!plot){
    return(g)
  } else {
    drawGraph(dnet)
  }
}

drawGraph = function(dnet, sub=F, by=0, color='blue', labelSize=.75) {
  if(sub == T) {
    if(class(by) == "character") {
      subg = subgraph.edges(dnet@graph, eids=E(dnet@graph)[which(E(dnet@graph)$TCP %in% by)])
    } else {
      subg = subgraph(g, v=V(dnet@graph)[which(dnet@communities$membership == by)])
    }
    V(subg)$color = color
    V(subg)$label.cex = labelSize
    plot(subg)
    return(subg)
  } else {
    V(dnet@graph)$color = color
    V(dnet@graph)$label.cex = labelSize
    plot(dnet@graph)
  }
}

detectCommunities <- function(g, type="walktrap") {
  if(type == "walktrap") {
    comms = walktrap.community(g)
  } else if(type == "edge.betweenness") {
    comms = edge.betweenness.community(g)
  } else if(type == "betweenness") {
    comms = betweenness(g)
  }
  dnet@community = comms
  viewCommunities(dnet)
  return(comms)
}

viewCommunities <- function(dnet) {
  comms = unique(dnet@communities$membership)
  names = c()
  comm = c()
  for(i in 1:length(comms)) {
    temp = names(membership(dnet@communities))[which(membership(dnet@communities) == comms[i])]
#     for(j in 1:length(temp)) {
#       if(j == 1) {
#         names[i] = temp[j]
#       } else {
#         names[i+j] = temp[j]
#       }
#     }
    names = c(names, temp)
    comm = c(comm, rep(comms[i], length(temp)))
  }
  browser()
  df = data.frame(names, comm)
  View(df)
}

getGraphDensity <- function(g){
  return(graph.density(g))
}

topNodes <- function(g, limit=50) {
  return(rev(sort(degree(g))))[1:limit]
}

# QUESTION: We probably want some way of helping our users keep track of
# their network analysis objects (community objects, degree, etc). Perhaps
# another S4 class that holds them? What do you think?

