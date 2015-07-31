# Creates a data frame of names and there associated TCP numbers
buildAuths = function(auths, tcp) {
  trim.leading <- function (x)  sub("^\\s+", "", x)
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