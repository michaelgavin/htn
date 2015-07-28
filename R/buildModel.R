buildModel = function(dl, dt, tnum, data.files = T, train = 100, maximize = 10, topic.list.length = 100 ) {
  model = docModel()
  model@directory = dl@directory
  model@index = dl@index
  files = names(dt@text)
  texts = c()
  for (i in 1:length(dt@text)) {
    texts[i] = paste(dt@text[[i]], collapse = " ")
  }
  write.table(dl@stopwords, file = "tempStops.txt", append = F, quote = F, sep = "\n",row.names = F, col.names = F)
  mallet.instances = mallet.import(files,
                                   texts,
                                   "tempStops.txt",
                                   preserve.case = FALSE,
                                   token.regexp="[\\p{L}']+")
  file.remove("tempStops.txt")
  tmod = MalletLDA(num.topics = tnum)
  tmod$loadDocuments(mallet.instances)
  tmod$train(100)
  tmod$maximize(10)
  model@malletObj = tmod
  if (data.files == T) {
    vocabulary = tmod$getVocabulary()
    twords = mallet.topic.words(tmod, normalized = T, smoothed = F)
    colnames(twords) = vocabulary
    model@termMatrix = twords
    
    tdocs = mallet.doc.topics(topic.model = tmod, normalized = T, smoothed = F)
    colnames(tdocs) = 1:ncol(tdocs)
    tdocs = round(tdocs, digits = 2)
    model@frequencies = tdocs
    
    tlist = list()
    for (i in 1:nrow(twords)) {
      t = mallet.top.words(tmod, twords[i,], topic.list.length)
      tlist[[i]] = t 
    }
    model@weights = tlist
    
    tlist.df = data.frame(1:nrow(tlist[[1]])) # Creates single column matrix
    for (i in 1:length(tlist)) {
      topic = tlist[[i]]$words # Pulls words (not frequency) from each topic
      tlist.df = cbind(tlist.df,topic) # Binds each column to existing tdf
    }
    topnames = paste("Topic",1:length(tlist), sep=" ") # Makes column names
    names(tlist.df)[2:ncol(tlist.df)] = topnames # Adds them
    tlist.df = tlist.df[,-1] # Removes placeholder column 
    model@topics = tlist.df
  }
  return(model)
}

