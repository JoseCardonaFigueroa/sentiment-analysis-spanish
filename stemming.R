library(tm)
library(grep)
library(plyr)
corpusNoStem <- read.csv('data/Libro1.csv', header = F)
stopWords <- read.csv('data/lista de stopwords.csv', header = F)
cuantificadores <- read.csv('data/cuantificadores.csv', header = F)
sustantivos <- read.csv('data/sustantivos.csv', header = F)

corpus <- laply(corpusNoStem$termino, function(sentence, pos.words, neg.words){
  sentenceClean <- gsub('[[:punct:]]', "", sentence)
  sentenceClean <- gsub('[[:cntrl:]]', "", sentence)
  sentenceClean <- tolower(sentence)
  
  return(sentenceClean)
})

corpus <- Corpus(VectorSource(corpus))

corpus <- tm_map(corpus, removeWords, stopWords[,1])
corpus <- tm_map(corpus, removeWords, cuantificadores[,1])
corpus <- tm_map(corpus, removeWords, sustantivos[,1])
dataframe.corpus.limpio<-data.frame(text=unlist(sapply(corpus, `[`, "content")))
write.csv(dataframe.corpus.limpio, file = "data/CleanCorpus.csv", row.names = F)

corpus <- tm_map(corpus, stemDocument, language = "spanish")  
dataframe.corpus.limpio<-data.frame(text=unlist(sapply(corpus, `[`, "content")))
dataframe.corpus.limpio<-merge(dataframe.corpus.limpio, corpusNoStem)
write.csv(dataframe.corpus.limpio, file = "data/StemCorpus.csv", row.names = F)

write.csv(dataframe.corpus.limpio, file = "data/stemSubjectivity.csv", row.names = F)
corpusNoStem <- read.csv('data/subjectivity.csv')
