library(quanteda)
library(e1071)
library(NLP)
library(tm) # Text mining
library(SnowballC)
library(plyr)
library(stringr)
library(MASS)
library(sentiment)
library(ggplot2)
library(wordcloud)

#Limpieza

corpus <- read.csv('C:\\Users\\Cardona\\Documents\\UIII\\corpusCommentsStem.csv')
corpus$opinar

corpus <- laply(corpus$text, function(sentence){
  sentence <- gsub('[[:punct:]]', '', sentence)
  sentence <- gsub('[[:cntrl:]]', '', sentence)
  sentence <- gsub('\\d+', '', sentence)
  sentence <- tolower(sentence)
})

custom.stopwords<- read.csv('C:\\Users\\Posgrado\\Desktop\\UI_DataMining\\UII_DataMining\\lista de stopwords.csv')
cuantificadores <- read.csv('C:\\Users\\Posgrado\\Desktop\\UI_DataMining\\UII_DataMining\\cuantificadores.csv')
sustantivos <- read.csv('C:\\Users\\Posgrado\\Desktop\\UI_DataMining\\UII_DataMining\\sustantivos.csv')

#Corpus object creation using tm library
corpus <- Corpus(VectorSource(corpus))
corpus
#hashtag.to.remove<-c("asevd")
#corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
#corpus <- tm_map(corpus, removeWords, hashtag.to.remove)
corpus <- tm_map(corpus, removeWords, custom.stopwords$a)
corpus <- tm_map(corpus, removeWords, cuantificadores$a)
corpus <- tm_map(corpus, removeWords, sustantivos$a)

corpus[1:3]
#dataframe.corpus.limpio<-data.frame(text=unlist(sapply(corpus, `[`, "content")),
 #                                   stringsAsFactors=F)

dataframe.corpus.limpio<-data.frame(text=unlist(sapply(corpus, `[`, "content")))


#Corpus limpio guardado en Corpus2.csv
write.csv(dataframe.corpus.limpio, file = "C:\\Users\\Posgrado\\Desktop\\UI_DataMining\\UII_DataMining\\corpus2.csv", row.names = F)
corpus


#***********************************************************************
#La variable corpus2 toma el valor de los tweets del corpus22.csv (2000)

corpus2 <- read.csv('C:\\Users\\Cardona\\Documents\\UIII\\corpusCommentsStem.csv')


# classify emotion
class_emo = classify_emotion(corpus2, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(corpus2, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

#PROBANDO C?DIGO DE NAIVE BAYES



# data frame with results
#¿Cómo se va a realizar el análisis de emociones si la lista de emociones no está traducido?
sent_df = data.frame(text=corpus2, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Categor?as", y="Comentarios") +
  
    labs(title = "An?lisis de Emociones para la mejora del docente\n(Clasificaci?n de comentarios por polaridad)",
  plot.title = element_text(size=12))
