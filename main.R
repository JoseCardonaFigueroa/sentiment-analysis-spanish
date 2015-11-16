library(ggplot2)
library(tm)
source('classify_polarity.R')
source('create_matrix.R')
source('classify_emotion.R')

corpus2 <- read.csv("data/corpusWithOneSpace.csv")
# classify emotion
class_emo = classify_emotion(corpus2$text, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(corpus2$text, algorithm="bayes", minWordLength = 1)
# get polarity best fit
polarity = class_pol[,4]

#PROBANDO C?DIGO DE NAIVE BAYES
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
