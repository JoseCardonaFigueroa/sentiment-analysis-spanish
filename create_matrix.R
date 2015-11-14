create_matrix <- function(textColumns, language="spanish", minDocFreq=1, minWordLength=0, removeNumbers=TRUE, removePunctuation=TRUE, removeSparseTerms=0, removeStopwords=FALSE, stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, weighting=weightTf) {
	
    stem_words <- function(x) {
        split <- strsplit(x," ")
        return(wordStem(split[[1]],language=language))
    }
	
	control <- list(language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stripWhitespace=stripWhitespace,minWordLength=minWordLength,stopwords=removeStopwords,minDocFreq=minDocFreq,weighting=weighting)
    
    if (stemWords == TRUE) control <- append(control,list(stemming=stem_words),after=6)
    
    trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
    trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")

	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=control);
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
	
	gc()
	return(matrix)
}