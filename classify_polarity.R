classify_polarity <- function(textColumns,algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=TRUE,...) {
  
  sink("output.txt")
  matrix <- create_matrix(textColumns,...)
  #lexicon <- read.csv("data/subjectivitynorepeated.csv",header=FALSE)
  #lexicon <- read.csv("data/subjectivityStemming.csv",header=FALSE)
  lexicon <- read.csv("data/subjectivityStemming2.csv",header=FALSE)
	counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))
	documents <- c()
	comneg <- 0
	compos <- 0
	comneu <- 0

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(positive=0,negative=0)
		doc <- matrix[i,]
		words <- findFreqTerms(doc,lowfreq=1)
		negation <- as.integer(0)
		
		for (word in words) {
			index <- match(word,lexicon[,1], nomatch=0)
			if (!is.na(match(word, 'no'))) {
			  negation <-as.integer(negation + 1)
			}
			if (index > 0) {
				entry <- lexicon[index,]
				
				polarity <- as.character(entry[[2]])
				category <- as.character(entry[[3]])
				count <- counts[[category]]
	
				score <- pweak
                if (polarity == "strongsubj") score <- pstrong
				if (algorithm=="bayes") score <- abs(log(score*prior/count))
		
				if (verbose) {
                    print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
				}
				
				scores[[category]] <- scores[[category]]+score
			}		
		}
		
		if (algorithm=="bayes") {
			for (key in names(scores)) {
				count <- counts[[key]]
				total <- counts[["total"]]
				score <- abs(log(count/total))
				scores[[key]] <- scores[[key]]+score
			}
		} else {
			for (key in names(scores)) {
				scores[[key]] <- scores[[key]]+0.000001
			}
		}
		
		#best_fit <- names(scores)[which.max(unlist(scores))]
		#   ratio <- (abs(scores$positive/scores$negative))
		#   if (identical(ratio,1)==TRUE) best_fit <- "neutral"
		if (scores$positive>scores$negative) 
		{
		  best_fit <- "positive" 
		  compos <- compos + 1
		}
		else if (scores$positive<scores$negative)
		{
		  best_fit <- "negative" 
		  comneg <- comneg + 1
		}
		
		else if (all(scores$positive == scores$negative) == TRUE)
		{
		  best_fit <- "neutral" 
		  comneu <- comneu + 1
		}
		   
		documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit, negation))
		if (verbose) {
			print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
			cat("\n")
		}
	}
	write.table(documents, file = "check_polarity.csv", append = FALSE, quote = FALSE, sep = ";",eol = "\n", na = "NA")
	print(paste("Positivos: ",compos,"Negativos: ",comneg,"Neutrales: ",comneu))
	cat("\n")
	sink()
	colnames(documents) <- c("POSITIVO","NEGATIVO","POSITIVO/NEGATIVO","BEST_FIT", "NEGATION WORDS")
	return(documents)
}

