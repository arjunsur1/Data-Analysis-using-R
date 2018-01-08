install.packages("tm")
library(tm)


#Step 1: Read in the positive and negative word files  
#Reading positive word file
pos <- "positive-words.txt"
#Reading Negative word file
neg <- "negative-words.txt"

#read the files
p <- scan(file.choose(), character(0),sep = "\n") # separate each word

n <- scan(file.choose(), character(0),sep = "\n") # separate each word
 
#remove the first 34 lines (header info)
p
n

p <- p[-1:-34]

n <- n[-1:-34]

# Checking the head
head(p,10)
head(n,10)

#Step 2 Process in the MLK speech

speechFile <- readLines(file.choose())
str(speechFile)

words.vec <- VectorSource(speechFile)
words.corpus <- Corpus(words.vec)
words.corpus

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

docmatrix <- TermDocumentMatrix(words.corpus)
docmatrix

mat <- as.matrix(docmatrix)
mat
wordCounts <- rowSums(mat)
wordCounts <- sort(wordCounts, decreasing=TRUE)
wordCounts
str(wordCounts)
head(wordCounts)


#Creating Word Cloud
install.packages("wordcloud")
library(wordcloud)
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq)

#calculate the total number of words

totalWords <- sum(wordCounts)
#have a vector that just has all the words
words <- names(wordCounts)

#Matching positive words
matched <- match(words, p, nomatch = 0) 
head(matched,10)

matched[2]
p[766]
words[2]

#Positive words
mCounts <- wordCounts[which(matched != 0)]
mCounts
length(mCounts)

mWords <- names(mCounts)
mWords
nPos <- sum(mCounts)
nPos
 # Ratio of Positive words
ratioPos <- (nPos/totalWords) * 100
ratioPos


##########Negative Words
matchedNegative <- match(words, n, nomatch = 0) 
head(matchedNegative,10)

nwords <- matchedNegative[which(matched!=0)]
nwords
NegativeCounts <- wordCounts[which(matchedNegative!=0)]
length(NegativeCounts)


NegativeWords <- names(NegativeCounts)
nNeg <- sum(NegativeCounts)
nNeg

# Ratio of Negative words
ratioNeg <- (nNeg/totalWords) * 100
ratioNeg

#Sentiment 
sentiment <- (nPos/nNeg) * 100
sentiment
####25%################################

length(speechFile)
speechFile25 <- speechFile[1:round((length(speechFile)/4))]
speechFile25

words.vec25 <- VectorSource(speechFile25)
words.corpus25 <- Corpus(words.vec25)
words.corpus25

words.corpus25 <- tm_map(words.corpus25, content_transformer(tolower))
words.corpus25 <- tm_map(words.corpus25, removePunctuation)
words.corpus25 <- tm_map(words.corpus25, removeNumbers)
words.corpus25 <- tm_map(words.corpus25, removeWords, stopwords("english"))

docmatrix25 <- TermDocumentMatrix(words.corpus25)
docmatrix25

mat25 <- as.matrix(docmatrix25)
mat25
wordCounts25 <- rowSums(mat25)
wordCounts25 <- sort(wordCounts25, decreasing=TRUE)
wordCounts25
str(wordCounts25)
head(wordCounts25)

#calculate the total number of words

totalWords25 <- sum(wordCounts25)
#have a vector that just has all the words
words25 <- names(wordCounts25)
words25
#Matching positive words
matched25 <- match(words25, p, nomatch = 0) 
head(matched25)

#Positive words
mCounts25 <- matched25[which(matched25 != 0)]
mCounts25
length(mCounts25)

positivewords25 <- wordCounts25[which(matched25 != 0)]
positivewords25

mWords25 <- names(positivewords25)
mWords25
nPos25 <- sum(positivewords25)
nPos25
# Ratio of Positive words for 25%
ratioPos25 <- (nPos25/totalWords25) * 100
ratioPos25

#Negative Words
matchedNegative25 <- match(words25, n, nomatch = 0) 
head(matchedNegative25,10)

nwords25 <- matchedNegative25[which(matchedNegative25!=0)]
nwords25
NegativeCounts25 <- wordCounts25[which(matchedNegative25!=0)]
length(NegativeCounts25)

negativewords25 <- names(NegativeCounts25)
negativewords25

nNeg25 <- sum(NegativeCounts25)
nNeg25

# Ratio of Negative words 25
ratioNeg25 <- (nNeg25/totalWords25) * 100
ratioNeg25

#Sentiment 
sentiment25 <- (nPos25/nNeg25) * 100
sentiment25

#####Comparing the results####

results <- data.frame(ratioPos,ratioNeg,ratioPos25,ratioNeg25)
results
results <- t(results)
results
colnames(results) <- c("Percentage")
str(results)

class(results)

results <- as.data.frame(results)

barplot(results$Percentage,xlab="Comparision of Ratios of 4 Numbers",ylab="Percentage of Values", ylim=c(0,12),main="Comparison",names.arg = c("Ratio of Positive","Ratio of Negative","Ratio of Positive 25%","Ratio of Negative 25%"),col="#8a9fc6")

