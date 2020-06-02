library(readr)
library(tm)
library(wordcloud)

#Step 1: Read in the positive and negative word files 


positive <- "HW11 - positive-words.txt"

negitive <- "HW11 - negative-words.txt"

# separate each word
pos <- scan(file.choose(), character(0),sep = "\n") 
neg <- scan(file.choose(), character(0),sep = "\n") 

pos 
neg


#Clean the dataset

p <- pos[-1:-34]
n <- neg[-1:-34]

head(p,10)
head(n,10)

#Step 2: Process in the MLK speech

speech <- readLines(file.choose())
str(speech)

words.vec <- VectorSource(speech)
words.corpus <- Corpus(words.vec)
words.corpus

doc <- TermDocumentMatrix(words.corpus)
doc

m <- as.matrix(doc)
wordCounts <- rowSums(m)
head(wordCounts)

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

inspect(doc)
m = as.matrix(doc)
wordCounts = rowSums(m)

wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)
str(wordCounts)

total <- sum(wordCounts)
words <- names(wordCounts)
str(words)

#Step 3: Determine how many positive words were in the speech

countP <- match(words,p,nomatch=0)
countP
matchCounts <- wordCounts[which(countP	!=	0)]
length(matchCounts) #42 positive words

#Step 4: Determine how many negative words were in the speech

countN <- match(words,n,nomatch=0)
countN
matchCounts <- wordCounts[which(countN	!=	0)]
length(matchCounts) 

#Step 5: Redo the 'positive' and 'negative' calculations for each 25% of the speech

#split speech into 4 parts 
newdataframes = split(words, sample(rep(1:2:3:4, 459)))

#first quarter
matchedp1 = match(newdataframes$`1`,p,nomatch = 0)
matchcountsp1 = wordCounts[which(matchedp1	!=	0)]
p1 = length(matchcountsp1)

#second quarter
matchedp2 = match(newdataframes$`2`,p,nomatch = 0)
matchcountsp2 = wordCounts[which(matchedp2	!=	0)]
p2 = length(matchcountsp2)

#third quarter 
matchedp3 = match(newdataframes$`3`,p,nomatch = 0)
matchcountsp3 = wordCounts[which(matchedp3	!=	0)]
p3 = length(matchcountsp3)

#fourth quarter
matchedp4 = match(newdataframes$`4`,p,nomatch = 0)
matchcountsp4 = wordCounts[which(matchedp4	!=	0)]
p4 = length(matchcountsp4)

#barchart for comparing the result
barchartpos = c(p1,p2,p3,p4)
barplot(barchartpos)
#Barplot shows that the fourth quarter has got the most positive words

#similarly for the negative words 

#first quarter
matchedn1 = match(newdataframes$`1`,n,nomatch = 0)
matchcountsn1 = wordCounts[which(matchedn1	!=	0)]
n1 = length(matchcountsn1)

#second quarter
matchedn2 = match(newdataframes$`2`,n,nomatch = 0)
matchcountsn2 = wordCounts[which(matchedn2	!=	0)]
n2 = length(matchcountsn2)

#third quarter
matchedn3 = match(newdataframes$`3`,n,nomatch = 0)
matchcountsn3 = wordCounts[which(matchedn3	!=	0)]
n3 = length(matchcountsn3)

#fourth quarter
matchedn4 = match(newdataframes$`4`,n,nomatch = 0)
matchcountsn4 = wordCounts[which(matchedn4	!=	0)]
n4 = length(matchcountsn4)

#barchart 
barchartneg = c(n1,n2,n3,n4)
barplot(barchartneg)

