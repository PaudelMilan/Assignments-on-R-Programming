library(pdftools)
library(tm)
pdf_path<- DirSource('E:/Milan/MDS/R by Sital Bhandary/MDS503P1/')
str(pdf_path$filelist)
combinedTextFile<-lapply(pdf_path$filelist,pdf_text)
str(combinedTextFile)

corpusFile<- Corpus(VectorSource(combinedTextFile))
inspect(corpusFile[1:3])

removeURL<- function(x)gsub("http[^[:space:]]*","",x)
corpusFile<- tm_map(corpusFile,removeURL)
inspect(corpusFile[1:3])

removeURL<- function(x)gsub("www[^[:space:]]*","",x)
corpusFile<- tm_map(corpusFile,removeURL)
inspect(corpusFile[1:3])


corpusFile<- tm_map(corpusFile,tolower)
inspect(corpusFile[1:3]) 

editContent<- content_transformer(function(x,pattern){gsub(pattern," ",x)})

corpusFile<- tm_map(corpusFile,editContent,"\\n")
corpusFile<- tm_map(corpusFile,editContent,"\\\\n")
#corpusFile<- tm_map(corpusFile,editContent,"\"")
corpusFile<- tm_map(corpusFile,editContent,"[^[:alpha:]\" \"]")

inspect(corpusFile[1:3])

#corpusFile<- tm_map(corpusFile,removeNumbers)
#inspect(corpusFile[1:3])

corpusFile<- tm_map(corpusFile,removePunctuation)
inspect(corpusFile[1:3])

corpusFile<- tm_map(corpusFile,removeWords,"[a-zA-Z]?")
inspect(corpusFile[1:3])

corpusFile<- tm_map(corpusFile,removeWords,stopwords())

str(corpusFile)
inspect(corpusFile[1:3])

corpusFileCopy<- corpusFile
corpusFileCopy<- tm_map(corpusFileCopy,stemDocument)

#corpusFileCopy<- tm_map(corpusFileCopy,stemCompletion,dictionary=corpusFile)

inspect(corpusFileCopy[1:3])

corpusFileCopy<- TermDocumentMatrix(corpusFileCopy,control=list(wordLengths=c(1,Inf)))
str(corpusFileCopy)

sortedMatrix<- sort(rowSums(as.matrix(corpusFileCopy)),decreasing = TRUE)
head(sortedMatrix,n = 50)

install.packages("wordcloud")
library(wordcloud)
wordcloud(words=names(sortedMatrix), freq=sortedMatrix, min.freq=10,random.order=F)

library(RColorBrewer)
wordcloud(words=names(sortedMatrix), freq=sortedMatrix, min.freq=20,random.order=F,colors = brewer.pal(12,"Paired"))

plot(corpusFileCopy,terms = sample(Terms(corpusFileCopy), 20),corThreshold = 0.1, weighting = TRUE)

library(topicmodels)
set.seed(123)
myLda <- LDA(as.DocumentTermMatrix(corpusFileCopy), k=5)
terms(myLda, 4)
