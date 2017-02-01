library(KoNLP)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(arules)
library(combinat)

par(family="AppleGothic")

file_loc <- '/Users/KangHyungWon/Documents/Source/project/datamining/data.csv'

f <- read.csv(file_loc)

## TITLE TEXT MINING
myCorpus <- Corpus(VectorSource(f$title))
inspect(myCorpus)

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, '문의드립니다')
myCorpus <- tm_map(myCorpus, removeWords, '요청의')
myCorpus <- tm_map(myCorpus, removeWords, '요청드립니다')
myCorpus <- tm_map(myCorpus, removeWords, '못하는')
myCorpus <- tm_map(myCorpus, removeWords, '관련하여')
myCorpus <- tm_map(myCorpus, removeWords, '이슈입니다')
myCorpus <- tm_map(myCorpus, removeWords, '발생하는')
myCorpus <- tm_map(myCorpus, removeWords, '사용시')
myCorpus <- tm_map(myCorpus, removeWords, '실행시')
myCorpus <- tm_map(myCorpus, removeWords, '오류의')
myCorpus <- tm_map(myCorpus, removeWords, '안됩니다')
myCorpus <- tm_map(myCorpus, removeWords, 'error')
myCorpus <- tm_map(myCorpus, removeWords, '이동시')
myCorpus <- tm_map(myCorpus, removeWords, '확인요청')
myCorpus <- tm_map(myCorpus, removeWords, '수정요청')
myCorpus <- tm_map(myCorpus, removeWords, '의')
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, removeNumbers)

myDTM = TermDocumentMatrix(myCorpus)

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
d = data.frame(word =names(v),freq=v)
pal <- brewer.pal(5,"Set1")

wordcloud(words = d$word, freq = d$freq, min.freq = 2,scale=c(5,.9),max.words=200, random.order=FALSE, rot.per=0.1,random.color=T,colors=pal)

## CONTENT TEXT MINING

f$content

contentCorpus = Corpus(VectorSource(f$content))

inspect(contentCorpus)

contentCorpus <- tm_map(contentCorpus, content_transformer(tolower))
contentCorpus <- tm_map(contentCorpus, removeWords, stopwords('english'))
contentCorpus <- tm_map(contentCorpus, removeWords, '문의드립니다')
contentCorpus <- tm_map(contentCorpus, removeWords, '요청의')
contentCorpus <- tm_map(contentCorpus, removeWords, '요청드립니다')
contentCorpus <- tm_map(contentCorpus, removeWords, '못하는')
contentCorpus <- tm_map(contentCorpus, removeWords, '관련하여')
contentCorpus <- tm_map(contentCorpus, removeWords, '이슈입니다')
contentCorpus <- tm_map(contentCorpus, removeWords, '발생하는')
contentCorpus <- tm_map(contentCorpus, removeWords, '사용시')
contentCorpus <- tm_map(contentCorpus, removeWords, '실행시')
contentCorpus <- tm_map(contentCorpus, removeWords, '오류의')
contentCorpus <- tm_map(contentCorpus, removeWords, '안됩니다')
contentCorpus <- tm_map(contentCorpus, removeWords, 'error')
contentCorpus <- tm_map(contentCorpus, removeWords, '이동시')
contentCorpus <- tm_map(contentCorpus, removeWords, '확인요청')
contentCorpus <- tm_map(contentCorpus, removeWords, '수정요청')
contentCorpus <- tm_map(contentCorpus, removeWords, '의')
contentCorpus <- tm_map(contentCorpus, removeWords, 'span')
contentCorpus <- tm_map(contentCorpus, removePunctuation)
contentCorpus <- tm_map(contentCorpus, stripWhitespace)
contentCorpus <- tm_map(contentCorpus, removeNumbers)

contentDTM = TermDocumentMatrix(contentCorpus)

contentm = as.matrix(contentDTM)

contentv = sort(rowSums(contentm), decreasing = TRUE)
contentd = data.frame(word =names(contentv),freq=contentv)
pal <- brewer.pal(5,"Set1")

wordcloud(words = contentd$word, freq = contentd$freq, min.freq = 2,scale=c(5,.9),max.words=200, random.order=FALSE, rot.per=0.1,random.color=T,colors=pal)


 ## YEAR DATA EXTRACT

yearCorpus <- Corpus(VectorSource(f$YMD))

yearCorpus <- tm_map(yearCorpus, stripWhitespace)
yearCorpus <- tm_map(yearCorpus, removePunctuation)

yearDTM = TermDocumentMatrix(yearCorpus)

ym = as.matrix(yearDTM)

ymv = sort(rowSums(ym), decreasing = TRUE)
ymd = data.frame(word =names(ymv), freq=ymv)

ymd <- ymd[do.call(order, ymd), ]

newData = read.table('/Users/KangHyungWon/Documents/Source/project/datamining/data.txt')
barplot(ymd$freq, names.arg = ymd$word, border = NA, ylim = c(0, 200), las=1, ylab = "frequency", xlab = "Month")
titleList = list(f$title)

f$title

nouns <- sapply(titleList,extractNoun,USE.NAMES = F)

nouns <- sapply(nouns,extractNoun,USE.NAMES = F)
nouns
undata <- unlist(nouns)
typeof(f)
typeof(newData)

nouns <- Filter(function(x){nchar(x)>=2}, undata)
nouns <- gsub('요청', '', nouns)
nouns <- gsub('문의', '', nouns)
nouns <- gsub('관련', '', nouns)
nouns <- gsub('오류', '', nouns)
nouns <- gsub('문제', '', nouns)
nouns <- gsub('발생', '', nouns)
nouns <- gsub('발급', '', nouns)
nouns <- gsub('확인', '', nouns)
nouns <- gsub('1.', '', nouns)
nouns <- gsub('적용', '', nouns)
nouns <- gsub('4.', '', nouns)
nouns <- gsub('27', '', nouns)
nouns <- gsub('2.', '', nouns)
nouns <- gsub('8.', '', nouns)
nouns <- gsub('방문.', '', nouns)
nouns <- gsub('ios', 'iOS', nouns)
nouns <- gsub('IOS', 'iOS', nouns)
nouns <- gsub('Push', 'Push', nouns)
nouns <- gsub('PUSH', 'Push', nouns)
nouns <- gsub('push', 'Push', nouns)
nouns <- gsub('푸시', 'Push', nouns)

nouns <- gsub('33', '', nouns)

nouns2 <- nouns

names(nouns2) = paste("Tr", 1: length(nouns2), sep="")
wordtran <- as(nouns2, "transactions")


write(unlist(nouns), '/Users/KangHyungWon/Documents/data-table.txt')

revised <- read.table('/Users/KangHyungWon/Documents/data-table.txt')

wordcount <- table(revised)

wordcount <- wordcount[do.call(order, wordcount), ]

wordcount

pal <- brewer.pal(5,"Set1")
par(family='AppleGothic')

wordcloud(names(wordcount), freq=wordcount, scale=c(5,.5),rot.per=0.15,min.freq=3,max.words=200,random.order=F,random.color=T,colors=pal)

