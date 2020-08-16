library(ggplot2) 
library(readr)
library(tm)
library(wordcloud)
library(plyr)
library(lubridate)
library(syuzhet)
library(BSDA)
library(RColorBrewer)
#Import the twitter data set
tweetsdata=read.csv('demonetization-tweets.csv',encoding = 'UTF-8',stringsAsFactors = FALSE)

summary(tweetsdata)

tweetsdata$created_date=as.Date(tweetsdata$created,format='%Y-%m-%d %H:%M:%S')
tweetsdata$hour = format(as.POSIXct(tweetsdata$created,format="%Y-%m-%d %H:%M:%S"),"%H")
tweetsdata$isRetweetNum=ifelse(tweetsdata$isRetweet==FALSE,0,1)
tweetsdata$retweetedNum=ifelse(tweetsdata$retweeted==FALSE,0,1)
tweetsdata$tweet=c(1)

HourFrame=as.data.frame(table(tweetsdata$hour))
colnames(HourFrame)=c("Hour","TweetCount")
HourFrame$Hour=as.numeric(HourFrame$Hour)
y=ddply(tweetsdata, .(tweetsdata$hour), numcolwise(sum))
HourFrame$retweetedNum=y$isRetweetNum
ggplot(HourFrame,aes(x=Hour))+geom_line(aes(y = TweetCount, colour = "TotalTweets")) + 
  geom_line(aes(y = retweetedNum, colour = "Retweets"))

devices=tweetsdata$statusSource
devices <- gsub("","", devices)
devices <- strsplit(devices, ">")
devices <- sapply(devices,function(x) ifelse(length(x) > 1, x[2], x[1]))
devices <- strsplit(devices, "<")
devices <- sapply(devices,function(x) ifelse(length(x) > 1, x[1], x[2]))

devices_source=as.data.frame(table(devices))
colnames(devices_source)=c("Device","TweetCount")
devices_source=devices_source[devices_source$TweetCount>50,]
devices_source=devices_source[order(-devices_source$TweetCount),]

ggplot(devices_source,aes(x=reorder(Device, -TweetCount),y=TweetCount,fill=TweetCount))+geom_bar(stat='identity') +coord_flip()

y=ddply(tweetsdata, .(screenName), numcolwise(sum))
popularUsers=y[,c("screenName","retweetCount","tweet")]
popularUsers=popularUsers[order(-popularUsers$retweetCount),]
popularUsers=head(popularUsers,n=10)
popularUsers

#Most Replies
Replies=tweetsdata[is.na(tweetsdata$replyToSN)==FALSE,]
y=ddply(Replies, .(replyToSN), numcolwise(sum))
Replies=y[,c("replyToSN","tweet")]
Replies=Replies[order(-Replies$tweet),]
Replies=head(Replies,n=20)
colnames(Replies)=c("User","RepliesReceived")
Replies

some_txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweetsdata$text)
some_txt<-gsub("http[^[:blank:]]+","",some_txt)
some_txt<-gsub("@\\w+","",some_txt)
some_txt<-gsub("[[:punct:]]"," ",some_txt)
some_txt<-gsub("[^[:alnum:]]"," ",some_txt)


tweetSentiment <- get_nrc_sentiment(some_txt)

coul <- brewer.pal(5, "Set2") 
barplot(sort(colSums(prop.table(tweetSentiment[, 1:8]))), cex.names = 0.7, las = 1, main = "Emotions in Tweets text", xlab="Percentage",col=coul)

corpus = tm::Corpus(tm::VectorSource(some_txt)) 

# Cleaning up 
# Handling UTF-8 encoding problem from the dataset 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8', sub='byte'))  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('english')) # Removing stop-words 
corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "english") # Stemming the words  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces

tdm <- tm::DocumentTermMatrix(corpus.cleaned[1:9000]) 
tdm.tfidf <- tm::weightTfIdf(tdm)

tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")


clustering.kmeans <- kmeans(tfidf.matrix,2,nstart=100)
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = 4) 
slave.dbscan <- clustering.dbscan$cluster 
stacked.clustering <- rep(NA, length(master.cluster))  
names(stacked.clustering) <- 1:length(master.cluster) 
for (cluster in unique(master.cluster)) { 
  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  slave1.votes <- table(slave.hierarchical[indexes]) 
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  slave2.votes <- table(slave.dbscan[indexes]) 
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  stacked.clustering[indexes] <- slave2.maxcount 
}



points <- cmdscale(dist.matrix, k = 2) 

palette <- colorspace::diverge_hcl(2) # Creating a color palette 
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
par(previous.par) # recovering the original plot space parameters
      


tc <- Corpus(VectorSource(some_txt))
tc <- tm_map(tc, removePunctuation)
tc <- tm_map(tc, content_transformer(tolower))
tc <- tm_map(tc, tm::removeWords, tm::stopwords('english'))
tc <- tm_map(tc, removeWords, c("00bd","will","00a0","amp","00b8","looking","for?"))

corpus <- TermDocumentMatrix(tc)
corpus <- as.matrix(corpus)
corpus <- sort(rowSums(corpus),decreasing=TRUE)
df <- data.frame(word = names(corpus),freq=corpus)

coul <- brewer.pal(5, "Set2") 
barplot(height=df[1:10,]$freq, names=df[1:10,]$word, col=coul ,horiz=F, las=1,xlab = "Words",ylab ="Frequency")

  
wordcloud(tc, min.freq = 50,
          max.words=1500, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale = c(3,0.5))    




positive_mean = mean(tweetSentiment$positive)
sigmap = sd(tweetSentiment$positive)
negative_mean = mean(tweetSentiment$negative)
sigman = sd(tweetSentiment$negative)

countp = length(tweetSentiment$positive)
countn = length(tweetSentiment$negative)

z_calcs1 = zsum.test(mean.x = positive_mean,sigma.x = sigmap,n.x = countp,mean.y = negative_mean,sigma.y =sigman,n.y = countn )
print(z_calcs1)
