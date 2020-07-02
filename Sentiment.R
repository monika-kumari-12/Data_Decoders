library(twitteR)
library(rtweet)
library(ROAuth)
library(RCurl)
library(httr)
library(stringr)
library(plyr)
library(dplyr)
library(tm)
library("stringr")
library("plyr")
#library(ggmap)
#library(wordcloud)

key <- "psaR3hzNCtFBRCiYRA4CKYEP5"
skey <- "uRKjKbwe4ClO3fIPRpNAJF4mg14xObNmtLA3JhOK6tfANix2Je"
token <- "741997671737614336-AOponExGaebnPttzzBeNegB8oya49fp"
stoken <- "puDEWkAgR4VKiAsnOtURILt3aRAYBU0SiCWwoT45CMw9o"

setup_twitter_oauth(key,skey,token,stoken)


sentimentfun = function(tweettext, pos, neg, .progress='non')
{
  scores = laply(tweettext,
                 function(singletweet, pos, neg)
                 {
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   singletweet = gsub("\\d+", "", singletweet)
                
                   tryTolower = function(x)
                   {
                     y = NA
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   singletweet = sapply(singletweet, tryTolower)
                   
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos)
                   neg.matches = match(words, neg)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos, neg, .progress=.progress )
  sentiment.df = data.frame(text=tweettext, score=scores)
  return(sentiment.df)
}

tweets = searchTwitter("amazon+delivery", n=5000, 
                       lang="en")

tweettext = sapply(tweets, function(x) x$getText())
tweettext=lapply(tweettext, function(x) iconv(x, "latin1", 
                                              "ASCII", sub=""))
tweettext=lapply(tweettext, function(x) gsub("htt.*",' ',x))
tweettext=lapply(tweettext, function(x) gsub("#",'',x))
tweettext=lapply(tweettext, function(x) gsub("RT @",'',x))
tweettext=lapply(tweettext, function(x) gsub("@.*",'',x))
tweettext=unlist(tweettext)

pos = readLines("~/Desktop/Sentiment Analysis with R/positive-words.txt")
neg = readLines("~/Desktop/Sentiment Analysis with R/negative-words.txt")

neg2 = c(neg, "late", "fraud"); tail(neg2)

scores = sentimentfun(tweettext, pos, neg, .progress='text')
tweetdate=lapply(tweets, function(x) x$getCreated())
tweetdate=sapply(tweetdate,function(x) strftime(x, format="%Y-%m-%d %H:%M:%S",tz = "UTC"))

isretweet=sapply(tweets, function(x) x$getIsRetweet())

retweetcount=sapply(tweets, function(x) x$getRetweetCount())

favoritecount=sapply(tweets, function(x) x$getFavoriteCount())

data=as.data.frame(cbind(ttext=tweettext,
                         date=tweetdate,
                         isretweet=isretweet, 
                         retweetcount=retweetcount,
                         favoritecount=favoritecount,
                         score = scores$score,
                         city = "Delhi NCR", country = "India"))

data2 = duplicated(data[,1])

data$duplicate = data2
write.csv(data, file= "~/Desktop/Sentiment Analysis with R/Amazon_Tweets_R_5000.csv")
