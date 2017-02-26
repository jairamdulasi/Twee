install.packages("twitteR")

library(twitteR)

install.packages("devtools")

require(devtools)

#Package sentiment has been archived so install it from archive location

install_url("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")

library(sentiment)

library(plyr)

library(ggplot2)

install.packages("wordcloud")

#Package slam has been archived so install it from archive location

install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")

library(slam)

library(wordcloud)

library(RColorBrewer)

#Set authentication with Twitter. Copy keys generated in step-1

api_key <- "API Key"

api_secret <- " API Secret "

access_token <- "Access token"

access_token_secret <- "Access token secret"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#Ingest tweets from twitter

#Here in this program search based on key word “Target” and number of #tweets ingested 1500, it can be increase/decrease by modifying parameter value of n

Target_tweets = searchTwitter("Target+Store", n=2500, lang="en",since='2017-02-24',until='2017-02-25',retryOnRateLimit=10)

# filter text from tweets


Target_text = sapply(Target_tweets, function(x) x$getText())

#Prepare/clean data for sentiment analysis

# delete re-tweet entries

Target_text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Target_text)

# remove @ word

Target_text = gsub("@\\w+", "", Target_text)

# delete punctuation

Target_text = gsub("[[:punct:]]", "", Target_text )

# remove Digits: 0 1 2 3 4 5 6 7 8 9

Target_text = gsub("[[:digit:]]", "", Target_text)

# delete html links

Target_text = gsub("http\\w+", "", Target_text)

# delete unnecessary spaces like tab and space

Target_text = gsub("[ \t]{2,}", "", Target_text)

Target_text = gsub("^\\s+|\\s+$", "", Target_text)

# define error handling function 

try.error = function(x)

{

 # create missing value

 y = NA

 # tryCatch error

 try_error = tryCatch(tolower(x), error=function(e) e)

 # if not an error

 if (!inherits(try_error, "error"))

   y = tolower(x)

 # result

 return(y)

}

Target_text = sapply(Target_text, try.error)

# remove NAs in Target_text

Target_text = Target_text [!is.na(Target_text)]

names(Target_text) = NULL

# Perform Sentiment Analysis by using naive bayes algorithm. 

#function classify_emotion is defined in “sentiment” package. 

#This function helps us to analyze some text and classify it in different types of #emotion: anger, disgust, #fear, joy, sadness, and surprise

class_emo = classify_emotion(Target_text, algorithm="bayes", prior=1.0)

# get emotion best fit

emotion = class_emo[,7]

# replace NA's by "unknown"

emotion[is.na(emotion)] = "unknown"

# function classify_polarity is defined in “sentiment” package.

# The classify_polarity function allows us to classify some text as positive or negative

class_pol = classify_polarity(Target_text, algorithm="bayes")

# get polarity best fit

polarity = class_pol[,4]

# Create data frame with the results and obtain some general statistics

# data frame with results

sent_df = data.frame(text= Target_text, emotion=emotion,

polarity=polarity, stringsAsFactors=FALSE)

# sort data frame

sent_df = within(sent_df,

emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of emotions

ggplot(sent_df, aes(x=emotion)) +

 geom_bar(aes(y=..count.., fill=emotion)) +

 scale_fill_brewer(palette="Dark2") +

 labs(x="emotion categories", y="number of tweets") +

 labs(title = "Sentiment Analysis of Tweets about Target\n(classification by emotion)")


ggplot(sent_df, aes(x=polarity)) +

 geom_bar(aes(y=..count.., fill=polarity)) +

 scale_fill_brewer(palette="Dark2") +

 labs(x="polarity categories", y="number of tweets") +

 labs(title = "Sentiment Analysis of Tweets about Target\n(classification by polarity)")


emos = levels(factor(sent_df$emotion))

nemo = length(emos)

emo.docs = rep("", nemo)

for (i in 1:nemo)

{

 tmp = Target_text[emotion == emos[i]]

 emo.docs[i] = paste(tmp, collapse=" ")

}



# remove stopwords

emo.docs = removeWords(emo.docs, stopwords("english"))

# create corpus

corpus = Corpus(VectorSource(emo.docs))

tdm = TermDocumentMatrix(corpus)

tdm = as.matrix(tdm)

colnames(tdm) = emos



# comparison word cloud

comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),

                scale = c(3,.5), random.order = FALSE, title.size = 1.5)
