Target_tweets1 = searchTwitter("Target+Store", n=2500, lang="en",since='2017-02-20',until='2017-02-25',retryOnRateLimit=10)
> 
> df <- do.call("rbind", lapply(Target_tweets1, as.data.frame))
> df<-rbind(df,file)
> df <- df[!duplicated(df[c("id")]),]
> write.csv(df,file="tweets1.csv",row.names=FALSE)
