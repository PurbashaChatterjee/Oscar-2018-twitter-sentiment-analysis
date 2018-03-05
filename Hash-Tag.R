library(twitteR)
tw = userTimeline("TheAcademy", n = 5500)
tw = twListToDF(tw)
vec1 = tw$text

#Extract the hashtags:
hash.pattern = "#[[:alpha:]]+"

#stores the indices of the tweets which have hashes
have.hash = grep(x = vec1, pattern = hash.pattern) 
hash.matches = gregexpr(pattern = hash.pattern,
                        text = vec1[have.hash])

#Storing actual hashtags
extracted.hash = regmatches(x = vec1[have.hash], m = hash.matches) 

#dataframe formed with var1(hashtag), freq of hashtag
df = data.frame(table(tolower(unlist(extracted.hash)))) 
colnames(df) = c("tag","freq")
df = df[order(df$freq,decreasing = TRUE),]

#Fetching and reordering with highest freq
dat = head(df,50)
dat2 = transform(dat,tag = reorder(tag,freq)) 


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Oscar (@TheAcademy)")