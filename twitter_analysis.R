

##install.packages("rtweet")
library(rtweet)
library(syuzhet)
tweets_ds = search_tweets("#BREXIT", n=10000,lang="en", include_rts = F)
dim(tweets_ds)
# 
saveRDS(tweets_ds, "tweet_br.rds")

tweets_BR = readRDS("tweet_br.rds")

tweets_BR$text <- iconv(tweets_BR$text, "UTF-8", "ASCII", sub="")
sa.value <- get_nrc_sentiment(tweets_BR$text)
sa.value[1:5,1:7]
score <- colSums(sa.value[,])
# Convert to data frame
score_df <- data.frame(score)

# View the data frame
score_df
# Convert row names into 'sentiment' column
# Combine with sentiment scores
sa.score <- cbind(sentiment = row.names(score_df), 
                  score_df, row.names=NULL)

print(sa.score)

# Plot the sentiment scores
ggplot(data = sa.score, aes(x = sentiment, y = score, 
                            fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sc_name = sort(table(tweets_BR$screen_name), decreasing = TRUE)
head(sc_name, 10)
tweets_ds
install.packages("maps")
library(maps)
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
brtwt_coord = lat_lng(tweets_BR)
View(brtwt_coord)


# saveRDS(pollution_coord, "pollution_coord.rds")

brtwt = na.omit(brtwt_coord[,c("lat", "lng")])
View(brtwt)


world_basemap = ggplot() +
  borders("world",fill = "gray95")+theme_map()
# Plot the lat and lng points on the world map
world_basemap +
  geom_point(data = brtwt, aes(x = lng, y = lat),
             colour = 'blue', alpha = .5, size=2)



# Extract the tweet texts and save it in a data frame
twt_txt <- tweets_BR$text




# Remove URLs from the tweet text
library(qdapRegex)
twt_txt_url <- rm_twitter_url(twt_txt)
twt_txt_url[1:3]



# Remove special characters, punctuation & numbers
twt_txt_chrs  <- gsub("[^A-Za-z]", " ", twt_txt_url)
twt_txt_chrs[1:3]





# Convert to text corpus
library(tm)
twt_corpus <- twt_txt_chrs %>% 
  VectorSource() %>% 
  Corpus() 
twt_corpus[[3]]$content




# Convert text corpus to lowercase
twt_corpus_lwr <- tm_map(twt_corpus, tolower) 
twt_corpus_lwr[[3]]$content





# Common stop words in English
stopwords("english")




# Remove stop words from corpus
twt_corpus_stpwd <- tm_map(twt_corpus_lwr, removeWords, stopwords("english")) 
twt_corpus_stpwd[[3]]$content

sa.value <- get_nrc_sentiment(twt_corpus_stpwd$content)
sa.value[1:5,1:7]
score <- colSums(sa.value[,])
# Convert to data frame
score_df <- data.frame(score)

# View the data frame
score_df
# Convert row names into 'sentiment' column
# Combine with sentiment scores
sa.score <- cbind(sentiment = row.names(score_df), 
                  score_df, row.names=NULL)

print(sa.score)

# Plot the sentiment scores
ggplot(data = sa.score, aes(x = sentiment, y = score, 
                            fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sc_name = sort(table(tweets_BR$screen_name), decreasing = TRUE)
head(sc_name, 10)


# Remove additional spaces
twt_corpus_final <- tm_map(twt_corpus_stpwd, stripWhitespace) 
twt_corpus_final[[3]]$content



# Extract term frequency
library(qdap)
term_count  <-  freq_terms(twt_corpus_final, 60)
term_count




# Create a vector of custom stop words
custom_stop <- c("brexit", "s", "t", "amp", "can", "will", "just", 
                 "many", "still", "st", "re", "need", "may", "now", 
                 "get", "s", "t", "m", "re", "k", "us")



# Remove custom stop words
twt_corpus_refined <- tm_map(twt_corpus_final,removeWords, custom_stop)







# Term count after refining corpus
term_count_clean <- freq_terms(twt_corpus_refined, 20)
term_count_clean



# Create a bar plot of frequent terms
library(ggplot2)

# Create a subset dataframe
term50 <- subset(term_count_clean, FREQ > 100)




# Create a bar plot
ggplot(term50, aes(x = reorder(WORD,  -FREQ),  y = FREQ)) +
  geom_bar(stat = "identity", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Create a word cloud based on min frequency
library(wordcloud)
wordcloud(twt_corpus_refined, min.freq = 100, colors = "red", 
          scale = c(3,0.5), random.order = FALSE)




# Create a colorful word cloud
library(RColorBrewer)
wordcloud(twt_corpus_refined, max.words = 250, 
          colors = brewer.pal(5,"Dark2"), scale = c(1.5,.25),
          random.order = FALSE)


#Document term matrix

tweet_dtm <- DocumentTermMatrix(twt_corpus_refined)
tweet_tdm <- TermDocumentMatrix(twt_corpus_refined)
inspect(tweet_tdm)

rowTotals <- apply(tweet_dtm , 1, sum) #Find the sum of words in each Document
tweet_dtm_new   <- tweet_dtm[rowTotals> 0, ]
#install.packages("topicmodels")
library(topicmodels)
lda_5 = LDA(tweet_dtm_new, k=5)
top_10terms = terms(lda_5,10)
top_10terms


tweet_tdm2 <- removeSparseTerms(tweet_tdm, sparse = 0.98)
hc <- hclust(d = dist(tweet_tdm2, method = "euclidean"), method = "complete")
# Plot a dendrogram
plot(hc)


# Create associations
associations <- findAssocs(tweet_tdm, "brexit", 0.1)
# Create associations_df
associations_df <- list_vect2df(associations)[, 2:3]
# Plot the associations_df values 
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3)




