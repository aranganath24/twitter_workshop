library(rtweet)
library(httpuv)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(qdapRegex)
library(tm)
library(webshot)
library(htmlwidgets)
library(tmap)
library(sf)

install.packages(c("rtweet", "httpuv"))

# Pulling Data from API ---------------------------------------------------

# Pull tweets with #CancelStudentDebt; returns 1000 most recent tweets; time by GMT
student_debt_tweets<-search_tweets(q="#CancelStudentDebt", 
                                   n=1000,
                                   include_rts=FALSE,
                                   `-filter`="replies",
                                   lang="en")
View(student_debt_tweets)

# write data 
write_as_csv(student_debt_tweets, "/Users/adra7980/Documents/git_repositories/twitter_workshop/exported_data/student_debt_tweets.csv")

# Pull tweets with #CancelStudentDebt AND capitalism
student_debt_capitalism_tweets<-search_tweets(q="#CancelStudentDebt capitalism", 
                                              n=1000,
                                              include_rts=FALSE,
                                              `-filter`="replies",
                                              lang="en")
View(student_debt_capitalism_tweets)

# Instead of pulling from the API, you could also pull tweets with #CancelStudentDebt, and then query the text
# of these tweets locally using a stringr function

student_debt_capitalism_tweets_ALT<-student_debt_tweets %>% 
                                      filter(str_detect(text, "[Cc]apitalism"))

View(student_debt_capitalism_tweets_ALT)

# Pull tweets with #CancelStudentDebt OR capitalism

student_debt_OR_capitalism_tweets<-search_tweets(q="#CancelStudentDebt OR capitalism", 
                                                 n=1000,
                                                 include_rts=FALSE,
                                                 `-filter`="replies",
                                                 lang="en")

View(student_debt_OR_capitalism_tweets)

# Pull tweets from an account (doesn't have same time constraints)
# Pull last 3200 BLM tweets
blm_tweets<-get_timeline("Blklivesmatter", n=3200)

# View blm_tweets dataset Note that there are only 3174, not 3200; that's because of deletions made on the feed
View(blm_tweets)

# Cleaning, Organizing, and Querying Downloaded Twitter Datasets -----------------------------

# Querying blm_tweets to find the 10 tweets with the most favorites

blm_tweets_most_favorited<-blm_tweets %>% slice_max(favorite_count, n=10)
View(blm_tweets_most_favorited)

# Remove unnecessary columns from "blm_tweets_most_favorited"

blm_tweets_most_favorited<- blm_tweets_most_favorited %>% 
                              select(created_at, screen_name, text, favorite_count)

View(blm_tweets_most_favorited)

# Query blm_tweets to find the 10 tweets with the most retweets and then select 
# desired columns in one block of code

blm_tweets_most_retweeted<-blm_tweets %>% 
                              slice_max(retweet_count, n=10) %>% 
                              select(created_at, screen_name, text, retweet_count)

View(blm_tweets_most_retweeted)


# Remove retweets from blm_tweets
blm_tweets_noretweets<-blm_tweets %>% filter(is_retweet=="FALSE")

# Query Data to find 5 most frequently shared links from blm_tweets

blm_tweets_links_top5<-blm_tweets %>% filter(!is.na(urls_expanded_url)) %>% 
                                      count(urls_expanded_url, sort = TRUE) %>% 
                                      rename(times_shared=n) %>% 
                                      slice_max(times_shared, n=5)

View(blm_tweets_links_top5)

# Query the data to find the 5 handles that have most frequently used #CancelStudentLoan

student_debt_tweets_frequentweeters<-student_debt_tweets %>% 
                                      count(screen_name) %>% 
                                      slice_max(n, n=5)

View(student_debt_tweets_frequentweeters)

# Query the data to find the 10 hashtags appearing most frequently in conjunction with 
# #CancelStudentDebt

CancelStudentDebt_coinciding_hashtags<-student_debt_tweets %>% 
                                          select(hashtags) %>% 
                                          unnest(hashtags) %>%
                                          mutate(hashtag_cleaned=str_to_lower(hashtags)) %>% 
                                          filter(hashtag_cleaned!="cancelstudentdebt") %>% 
                                          select(-hashtag_cleaned) %>% 
                                          count(hashtags) %>% 
                                          slice_max(n, n=10)

View(CancelStudentDebt_coinciding_hashtags)

# Visualizing and Exploring Data ------------------------------------------

# Using ggplot to make visualizations of twitter data: bar graph of coincident hashtags

CancelStudentDebt_coinciding_hashtags<-CancelStudentDebt_coinciding_hashtags %>% 
                                        mutate(hashtag=paste0("#", hashtags))

coincident_hashtags_plot<-
  ggplot(CancelStudentDebt_coinciding_hashtags, aes(x=reorder(hashtag, n), y=n))+
    geom_bar(stat="identity")+
      coord_flip()+
      xlab("")+
      ylab("Frequency")+
      ggtitle("Hashtags Most Frequently Used Along With #CancelStudentDebt")+
      labs(caption = "Data Collected from Twitter REST API via rtweet")

coincident_hashtags_plot

# Using rtweet's visualization functions: time series
ts_plot(student_debt_tweets, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a #CancelStudentDebt hashtag",
       subtitle = paste0(format(min(student_debt_tweets$created_at), "%d %B %Y"), " to ", format(max(student_debt_tweets$created_at),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

## making a map of tweets

# extract lat/longs
student_debt_tweets<-student_debt_tweets %>% lat_lng()

student_debt_tweets_latlong_extract<-student_debt_tweets %>% 
                                      filter(is.na(lat) == FALSE & is.na(lng) == FALSE)

student_debt_tweets_latlong_extract<-student_debt_tweets_latlong_extract %>% 
                                      st_as_sf(coords=c("lng", "lat")) %>% 
                                      st_set_crs("EPSG:4326")

tm_shape(student_debt_tweets_latlong_extract)+
  tm_dots()

## BLM Word Cloud 

blm_text<-str_c(blm_tweets$text, collapse="")


blm_text <- 
  blm_text %>%
  str_remove("\\n") %>%                   # remove linebreaks
  rm_twitter_url() %>%                    # Remove URLS
  rm_url() %>%
  str_remove_all("#\\S+") %>%             # Remove any hashtags
  str_remove_all("@\\S+") %>%             # Remove any @ mentions
  removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
  removeNumbers() %>%
  stripWhitespace() %>%
  removeWords(c("amp", "the")) %>% 
  removePunctuation() %>% 
  str_remove_all(pattern='[Tt]he') %>% 
  str_remove_all(pattern='[:emoji:]')

textCorpus <- 
  Corpus(VectorSource(blm_text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

View(textCorpus)

wordcloud_blm <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.2)
wordcloud_blm

# write out wordcloud_blm:

install_phantomjs()
saveWidget(wordcloud_blm, "blm.html", selfcontained = F)
webshot("blm.html", "blm.png", vwidth=1000, vheight=1000, delay=10)

# Write wc function

twitter_wordcloud<-function(twitterhandle, tweet_number){
  tweet_timeline<-get_timeline(twitterhandle, n=tweet_number)
  tweet_timeline_text<-str_c(tweet_timeline$text, collapse="")
  
  tweet_timeline_text<-tweet_timeline_text %>%
    str_remove("\\n") %>%                   # remove linebreaks
    rm_twitter_url() %>%                    # Remove URLS
    rm_url() %>%
    str_remove_all("#\\S+") %>%             # Remove any hashtags
    str_remove_all("@\\S+") %>%             # Remove any @ mentions
    removeWords(stopwords("english")) %>%   # Remove common words (a, the, it etc.)
    removeNumbers() %>%
    stripWhitespace() %>%
    removeWords(c("amp")) %>% 
    removePunctuation() %>% 
    str_remove_all(pattern='[Tt]he') %>% 
    str_remove_all(pattern='[:emoji:]')
  
  textCorpus <- 
    Corpus(VectorSource(tweet_timeline_text)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

  wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.2)
  return(wordcloud)
  
}

# test function
nyt_wordcloud<-twitter_wordcloud("nytimes", 400)
nyt_wordcloud
ft_wordcloud<-twitter_wordcloud("FinancialTimes", 400)
ft_wordcloud

# iterate WC generation 

# Iterate across handles for New York Times, Financial Times, 
# Washington Post, Fox News, CNN, and the Denver Post. 

handles<-c("nytimes", "FinancialTimes", "FoxNews", "cnn", "washingtonpost", "denverpost")
number<-c(400)

wordcloud_list<-map2(.x=handles, .y=number, twitter_wordcloud)

# Assign names to list 
names(wordcloud_list)<-handles

# examine wc's 
# nyt
wordcloud_list[["nytimes"]]

# FoxNews
wordcloud_list[["FoxNews"]]

# iterate writing out files

# write function
output_wordclouds<-function(wordclouds_to_export, wordcloud_names){
  setwd("/Users/adra7980/Documents/git_repositories/twitter_workshop/wordclouds")
  install_phantomjs()
  saveWidget(wordclouds_to_export, paste0(wordcloud_names, ".html"), selfcontained=F)
  webshot(paste0(wordcloud_names, ".html"), paste0(wordcloud_names, ".png"), vwidth=1992, vheight=1744, delay=10)
}

# iterate function across word clouds in list
map2(.x=wordcloud_list, .y=names(wordcloud_list), .f=output_wordclouds)





