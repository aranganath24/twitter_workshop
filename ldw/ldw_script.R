

# Load Libraries and log into twitter-------------------------------------------------------------------------

library(rtweet)
library(httpuv)
library(tidyverse)
library(tidytext)


# Make sure you're logged onto twitter in a web browser

# Extracting Data from the Twitter API via *rtweet* Package---------------------------------------------------

# Pull tweets with #ValentinesDay; returns 1000 most recent tweets; time by GMT
valentine_tweets<-search_tweets(q="#ValentinesDay", 
                                n=1000,
                                include_rts=FALSE,
                                `-filter`="replies",
                                lang="en")

View(valentine_tweets)


# Pull tweets with #ValentinesDay AND #SinglesAwareness
valentinesday_AND_singlesawareness<-search_tweets(q="#ValentinesDay #SinglesAwareness", 
                                    n=1000,
                                    include_rts=FALSE,
                                    `-filter`="replies",
                                    lang="en")

View(valentinesday_AND_singlesawareness)


# Pull tweets with #ValentinesDay OR singlesday
valentinesday_OR_singlesawareness<-search_tweets(q="#ValentinesDay OR singlesday", 
                                                  n=1000,
                                                  include_rts=FALSE,
                                                  `-filter`="replies",
                                                  lang="en")

View(valentinesday_OR_singlesawareness)


#Pull tweets from an account (doesn't have same time constraints)
# Pull last 500 tweets from @VDay, a global activist movement to end violence against women that is associated with Valentine's day (note sometimes the query will return less than specified number due to deletions)
vday_tweets<-get_timeline("@VDay", n=500)

View(vday_tweets)


# Querying twitter datasets -----------------------------------------------

# Extracts 10 most favorited tweets from "vday_tweets"
vday_tweets_most_favorites<-vday_tweets %>% 
                            slice_max(favorite_count, n=10)


View(vday_tweets_most_favorites)


# Remove unnecessary columns from "vday_tweets_most_favorites"

vday_tweets_most_favorites<-vday_tweets_most_favorites %>% 
                              select(created_at, screen_name, text, favorite_count)

View(vday_tweets_most_favorites)


# Extracts 10 most retweeted observations from "vday_tweets"
vday_tweets_most_retweeted<-vday_tweets %>% 
                              slice_max(retweet_count, n=10) %>% 
                              select(created_at, screen_name, text, retweet_count)


# prints "vday_tweets_most_retweeted"
vday_tweets_most_retweeted


# extracts table with 5 most frequently shared links from the @Vday handle
vday_links_top5<-vday_tweets %>% filter(!is.na(urls_expanded_url)) %>% 
                                count(urls_expanded_url, sort = TRUE) %>% 
                                rename(times_shared=n) %>% 
                                slice_max(times_shared, n=5) %>% 
                                unnest(cols=urls_expanded_url)


View(vday_links_top5)


# Query "valentine_tweets" to find the 5 handles that have most frequently 
#used #ValentinesDay

valentines_frequent_tweeters<-valentine_tweets %>% 
                              count(screen_name) %>% 
                              slice_max(n, n=5)


View(valentines_frequent_tweeters)


# Query the data to find the 10 hashtags appearing most frequently in conjunction with 
# #ValentinesDay

ValentinesDay_coinciding_hashtags<-valentine_tweets %>% 
                                    select(hashtags) %>% 
                                    unnest(hashtags) %>%
                                    mutate(hashtag_cleaned=str_to_lower(hashtags)) %>% 
                                    filter(hashtag_cleaned!="valentinesday") %>% 
                                    select(-hashtag_cleaned) %>% 
                                    count(hashtags) %>% 
                                    slice_max(n, n=10)

View(ValentinesDay_coinciding_hashtags)



# visualization -----------------------------------------------------------

# creates new column that adds #
ValentinesDay_coinciding_hashtags<-ValentinesDay_coinciding_hashtags %>% 
                                          mutate(hashtag=paste0("#", hashtags))



# Makes inverted bar chart of "CancelStudentDebt_coinciding_hashtags"
coincident_hashtags_plot<-
  ggplot(CancelStudentDebt_coinciding_hashtags, aes(x=reorder(hashtag, n), y=n))+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("")+
  ylab("Frequency")+
  ggtitle("Hashtags Most Frequently Used Along With #ValentinesDay")+
  labs(caption = "Data Collected from Twitter REST API via rtweet")


coincident_hashtags_plot

# Using rtweet's visualization functions: time series
ts_plot(valentine_tweets, by="hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with #ValentinesDay",
       subtitle = paste0(format(min(valentine_tweets$created_at), "%d %B %Y"), 
                         " to ", 
                         format(max(valentine_tweets$created_at),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()











