

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

