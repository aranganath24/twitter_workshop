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

top_hashtags_plot<-function(twitterhashtag, tweetnumber){
  
twitter_hashtag<-search_tweets(q=twitterhashtag,
                                 n=1000,
                                 include_rts = FALSE,
                                 `-filter`="replies",
                                 lang="en")
  
hashtag_frequency_table<-
  twitter_hashtag %>% 
    select(hashtags) %>% 
    unnest(hashtags) %>%
    mutate(hashtag_cleaned=str_to_lower(hashtags)) %>% 
    filter(hashtag_cleaned!=str_to_lower(twitterhashtag)) %>% 
    select(-hashtag_cleaned) %>% 
    count(hashtags) %>% 
    slice_max(n, n=10)

return(hashtag_frequency_table)
}





























