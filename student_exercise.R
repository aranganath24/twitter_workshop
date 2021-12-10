
# Make function to take inputs of twitter hashtag and number of queries, and create a bar chart
# of the hashtags occuring most frequently alongside the input hashtag; then, iterate this function 
# across several hashtags and put the resulting visualizations in a list

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
    slice_max(n, n=10, with_ties = FALSE)

hashtag_frequency_table<-hashtag_frequency_table %>% mutate(hashtag=paste0("#", hashtags))

related_hashtags_plot<-
  ggplot(hashtag_frequency_table, aes(x=reorder(hashtag, n), y=n))+
  geom_bar(stat="identity")+
  coord_flip()+
  xlab("")+
  ylab("Frequency")+
  ggtitle(paste0("Hashtags Most Frequently Used Along With", " ", "#", twitterhashtag))+
  labs(caption = "Data Collected from Twitter REST API via rtweet")

return(related_hashtags_plot)

}

# Test function
top_hashtags_plot("SupremeCourt", 1000)
top_hashtags_plot("ZeroCovid", 1000)

# Iterate and put visualizations into list

desired_hashtags<-c("ZeroCovid", "teslaq", "airbnb", "trump")
number<-c(1000)

hashtag_viz_list<-map2(.x=desired_hashtags, .y=number, .f=top_hashtags_plot)

hashtag_viz_list[[2]]
hashtag_viz_list[[4]]
hashtag_viz_list[[1]]
hashtag_viz_list[[3]]

# Assig names
names(hashtag_viz_list)<-desired_hashtags

hashtag_viz_list









