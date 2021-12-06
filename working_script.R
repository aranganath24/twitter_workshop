library(rtweet)
library(httpuv)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(qdapRegex)
library(tm)
library(webshot)
library(htmlwidgets)


# Pulling Data from API ---------------------------------------------------


# Pull tweets with #CancelStudentDebt; returns 1000 most recent tweets; time by GMT
student_debt_tweets<-search_tweets(q="#CancelStudentDebt", 
                                    n=1000,
                                    include_rts=FALSE,
                                    `-filter`="replies",
                                     lang="en")

View(student_debt_tweets)

# Pull tweets with #CancelStudentDebt AND capitalism
student_debt_capitalism_tweets<-search_tweets(q="#CancelStudentDebt capitalism", 
                                   n=1000,
                                   include_rts=FALSE,
                                   `-filter`="replies",
                                   lang="en")

# Insteading of pulling from the API, you could also pull tweets with #CancelStudentDebt, and then query the text
# of these tweets locally using a stringr function

student_debt_capitalism_tweets_ALT<-student_debt_tweets %>% 
                                    filter(str_detect(text, "[Cc]apitalism"))

# Pull tweets with #CancelStudentDebt OR capitalism

student_debt_OR_capitalism_tweets<-search_tweets(q="#CancelStudentDebt OR capitalism", 
                                               n=1000,
                                               include_rts=FALSE,
                                               `-filter`="replies",
                                               lang="en")

View(student_debt_OR_capitalism_tweets)

# Pull tweets from an account (doesn't have same time constraints)
# Pull last 3200 BLM tweets
blm_tweets<-get_timeline("@Blklivesmatter", n=3200)
View(blm_tweets) # Note that there are only 3174, not 3200; that's because of deletions made on the feed


# Cleaning and Organizing Downloaded Datasets -----------------------------









# Visualizing and Exploring Data ------------------------------------------

# BLM word cloud

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
  removeWords(c("amp"))                   # Final cleanup of other small changes


textCorpus <- 
  Corpus(VectorSource(blm_text)) %>%
  TermDocumentMatrix() %>%
  as.matrix()

textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)

wordcloud <- wordcloud2(data = textCorpus, minRotation = 0, maxRotation = 0, ellipticity = 0.2)

wordcloud


# Wrap word cloud code into a function that takes twitter account, n, and generates word cloud

twitter_wordcloud<-function(twitterhandle, tweet_number){
  handle_as_string<-deparse(substitute(twitterhandle))
  tweet_timeline<-get_timeline(handle_as_string, n=tweet_number)
  return(tweet_timeline)
}

  
lebron_james<-twitter_wordcloud(KingJames, 100)

twitter_wordcloud<-function(twitterhandle, tweet_number){
  tweet_timeline<-get_timeline(twitterhandle, n=tweet_number)
  tweet_timeline_text<-str_c(tweet_timeline$text, collapse="")
  tweet_timeline_text<-str_remove_all(tweet_timeline_text, pattern='[:emoji:]')
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
                              removePunctuation()

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
lebron_wordcloud<-twitter_wordcloud("KingJames", 400)
krugman_wordcloud<-twitter_wordcloud("paulkrugman", 400)
elon_wordcloud<-twitter_wordcloud("elonmusk", 400)

# iterate WC generation 

handles<-c("kingJames", "paulKrugman", "elonmusk")
number<-c(400)

wordcloud_list<-map2(.x=handles, .y=number, twitter_wordcloud)

# Assign names to elements in "wordcloud_list"
names(wordcloud_list)<-handles

# access list elements with name or index
wordcloud_list[["paulKrugman"]]
wordcloud_list[[2]]


# writing files

install_phantomjs()
saveWidget(elon_wordcloud, "elon.html", selfcontained = F)
webshot("elon.html", "elon.png", vwidth=1992, vheight=1744, delay=10)


# iterate writing out files

output_wordclouds<-function(wordclouds_to_export, wordcloud_names){
  setwd("~/Documents/git_repositories/twitter_workshop")
  install_phantomjs()
  saveWidget(wordclouds_to_export, paste0(wordcloud_names, ".html"), selfcontained=F)
  webshot(paste0(wordcloud_names, ".html"), paste0(wordcloud_names, ".png"), vwidth=1992, vheight=1744, delay=10)
}

# method1
list(wordclouds_to_export=wordcloud_list, wordcloud_names=names(wordcloud_list)) %>% 
        pmap(output_wordclouds)

#method2
map2(.x=wordcloud_list, .y=names(wordcloud_list), .f=output_wordclouds)




# Appendix ----------------------------------------------------------------

# iterate word cloud generation using pmap instead of map2

handles<-c("kingJames", "paulKrugman", "elonmusk")
number=c(400)
wordcloud_list_alt<-pmap(list(handles, number), twitter_wordcloud)
wordcloud_list_alt<-pmap(list(twitterhandle=handles, tweet_number=number), twitter_wordcloud)

list(twitterhandle=handles, tweet_number=number) %>% pmap(twitter_wordcloud)

# iteratively write out using map2 instead of pmap
map2(.x=wordcloud_list, .y=names(wordcloud_list), .f=output_wordclouds)







maya_wordcloud<-twitter_wordcloud("maya_ranganath", 400)
cu_libraries<-twitter_wordcloud("cublibraries", 400)




https://martinctc.github.io/blog/vignette-write-and-read-multiple-excel-files-with-purrr/






blm_retweets<-blm_tweets %>% filter(is_retweet==TRUE)
View(blm_retweets)

blm_original_tweets<-blm_tweets %>% filter(is_retweet==FALSE)



data_for_blacklives<-get_timeline("@Data4BlackLives", n=3200)

@Data4BlackLives




