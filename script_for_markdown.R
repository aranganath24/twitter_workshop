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

# write data 
write_as_csv(student_debt_tweets, "/Users/adra7980/Documents/git_repositories/twitter_workshop/exported_data/student_debt_tweets.csv")

# Pull tweets with #CancelStudentDebt AND capitalism
student_debt_capitalism_tweets<-search_tweets(q="#CancelStudentDebt capitalism", 
                                              n=1000,
                                              include_rts=FALSE,
                                              `-filter`="replies",
                                              lang="en")

write_as_csv(student_debt_capitalism_tweets, "student_debt_capitalism_tweets.csv")


# Insteading of pulling from the API, you could also pull tweets with #CancelStudentDebt, and then query the text
# of these tweets locally using a stringr function

student_debt_capitalism_tweets_ALT<-student_debt_tweets %>% 
  filter(str_detect(text, "[Cc]apitalism"))

write_as_csv(student_debt_capitalism_tweets_ALT, "student_debt_capitalism_tweets_ALT")

# Pull tweets with #CancelStudentDebt OR capitalism

student_debt_OR_capitalism_tweets<-search_tweets(q="#CancelStudentDebt OR capitalism", 
                                                 n=1000,
                                                 include_rts=FALSE,
                                                 `-filter`="replies",
                                                 lang="en")

View(student_debt_OR_capitalism_tweets)

write_as_csv(student_debt_OR_capitalism_tweets, "student_debt_OR_capitalism_tweets")


# Pull tweets from an account (doesn't have same time constraints)
# Pull last 3200 BLM tweets
blm_tweets<-get_timeline("@Blklivesmatter", n=3200)
View(blm_tweets) # Note that there are only 3174, not 3200; that's because of deletions made on the feed
write_as_csv(blm_tweets, "blm_tweets")


# Cleaning and Organizing Downloaded Datasets -----------------------------

# Visualizing and Exploring Data ------------------------------------------

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








