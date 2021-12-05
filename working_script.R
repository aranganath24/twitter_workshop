library(rtweet)
library(httpuv)
library(tidyverse)
library(tidytext)

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
View(blm_tweets) # Note that there are only 3174, not 3200; that's because of deletions



















































blm_retweets<-blm_tweets %>% filter(is_retweet==TRUE)
View(blm_retweets)

blm_original_tweets<-blm_tweets %>% filter(is_retweet==FALSE)



data_for_blacklives<-get_timeline("@Data4BlackLives", n=3200)

@Data4BlackLives




