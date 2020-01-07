library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(purrr)
library(wordcloud)

load(file = '~/Desktop/BU MET/688/Project/twitterAuthentication.Rdata')
## Execute load file code above to retreive API information.

load(file = '~/Desktop/BU MET/688/Project/tweets.Rdata')
## Execute load file code above to retreive mined tweets.

load(file = '~/Desktop/BU MET/688/Project/Project data.Rdata')
## Execute load file code above to retreive all data for the following project

create_token(app = app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token, 
             access_secret = access_secret)

## Gainers as of 10/14/2019 --------------------------------------
# gainer1 <- search_tweets(
#   "$ACM", n = 150, include_rts = FALSE
# )
# 
# gainer1 <- gainer1[1:100,]
# 
# gainer2 <- search_tweets(
#   "$RETA", n = 150, include_rts = FALSE
# )
# gainer2 <- gainer2[1:100,]
# 
# gainer3 <- search_tweets(
#   "$SHOP", n = 150, include_rts = FALSE
# )
# gainer3 <- gainer3[1:100,]
# 
## Losers as of 10/14/2019 --------------------------------------
# 
# loser1 <- search_tweets(
#   "$PE", n = 150, include_rts = FALSE
# )
# 
# loser1 <- loser1[1:100,]
# 
# loser2 <- search_tweets(
#   "$SDC", n = 150, include_rts = FALSE
# )
# loser2 <- loser2[1:100,]
# 
# loser3 <- search_tweets(
#   "$CRWD", n = 150, include_rts = FALSE
# )
# loser3 <- loser3[1:100,]

## Create 2 tidy text objects ----------------------------------
# gainers.all <- c(gainer1$text,gainer2$text,gainer3$text)
# gainers.tib <- tibble(gainers.all)
# 
# losers.all <- c(loser1$text,loser2$text,loser3$text)
# losers.tib <- tibble(losers.all)


## Pre-processing Function -------------------------------------
cleaning.function <- function(tweets){
  # Remove http elements manually
  tweets$stripped_text <- gsub("http\\S+","",tweets)
  tweets$stripped_text <- gsub("[^\u0020-\u007F]+","",tweets$stripped_text)
  tweets$stripped_text <- gsub("'|’","",tweets$stripped_text)

  # Lowercase, remove punctuation, remove English stop_words
  data("stop_words")
  tweets.clean <- tweets %>%
    select(stripped_text) %>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words)
}

gainers.clean <- cleaning.function(gainers.tib)
losers.clean <- cleaning.function(losers.tib)

## Find most frequent terms ---------------------------------
gainers.frequency <- gainers.clean %>% 
  count(word, sort = TRUE) 
gainers.frequency %>% top_n(10)
gainers.top <- gainers.frequency %>% top_n(50)

losers.frequency <- losers.clean %>% 
  count(word, sort = TRUE)
losers.frequency %>% top_n(10)
losers.top <- losers.frequency %>% top_n(50)

## Create word cloud for each set ----------------------------
wordcloud(words = gainers.top$word, freq = gainers.top$n,
          random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))

wordcloud(words = losers.top$word, freq = losers.top$n,
          random.order=FALSE,
          colors=brewer.pal(6, "Dark2"))

## Compute sentiment score -----------------------------------
get_sentiments('bing')

sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word,stripped_text) %>% 
    anti_join(stop_words, by="word") %>%  #remove stop words
    inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score <- case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which tweets contained no words at all from the bing list
  zero.type <- case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

gainers_sent <- lapply(gainers.all,function(x){sentiment_bing(x)})
losers_sent <- lapply(losers.all,function(x){sentiment_bing(x)})

stock_sentiment <- bind_rows(
  tibble(
    stock = 'gainers',
    score = unlist(map(gainers_sent,'score')),
    type = unlist(map(gainers_sent,'type'))
  ),
  tibble(
    stock = 'losers',
    score = unlist(map(losers_sent,'score')),
    type = unlist(map(losers_sent,'type'))
  )
)

ggplot(stock_sentiment %>% filter(type != "Type 1"), aes(x=score, fill = stock)) + geom_histogram(bins = 10, alpha = .6) +
  facet_grid(~stock) + theme_bw()

stock_sentiment %>% group_by(stock) %>% 
  filter(type != "Type 1") %>%
  summarise(
    Count = n(),
    Median = median(score),
    SD = sd(score),
    max = max(score),
    min = min(score)
  )

## Create one appropriate data visualization ---------------------------

stock_info <- data.frame(
  Type = c("Gainer","Gainer","Gainer","Loser","Loser","Loser"),
  Stock = c("ACM","RETA","SHOP","PE","SDC","CRWD"),
  ChangePercent = c(6.3,4.99,4.57,-10.55,-12.85,-9.54)
)

ggplot(stock_info, aes(Stock, ChangePercent)) +
  geom_bar(stat = "identity", aes(fill = Stock), legend = FALSE) + 
  geom_text(aes(label = paste(ChangePercent, "%"),
                vjust = ifelse(ChangePercent >= 0, 0, 1))) +
  scale_y_continuous("Stock Price Change in Percent")
