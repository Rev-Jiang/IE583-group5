data<-read.csv("/Users/Anesthesia/Downloads/metadata.csv")

title.text <- as.character(data$title)
title.df<-data.frame(title=title.text,journal=data$journal,id=data$doi)

tidy_title <- title.df %>% ## pipe operator in dplyr, can be used to chain code together.
  unnest_tokens(output = word, input = title,
                token = "ngrams",n=1) 
#### Checking mis-spelling
words <- tidy_title$word
checks <- hunspell_check(words)
bads <- which(!checks)
for(i in bads){ # time-consuming
  words[i] = unlist(hunspell_suggest(words[i]))[1]
}
tidy_title$word = words
## Stop word removal
#stop_words ## from tidytext; tibble: a reimagining of the data.frame, https://tibble.tidyverse.org/
tidy_title <- tidy_title %>%
  anti_join(stop_words, by="word")
#head(tidy_imdb,10)
# create new bag of stop words
#my_stopw = tibble(word = c("movie","imdb"),lexicon = "imdbreview")
#tidy_title <- tidy_title %>%
#  anti_join(my_stopw, by="word")

## Stemming and lemmatization
library(textstem)
words <- tidy_title$word
stems <- hunspell_stem(words)
for(i in 1:length(words)){
  l <- length(unlist(stems[[i]]))
  if(l>0) words[i] = stems[[i]][l]
  else words[i] = words[i]
}
words = lemmatize_words(words)
tidy_title$word = words

## Creating weighted TDM
# tf_idf weighting 
words_n <- tidy_title %>%
  count(c("id","word"))
tf_idfs <- words_n %>%
  bind_tf_idf(word, id, freq)
tf_idfs = na.omit(tf_idfs)
# weighted term-document matrix
tf_idfs$id<-as.character(tf_idfs$id)
tdm <- tf_idfs %>%
  cast_tdm(id, word, tf_idf)
tdm <- as.matrix(tdm)
dim(tdm)
tdm[1:8,1:8]
View(tdm)



