# INSTALL PACKAGES ##########################

install.packages("shiny")
install.packages("wordcloud")
install.packages("devtools")
install.packages("tidyverse")      
install.packages("stringr")        
install.packages("tidytext")
install.packages("dplyr")
install.packages("reshape2")
install.packages("igraph")
install.packages("ggraph")
install.packages("pacman")
install.packages("rmarkdown")

install.packages("memoise")

# MAKE PACKAGES AVAILABLE ######################## 

library(shiny)
library(wordcloud)
library(devtools)
library(tidyverse)      
library(stringr)        
library(tidytext)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
library(memoise)
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
pacman::p_load(pacman, tm, SnowballC, dplyr)

# IMPORT DATA ######################### 
# "Angels & Demons" by Dan Brown, published 2000
bookAAD <- readLines('ANGELS AND DEMONS.txt')

# "The Da Vinci Code" by Dan Brown, published 2003
bookDVC <- readLines('The Da Vinci Code.txt')

# "The Lost Symbol" by Dan Brown, pubished 2009
bookTLS <- readLines('The Lost Symbol.txt')

# SINGLE BOOK DATA #################
# CORPUS FOR ANGELS & DEMONS ############################

# Preliminary corpus
corpusAAD <- Corpus(VectorSource(bookAAD)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)

# Create term-document matrices & remove sparse terms
tdmAAD <- DocumentTermMatrix(corpusAAD) %>%
  removeSparseTerms(1 - (5/length(corpusAAD)))

# Calculate and sort by word frequencies
word.freqAAD <- sort(colSums(as.matrix(tdmAAD)),
                     decreasing = T)

# Create frequency table
tableAAD <- data.frame(word = names(word.freqAAD),
                       absolute.frequency = word.freqAAD,
                       relative.frequency =
                         word.freqAAD/length(word.freqAAD))

# Remove the words from the row names
rownames(tableAAD) <- NULL

# Show the 10 most common words
head(tableAAD, 10)

# Export the 1000 most common words in CSV files
write.csv(tableAAD[1:1000, ], "AAD_1000.csv")

# CORPUS FOR THE DA VINCI CODE ############################

corpusDVC <- Corpus(VectorSource(bookDVC)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)
tdmDVC <- DocumentTermMatrix(corpusDVC) %>%
  removeSparseTerms(1 - (5/length(corpusDVC)))
word.freqDVC <- sort(colSums(as.matrix(tdmDVC)),
                     decreasing = T)
tableDVC <- data.frame(word = names(word.freqDVC),
                       absolute.frequency = word.freqDVC,
                       relative.frequency =
                         word.freqDVC/length(word.freqDVC))
rownames(tableDVC) <- NULL
head(tableDVC, 10)
write.csv(tableDVC[1:1000, ], "DVC_1000.csv")

# CORPUS FOR THE LOST SYMBOL ############################

corpusTLS <- Corpus(VectorSource(bookTLS)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)
tdmTLS <- DocumentTermMatrix(corpusTLS) %>%
  removeSparseTerms(1 - (5/length(corpusTLS)))
word.freqTLS <- sort(colSums(as.matrix(tdmTLS)),
                     decreasing = T)
tableTLS <- data.frame(word = names(word.freqTLS),
                       absolute.frequency = word.freqTLS,
                       relative.frequency =
                         word.freqTLS/length(word.freqTLS))
rownames(tableTLS) <- NULL
head(tableTLS, 10)
write.csv(tableTLS[1:1000, ], "TLS_1000.csv")

# TWO BOOKS DATA ###################
# MOST DISTINCTIVE WORDS ################################

# Set number of digits for output
options(digits = 2)

# Compare relative frequencies (via subtraction) 
# ("Angels & Demons" vs "The Da Vinci Code")
AADvsDVC <- tableAAD %>%
  merge(tableDVC, by = "word") %>%
  mutate(dProp = 
           relative.frequency.x - 
           relative.frequency.y,
         dAbs = abs(dProp)) %>%
  arrange(desc(dAbs)) %>%
  rename(AAD.freq = absolute.frequency.x,
         AAD.prop = relative.frequency.x,
         DVC.freq = absolute.frequency.y,
         DVC.freq = relative.frequency.y)

# Show the 10 most distinctive terms
head(AADvsDVC, 10)

# Save full table to CSV
write.csv(AADvsDVC, "AAD vs DVC.csv")

# ("Angels & Demons" vs "The Lost Symbol")
AADvsTLS <- tableAAD %>%
  merge(tableTLS, by = "word") %>%
  mutate(dProp = 
           relative.frequency.x - 
           relative.frequency.y,
         dAbs = abs(dProp)) %>%
  arrange(desc(dAbs)) %>%
  rename(AAD.freq = absolute.frequency.x,
         AAD.prop = relative.frequency.x,
         TLS.freq = absolute.frequency.y,
         TLS.freq = relative.frequency.y)
head(AADvsTLS, 10)
write.csv(AADvsTLS, "AAD vs TLS.csv")

# ("The Da Vinci Code" vs "The Lost Symbol")
DVCvsTLS <- tableDVC %>%
  merge(tableTLS, by = "word") %>%
  mutate(dProp = 
           relative.frequency.x - 
           relative.frequency.y,
         dAbs = abs(dProp)) %>%
  arrange(desc(dAbs)) %>%
  rename(DVC.freq = absolute.frequency.x,
         DVC.prop = relative.frequency.x,
         TLS.freq = absolute.frequency.y,
         TLS.freq = relative.frequency.y)
head(DVCvsTLS, 10)
write.csv(DVCvsTLS, "DVC vs TLS.csv")

  
# Three BOOKS DATA ###################  
titles <- c("Angels & Demons", "The Da Vinci Code", "The Lost Symbol")

books <- list(bookAAD, bookDVC, bookTLS) 

##Each book is an array in which each value in the array is a chapter 
series <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    ##Here we tokenize each chapter into words
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}
# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series

# We can get simple counts for each word using the count function.
series %>% count(word, sort = TRUE)

# Remove stopwords and make a word cloud
series$book <- factor(series$book, levels = rev(titles))
series %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Sentiment derived from the NRC ###########
(hp_nrc <- series %>% 
   inner_join(get_sentiments("nrc")) %>%
   group_by(book, chapter, sentiment))

# Visualize the positive/negative sentiment for each book
# over time using the AFINN dictionary
series %>%    
  inner_join(get_sentiments("afinn")) %>%
  group_by(book, chapter) %>%
  summarize(score = sum(score)) %>%
  ggplot(aes(chapter, score, fill = book)) +
  geom_col() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "Emotional Arc of the Three Books",
       subtitle = "AFINN sentiment dictionary",
       x = "Word",
       y = "Emotional score") +
  theme(legend.position = "none")

# Cumulative score 
series %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(book) %>%
  mutate(cumscore = cumsum(score)) %>%
  ggplot(aes(chapter, cumscore, fill = book)) +
  geom_step() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "Emotional Arc of the Three Books",
       subtitle = "AFINN sentiment dictionary",
       x = "Word",
       y = "Cumulative emotional score")

# Visualize the sentimental content of each chapter 
# in each book using the NRC dictionary
hp_nrc %>%
  count(sentiment, book, chapter) %>%
  filter(!(sentiment %in% c("positive", "negative"))) %>%
  # create area plot
  ggplot(aes(x = chapter, y = n)) +
  geom_col(aes(fill = sentiment)) + 
  # add black smoothing line without standard error
  geom_smooth(aes(fill = sentiment), method = "loess", se = F, col = 'black') + 
  theme(legend.position = 'none') +
  labs(x = "Word", y = "Emotion score", # add labels
       title = "Emotions during the books",
       subtitle = "Using tidytext and the nrc sentiment dictionary") +
  # seperate plots per sentiment and book and free up x-axes
  facet_grid(sentiment ~ book, scale = "free")
### This chunk takes longer to execute


# Another sentiment analysis ##############
series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# Use the 'bing' lexicon for sentiment analysis
series %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# Use the the 'bing' lexicon for sentiment analysis
# and make a comparison cloud
series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 50)

# Use the the 'bing' lexicon for sentiment analysis
# and make a comparison cloud with stopwords removed
series %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 50)

# Calculate "Sentiment Score"
series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

# Pairs of words (Bigrams)
series <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    ##Here we tokenize each chapter into bigrams
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}
# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series

# Use the count function to find the most common bigrams in the books.
series %>%
  count(bigram, sort = TRUE)

# bigrams without stopwords
bigrams_separated <- series %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united %>% 
  count(bigram, sort = TRUE)

# Use bigrams to practice tf-idf
# (term frequency -inverse document frequency)
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

# Make the chart
plot <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))
plot  %>% 
  top_n(20) %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

# Find the bigrams that have the word “not” as the first word
# in the bigram to show how sentiment analysis was affected by negations,
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Remove stopwords
bigrams_separated <- bigrams_separated %>%
  filter(word1 == "not") %>%
  filter(!word2 %in% stop_words$word)%>%
  count(word1, word2, sort = TRUE)
bigrams_separated

# Use Bing lexicon
BING <- get_sentiments("bing")
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  filter(!word2 %in% stop_words$word)%>%
  inner_join(BING, by = c(word2 = "word")) %>%
  ungroup()
not_words

# CLEAN UP ###########################################

# Clear workspace 
rm(list = ls())


