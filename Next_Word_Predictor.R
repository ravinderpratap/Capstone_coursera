# Foreign Language Evaluation : right now it is not discriminating any foreign language based 
# on dictonary. We can make use of “tm_map” function to “removeWords” based on a language. The
# difference in word count will provide the words from that language in the corpora.

# Can you think of a way to increase the coverage? : identifying words that may not be in the 
# corpora or using a smaller number of words in the dictionary to cover the same number 
# of phrases?
# Learning the writing style and removing the low-frequency words for better prediction.
# Clustering of the words (similar meaning words)


#################
# 1. Build basic n-gram model - using the exploratory analysis you performed, build a 
# 2. basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.


# In order to build the N-gram, I'll process the N-grams by breaking them in several different 
# indexes. Each of the N-grams will be saved as intermediary files, as follows:  
#* a file for the "n-gram" to "index" mapping (since the words take most of the memory space, 
# I'll keep them in only one place)  
#* a file for the "n-1 gram", the prior. This has the indexes in the N-1 "n-gram to index" 
# mapping (calculated in a prev step)  
#* a file for the "1-gram" posterior. This has the indexes in the 1-gram "n-gram to index" 
# mapping (calculated in first step)  
#* a file with the calculated probabilities, mapping N-gram to calculated probabilities (sorted
# decreasingly by probability).  


# So, the code underneath does:  
# * load the "term frequency" file, sorted decreasingly in a previous step by frequency  
# * extract the "N-gram to index" vector, save  
# * extract the priors (N-1-Grams), posteriors (1-Grams), save
# * calculate the conditional probabilities for the N-Grams | N-1-Grams, using MLE   
# * Extract Top 20 Predicted words

library(dplyr)
library(tidyr)
library(tm)
library(ggplot2)
library(caret)
library(tokenizers)
library(ngram)

## Word Prediction Model Function with backoff functioning

word_predict_model <- function(sentence, w1, s2, s3, s4) {
  sentence <- trimws(sentence)
  sentence <- removeNumbers(removePunctuation(sentence))
  sentence <- tolower(stripWhitespace(sentence))
  sentence <- unlist(strsplit(sentence, " "))
  length1 <- length(sentence)
  
  back_off <- 0
  
  if (length1 >= 3) {
    sentence <- sentence[(length1-2) :length1]
    sentence <- paste(sentence, collapse = " ")
    
    test <- s4 %>%
      filter(words == sentence) %>%
      arrange(desc(Freq)) %>%
      top_n(20,Freq) %>%
      select(out)
    
    if (nrow(test) == 0){
      back_off <- 1
      sentence <- unlist(strsplit(sentence, " "))
    }
    
  } 
  
  if(length1 == 2 | back_off == 1) {
    length1 <- length(sentence)
    sentence <- sentence[(length1-1) :length1]
    sentence <- paste(sentence, collapse = " ")
    test <- s3 %>%
      filter(words == sentence) %>%
      arrange(desc(Freq)) %>%
      top_n(20,Freq) %>%
      select(out)
    
    if (nrow(test) == 0){
      back_off <- 1
      sentence <- unlist(strsplit(sentence, " "))
    }
    
  }
  
  if(length1 == 1 | back_off == 1) {
    length1 <- length(sentence)
    sentence <- sentence[length1 :length1]
    sentence <- paste(sentence, collapse = " ")
    test <- s2 %>%
      filter(words == sentence) %>%
      arrange(desc(Freq)) %>%
      top_n(20,Freq) %>%
      select(out)
    
    if (nrow(test) == 0){
      back_off <- 1
      sentence <- unlist(strsplit(sentence, " "))
    }
  } 
  
  if (length1 == 0) {
    test <- w1 %>%
      arrange(desc(Freq)) %>%
      top_n(20,Freq)  %>%
      select(Var1)
  }
  
  #
  return(test)
}



