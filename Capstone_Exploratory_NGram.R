# Loading required Packages 
library(dplyr)
library(tidyr)
library(tm)
library(ggplot2)
library(lexicon)
library(sentimentr)
library(stringr)
library(wordcloud)
library(caret)
library(tokenizers)
library(ngram)
library(NLP)

#Set Working Directory
setwd("C:/Users/r.pratap.singh/Desktop/JohnHopkins/capstone/Coursera-SwiftKey/final/en_US")

# Reading of files
# US Twitter File
us_twitter <- "en_US_twitter.txt"
con_tw <- file(us_twitter,open="r")
line_tw <- readLines(con_tw) 
long_tw <- length(line_tw)
close(con_tw)

# US News File
us_news <- "en_US_news.txt"
con_news <- file(us_news,open="r")
line_news <- readLines(con_news) 
long_news <- length(line_news)
close(con_news)

# US blog File
us_blog <- "en_US_blogs.txt"
con_blog <- file(us_blog,open="r")
line_blog <- readLines(con_blog) 
long_blog <- length(line_news)
close(con_blog)

################### SAMPLING ##########################
set.seed(5)

sample_tweet <- sample(line_tw, size = 5000)
sample_blog <- sample(line_blog, size = 5000)
sample_news <- sample(line_news, size = 5000)

sample_all <- as.character(rbind(sample_tweet, sample_blog, sample_news))

############# 1. Tokenization ############################

tokenize_fun <- function(x){
  # convert to lowercase
  x <- tolower(x)
  
  ###################### 2. Removing Prfanity #######################
  
  # getting the profane list of words from lexicon library profanity_alvarez
  profane <- unique(tolower(lexicon::profanity_alvarez))
  profane <- gsub("\\(", "c", profane)
  for (i in 1:length(profane)){
    sample_all <- gsub(profane[i],"", sample_all)
  }
  
  # remove punctuation
  x <- removePunctuation(x)
  
  # remove numbers 
  x <- removeNumbers(x)
  
  # Removing blank line
  x <- x[which(x != " ")]
  
  # Removing all Special Characters apart from alphabets
  x <- gsub("[^a-zA-Z]", " " , x )
  
  # remove numbers 
  x <- stripWhitespace(x)
  
  return(x)
}

sample_all <- tokenize_fun(sample_all)

# remove independent ing, ed, ies and er after removing profanity
sample_all <- gsub("[^a-z]ing |[^a-z]ed |[^a-z]ies |[^a-z]er |[^a-z]s |[^a-z]e ", " ", sample_all)


##################### 3. Exploratory Data Analysis ####################

# Block to remove articles and fillers for Plot
sample_all_1 <- gsub("[^a-z]is |[^a-z]am |[^a-z]are |[^a-z]an |[^a-z]the |[^a-z]so ",
                     " ", sample_all)
sample_all_1 <- gsub("[^a-z]was |[^a-z]were |[^a-z]a |[^a-z]in |[^a-z]on |[^a-z]to |[^a-z]if ",
                     " ", sample_all_1)
sample_all_1 <- gsub("[^a-z]and |[^a-z]of |[^a-z]we |[^a-z]you |[^a-z]at |[^a-z]as |[^a-z]or ",
                     " ", sample_all_1)
sample_all_1 <- gsub("[^a-z]his |[^a-z]that |[^a-z]they |[^a-z]for |[^a-z]it |[^a-z]my ",
                     " ", sample_all_1)
sample_all_1 <- gsub("[^a-z]has |[^a-z]have |[^a-z]this |[^a-z]not |[^a-z]her |[^a-z]or |[^a-z]i ",
                     " ", sample_all_1)
sample_all_1 <- gsub("[^a-z]he |[^a-z]be |[^a-z]she |[^a-z]by |[^a-z]a |[^a-z]in |[^a-z]its ",
                     " ", sample_all_1)

# Tokenizing with ngram where n= 1
sample_all_1 <- tokenize_ngrams(sample_all_1, n = 1)
# Tokenizing with ngram where n= 2
sample_all_2 <- tokenize_ngrams(sample_all, n = 2)
# Tokenizing with ngram where n= 3
sample_all_3 <- tokenize_ngrams(sample_all, n = 3)
# Tokenizing with ngram where n= 4
sample_all_4 <- tokenize_ngrams(sample_all, n = 4)
# Tokenizing with ngram where n= 1
sample_word <- tokenize_ngrams(sample_all, n = 1)

# Creating dataframe for ngram and store it in descending order frequency
s1 <- cbind.data.frame(table(unlist(sample_all_1)))  %>%
  arrange(desc(Freq)) 
s2 <- cbind.data.frame(table(unlist(sample_all_2)))  %>%
  arrange(desc(Freq))
s3 <- cbind.data.frame(table(unlist(sample_all_3)))  %>%
  arrange(desc(Freq))
s4 <- cbind.data.frame(table(unlist(sample_all_4)))   %>%
  arrange(desc(Freq))

w1 <- cbind.data.frame(table(unlist(sample_word))) %>%
  arrange(desc(Freq))

# Creating the word Clouds
wordcloud(words = s1$Var1, freq = s1$Freq, min.freq = 250, colors = brewer.pal(8,"Paired"))

wordcloud(words = s2$Var1, freq = s2$Freq, min.freq = 200, colors = brewer.pal(8,"Paired"))

wordcloud(words = s3$Var1, freq = s3$Freq, min.freq = 42, colors = brewer.pal(8,"Paired"))

wordcloud(words = s4$Var1, freq = s4$Freq, min.freq = 8, colors = brewer.pal(8,"Paired"))


# BarPlot for 2 Ngram model
ggplot(data=s2[1:20,], aes(x=reorder(Var1,-Freq), y = Freq) )+
  geom_bar(stat = "identity", fill ="sky blue", color="blue") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("2Gram words ") +
  ylab("Frequency of Words")

# BarPlot for 3 Ngram model
ggplot(data=s3[1:20,], aes(x=reorder(Var1,-Freq), y = Freq) )+
  geom_bar(stat = "identity", fill ="sky blue", color="blue") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("3Gram words ") +
  ylab("Frequency of Words")

# BarPlot for 4 Ngram model
ggplot(data=s4[1:20,], aes(x=reorder(Var1,-Freq), y = Freq) )+
  geom_bar(stat = "identity", fill ="sky blue", color="blue") +
  theme(axis.text.x=element_text(angle=90)) +
  xlab("4Gram words ") +
  ylab("Frequency of Words")
# How many unique words do you need in a frequency sorted dictionary to cover 50% 
# of all word instances in the language? 90%?

coverage <- function(object, percent) {
  cover <- 0
  sumCover <- sum(object)
  for(i in 1:length(object)) {
    cover <- cover + object[i]
    if(cover >=  percent*(sumCover)){break}
  }
  return(i)
}

coverage(w1$Freq, .5)
# 134 words to cover 50% instances

coverage(w1$Freq, .9)
# 7137 words to cover 90% instances

# Creating Dataframe with Frequency for Ngram 2, 3, 4 and 1


s4 <- separate(s4, Var1, into = c("words", "out"), sep = " (?=[^ ]+$)")
s3 <- separate(s3, Var1, into = c("words", "out"), sep = " (?=[^ ]+$)")
s2 <- separate(s2, Var1, into = c("words", "out"), sep = " (?=[^ ]+$)")

#Set Working Directory
setwd("C:/Users/r.pratap.singh/Desktop/JohnHopkins/capstone/Final_capstone/Capstone_final/")

saveRDS(w1, file = "Ngram1.rds")
saveRDS(s2, file = "Ngram2.rds")
saveRDS(s3, file = "Ngram3.rds")
saveRDS(s4, file = "Ngram4.rds")


