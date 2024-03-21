####[1] How do customers view our product against competition? 
library(ggplot2) # for plotting
library(tidytext) # for analyzing text in tidy manner
library(dplyr) # data munging
library(tidyverse)
library(wordcloud)
library(tidyr)
library(tm)
library(NLP)

getwd()
setwd("C:/Users/pooja/Desktop/Residency 2/Text analytics/assignment")


reviews_df <- read.csv('car_data.csv',stringsAsFactors = FALSE)

head(reviews_df, 5)

nrow(reviews_df)

reviews_df$doc_id <- paste0('doc ',seq(1,nrow(reviews_df))) # we need this later on

head(reviews_df,2)

summary(reviews_df)

reviews_df$doc_id

text_df <- reviews_df %>% select(c('doc_id','Brand.Name','Review.Text')) #lets focus on reviews only

compass_df <- text_df %>% filter(Brand.Name=="Jeep Compass")
seltos_df<- text_df %>% filter(Brand.Name=="Kia Seltos")
hector_df <- text_df %>% filter(Brand.Name=="MG Hector")

summary(compass_df)
summary(seltos_df)

summary(hector_df)

#*******************************************Word Cloud*********************************************
compass_reviews <-compass_df %>% unnest_tokens(word,Review.Text)
seltos_reviews <- seltos_df %>% unnest_tokens(word,Review.Text)
hector_reviews <- hector_df %>% unnest_tokens(word,Review.Text)


custom_stop_words <- bind_rows(stop_words, 
                               tibble(word = c("mg", "hector", "jeep", "compass", "kia", "seltos","car","love","suv","2018","vehicle","ia","1st","read","kia","seltos","cars","ve","ia","owned","XXXXX","NA","12","past","thrusday","nissan","sentra","200k)"), 
                                      lexicon = rep("custom", 28)))                                     

compass_reviews %>%
  count(word, sort = TRUE)%>%
  filter(n >5)%>%   # n is wordcount colname.
  anti_join(custom_stop_words)%>%
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "red", fill = "red") +
  xlab(NULL) +ggtitle("Jeep compass Reviews")+
  coord_flip()         


hector_reviews %>%
  count(word, sort = TRUE)%>%
  filter(n >25)%>%   # n is wordcount colname.
  anti_join(custom_stop_words)%>%
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "red", fill = "red") +
  xlab(NULL) +ggtitle("MG Hector Reviews")+
  coord_flip()




seltos_reviews %>%
  count(word, sort = TRUE)%>%
  filter(n > 25)%>%   # n is wordcount colname.
  anti_join(custom_stop_words)%>%
  mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity", col = "red", fill = "red") +ggtitle("Kia Seltos Reviews")+
  xlab(NULL) +
  coord_flip()


# define a nice color palette
pal <- brewer.pal(8,"Dark2")


custom_stop_words <- bind_rows(stop_words, 
                               tibble(word = c("30","4","carries","class","2","months","a4","mercedes","1.4","10","10 10","300","gt","line","ht" ,"line","3","10 10","mg", "hector", "jeep", "compass", "kia", "seltos","car","love","suv","2018","vehicle","ia","1st","read","kia","seltos","cars","top","also","the","this","kia","seltos","hector","mg","car","and","the","very","has","suv","rear","with","have","2000","rpm","havee","eve","NA","owned","quote","factors"), 
                                      
                                      lexicon = rep("custom", 60)))
#####___________________________________________word cloud of 3 ______________________________************************8
compass_reviews %>% 
  anti_join(custom_stop_words)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) %>%
  filter(n >1)%>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 200, colors=pal,))


seltos_reviews %>% 
  anti_join(custom_stop_words)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) %>%
  filter(n >1)%>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 200, colors=pal))

hector_reviews %>% 
  anti_join(custom_stop_words)%>%
  anti_join(stop_words)%>%
  count(word, sort = TRUE) %>%
  filter(n >1)%>%
  with(wordcloud(word, n, random.order = FALSE, max.words = 200, colors=pal))




#******************************************Bi Grams*******************************************
########################compass bigram########################################################################################
summary(compass_df)

compass_bigrams <- compass_df %>%
  unnest_tokens(bigram, Review.Text, token = "ngrams", n = 2)
compass_bigrams %>%
  count(bigram, sort = TRUE)
library(tidyr)
bigram_separated <- compass_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
custom_stop_words
summary(custom_stop_words)
bigrams_filtered <- compass_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
head(bigram_counts)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_filtered %>%
  filter(word2 == "quality") %>%
  count(Brand.Name, word1, sort = TRUE)

bigrams_filtered %>%
  filter(word2 == "price") %>%
  count(Brand.Name, word1, sort = TRUE)
bigram_tf_idf <- bigrams_united %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))
bigrams_united%>%
  count(bigram)%>%
  with(wordcloud(bigram_tf_idf$bigram,bigram_tf_idf$n,max.words=30,min.freq = 5,random.order = F, colors=pal))

#_______________________bigram for kia seltos____________________________________
#####################################################################################seltos_df#######################################

seltos_bigrams <- seltos_df %>%
  unnest_tokens(bigram, Review.Text, token = "ngrams", n = 2)

seltos_bigrams %>%
  count(bigram, sort = TRUE)
library(tidyr)
bigram_separated <- seltos_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
custom_stop_words
summary(custom_stop_words)
bigrams_filtered <- bigram_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
head(bigram_counts)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_filtered %>%
  filter(word2 == "price") %>%
  count(Brand.Name, word1, sort = TRUE)
bigrams_filtered %>%
  filter(word2 == "features") %>%
  count(Brand.Name, word1, sort = TRUE)
bigram_tf_idf <- bigrams_united %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))
bigrams_united%>%
  count(bigram)%>%
  with(wordcloud(bigram_tf_idf$bigram,bigram_tf_idf$n,max.words=30,min.freq = 5,random.order = F, colors=pal))

#*******************************************************************************************************************************************************
#####################################################hector_df###########################################################################################################

hector_bigrams <- hector_df %>%
  unnest_tokens(bigram, Review.Text, token = "ngrams", n = 2)
hector_bigrams %>%
  count(bigram, sort = TRUE)
library(tidyr)
bigram_separated <- hector_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
custom_stop_words
summary(custom_stop_words)
bigrams_filtered <- bigram_separated %>%
  filter(!word1 %in% custom_stop_words$word) %>%
  filter(!word2 %in% custom_stop_words$word)
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
head(bigram_counts)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_filtered %>%
  filter(word2 == "price") %>%
  count(Brand.Name, word1, sort = TRUE)
bigrams_filtered %>%
  filter(word2 == "features") %>%
  count(Brand.Name, word1, sort = TRUE)
bigram_tf_idf <- bigrams_united %>%
  count(doc_id, bigram) %>%
  bind_tf_idf(bigram, doc_id, n) %>%
  arrange(desc(tf_idf))
bigrams_united%>%
  count(bigram)%>%
  with(wordcloud(bigram_tf_idf$bigram,bigram_tf_idf$n,max.words=30,min.freq = 5,random.order = F, colors=pal))
#*************************************************************************************

#first is identifying discussed product features & analyzing sentiment towards them

install.packages('udpipe')

library('udpipe')
library(lattice)
library(wordcloud)
library(RColorBrewer)
library('sentimentr')
library(dplyr)


summary(compass_df)
summary(seltos_df)

summary(hector_df)


#reviews_df<- reviews_df %>% filter(Brand.Name=="Jeep Compass")

english_model = udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

summary(english_model)
head(english_model,5)
x <- udpipe_annotate(english_model, x = compass_df$Review.Text, parser = "none", trace = FALSE)
x <- as.data.frame(x)
head(x)
summary(x)
head(x,1)
all_nouns = x %>% subset(., xpos %in% c("NNS") ) # subset all the proper noun in corpus
top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
head(top_nouns, 5) 
pal <- brewer.pal(8,"Dark2")
wordcloud(top_nouns$key,top_nouns$freq,min.freq = 0,max.words = 100,colors=pal)

###########################noun phrases#############################################################################################################

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos") # recode upos to 1-letter tag for better regex pattern

stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",                        #####noun phrase
                          is_regex = TRUE, detailed = FALSE)  

stats <- subset(stats, ngram > 0 & freq >0)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

####RAKE MEthod 3######################################################################################################################################

stats <- keywords_rake(x = x, term = "lemma", group = c("doc_id"), 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 0), 20), col = "cadetblue", 
         main = "jeep compass", 
         xlab = "Rake")

############################################# seltos rake##############################
english_model = udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

summary(english_model)
head(english_model,5)
x <- udpipe_annotate(english_model, x = seltos_df$Review.Text, parser = "none", trace = FALSE)
x <- as.data.frame(x)
head(x)
summary(x)
head(x,1)
all_nouns = x %>% subset(., xpos %in% c("NNS") ) # subset all the proper noun in corpus
top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
head(top_nouns, 5) 
pal <- brewer.pal(8,"Dark2")
wordcloud(top_nouns$key,top_nouns$freq,min.freq = 0,max.words = 100,colors=pal)

###########################noun phrases#############################################################################################################

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos") # recode upos to 1-letter tag for better regex pattern

stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",                        #####noun phrase
                          is_regex = TRUE, detailed = FALSE)  

stats <- subset(stats, ngram > 0 & freq >0)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

####RAKE MEthod 3######################################################################################################################################

stats <- keywords_rake(x = x, term = "lemma", group = c("doc_id"), 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 0), 20), col = "cadetblue", 
         main = "Kia seltos", 
         xlab = "Rake")
####################################################### hector#########################################
english_model = udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

summary(english_model)
head(english_model,5)
x <- udpipe_annotate(english_model, x = hector_df_df$Review.Text, parser = "none", trace = FALSE)
x <- as.data.frame(x)
head(x)
summary(x)
head(x,1)
all_nouns = x %>% subset(., xpos %in% c("NNS") ) # subset all the proper noun in corpus
top_nouns = txt_freq(all_nouns$lemma)  # txt_freq() calcs noun freqs in desc order
head(top_nouns, 5) 
pal <- brewer.pal(8,"Dark2")
wordcloud(top_nouns$key,top_nouns$freq,min.freq = 0,max.words = 100,colors=pal)

###########################noun phrases#############################################################################################################

x$phrase_tag <- as_phrasemachine(x$upos, type = "upos") # recode upos to 1-letter tag for better regex pattern

stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",                        #####noun phrase
                          is_regex = TRUE, detailed = FALSE)  

stats <- subset(stats, ngram > 0 & freq >0)

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))

barchart(key ~ freq, data = head(stats, 20), col = "cadetblue", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

####RAKE MEthod 3######################################################################################################################################

stats <- keywords_rake(x = x, term = "lemma", group = c("doc_id"), 
                       relevant = x$upos %in% c("NOUN", "ADJ"))
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 0), 20), col = "cadetblue", 
         main = "MG Hector", 
         xlab = "Rake")

############### sentiment analysis  #######################################################################################################

feature_list <- c('service','centre','jeep', 'compass','quality','bad','best', 'day', 'vehicle',  'km', 'segment', 'team','ride')
df <- x[,1:4] # select doc_id, par_id, sentence_id, sentence

summary(df)
count(df)
df <- df[!duplicated(df),] # remove duplicate sentences Why? check dataframe x
head(df)
df

sentiment<-sentiment_by(df$sentence)         
summary(df)
df$sent_sentiment <- sentiment$ave_sentiment


#filter sentences based on feature list
df$feature<-NA

# extracting sentiment of features
df$sentence <- tolower(df$sentence) #to get maximum sentences

for (feature in feature_list){
  #print(i)
  df$feature <- ifelse(grepl(feature,df$sentence),feature,df$feature)
}

head(df[!is.na(df$feature),])

summary(df)


df %>% select(doc_id,sent_sentiment,feature)%>%group_by(feature)%>%summarise(mean_sentiment = mean(sent_sentiment))

df%>%filter(feature=="quality")%>%select(sentence,sent_sentiment)


