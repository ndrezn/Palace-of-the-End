library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
data(stop_words)

#read from file
harrowdownRaw <- readLines("/Users/nathandrezner/OneDrive - McGill University/McGill/U2/Fall Semester/ENGL 395_ Drama/presentation/texts_by_paragraph/Harrowdown_Hill.txt")
instrumentsRaw <- readLines("/Users/nathandrezner/OneDrive - McGill University/McGill/U2/Fall Semester/ENGL 395_ Drama/presentation/texts_by_paragraph/Instruments_of_Yearning.txt")
pyramidsRaw <- readLines("/Users/nathandrezner/OneDrive - McGill University/McGill/U2/Fall Semester/ENGL 395_ Drama/presentation/texts_by_paragraph/My_Pyramids.txt")

#convert to data frame
pyramids <- data_frame(line = 1:122, text = pyramidsRaw)
harrowdown <- data_frame(line = 1:104, text = harrowdownRaw)
instruments <- data_frame(line = 1:114, text = instrumentsRaw)

#seperate strings into individual words
harrowdown <- harrowdown %>%
  unnest_tokens(word, text)
pyramids <- pyramids %>%
  unnest_tokens(word, text)
instruments <- instruments %>%
  unnest_tokens(word, text)

#remove stop words
harrowdown <- harrowdown %>%
  anti_join(stop_words)
pyramids <- pyramids %>%
  anti_join(stop_words)
instruments <- instruments %>%
  anti_join(stop_words)

#collect sentiment analysis using Bing Liu database
#all words categorized between two sentiments
harrowdown_bing <- harrowdown %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, paragraph=line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
pyramids_bing <- pyramids %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, paragraph=line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
instruments_bing <- instruments %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, paragraph=line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#colect sentiment analysis using Saif Mohammad and Peter Turney database
#10 sentiments: positive, negative, anger, anticipation, disgust, 
#fear, joy, sadness, surprise, and trust
instruments_nrc <- instruments %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0)
harrowdown_nrc <- harrowdown %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0)
pyramids_nrc <- pyramids %>%
  inner_join(get_sentiments("nrc")) %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0)

#collect sentiment analysis using Finn Årup Nielsen database
pyramids_afinn <- pyramids %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word, paragraph = line) %>% 
  summarise(sentiment = sum(score))
harrowdown_afinn <- harrowdown %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word, paragraph = line) %>% 
  summarise(sentiment = sum(score)) 
instruments_afinn <- instruments %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word, paragraph = line) %>% 
  summarise(sentiment = sum(score)) 