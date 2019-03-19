library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
data(stop_words)

#read from file
harrowdownRaw <- readLines("/Users/nathandrezner/OneDrive - McGill University/McGill/U2/Fall Semester/ENGL 395_ Drama/presentation/texts_unsorted/Harrowdown_Hill_no_sort.txt")
instrumentsRaw <- readLines("/Users/nathandrezner/OneDrive - McGill University/McGill/U2/Fall Semester/ENGL 395_ Drama/presentation/texts_unsorted/Instruments_of_Yearning_no_sort.txt")
pyramidsRaw <- readLines("/Users/nathandrezner/OneDrive - McGill University/McGill/U2/Fall Semester/ENGL 395_ Drama/presentation/texts_unsorted/My_Pyramids_no_sort.txt")
#i represents the number of words in each cluster
i = 50
#split harrowdown into groups
y <- strsplit(harrowdownRaw, split = " ")[[1]]
group_num <- length(y) %/% i
group_last <- length(y) %% i

harrowdownRaw <- tapply(y, c(rep(1:group_num, each = i), 
                 rep(group_num + 1, times = group_last)),
            toString)
#split instruments into groups
y <- strsplit(instrumentsRaw, split = " ")[[1]]
group_num <- length(y) %/% i
group_last <- length(y) %% i
instrumentsRaw <- tapply(y, c(rep(1:group_num, each = i), 
                             rep(group_num + 1, times = group_last)),
                        toString)
#split pyramids into groups
y <- strsplit(pyramidsRaw, split = " ")[[1]]
group_num <- length(y) %/% i
group_last <- length(y) %% i
pyramidsRaw <- tapply(y, c(rep(1:group_num, each = i), 
                              rep(group_num + 1, times = group_last)),
                         toString)

#convert to data frame
harrowdown <- data_frame(line = 1:length(harrowdownRaw), text = harrowdownRaw)
pyramids <- data_frame(line = 1:length(pyramidsRaw), text = pyramidsRaw)
instruments <- data_frame(line = 1:length(instrumentsRaw), text = instrumentsRaw)

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
harrowdown_bing_wds <- harrowdown %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index=line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
pyramids_bing_wds <- pyramids %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index=line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
instruments_bing_wds <- instruments %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, index=line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#collect sentiment analysis using Finn Årup Nielsen database
pyramids_afinn_wds <- pyramids %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word, index = line) %>% 
  summarise(sentiment = sum(score))
harrowdown_afinn_wds <- harrowdown %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word, index = line) %>% 
  summarise(sentiment = sum(score)) 
instruments_afinn_wds <- instruments %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(word, index = line) %>% 
  summarise(sentiment = sum(score)) 