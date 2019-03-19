#INSTRUMENTS
#sentiment analysis using afinn
i_afinn <- instruments %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
#sentiment analysis using bing & nrc
i_bing_and_nrc <- bind_rows(instruments %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            instruments %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#HARROWDOWN
#sentiment analysis using afinn
h_afinn <- harrowdown %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
#sentiment analysis using bing & nrc
h_bing_and_nrc <- bind_rows(harrowdown %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            instruments %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#PYRAMIDS
#sentiment analysis using afinn
p_afinn <- pyramids %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = line) %>% 
  summarise(sentiment = sum(score)) %>% 
  mutate(method = "AFINN")
#sentiment analysis using bing & nrc
p_bing_and_nrc <- bind_rows(pyramids %>% 
                              inner_join(get_sentiments("bing")) %>%
                              mutate(method = "Bing et al."),
                            instruments %>% 
                              inner_join(get_sentiments("nrc") %>% 
                                           filter(sentiment %in% c("positive", 
                                                                   "negative"))) %>%
                              mutate(method = "NRC")) %>%
  count(method, index = line, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#instruments visualization
bind_rows(i_afinn, 
          i_bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

#harrowdown visualization
bind_rows(h_afinn, 
          h_bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
#pyramids visualization
bind_rows(p_afinn, 
          p_bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
