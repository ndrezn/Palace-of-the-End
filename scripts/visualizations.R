library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)

#afinn visualization (paragraph cluster)
h_afinn_plot <- ggplot(harrowdown_afinn, aes(paragraph, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Harrowdown Hill\" by paragraph (Finn Årup Nielsen)")
p_afinn_plot <- ggplot(pyramids_afinn, aes(paragraph, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiments of \"My Pyramids\" by paragraph (Finn Årup Nielsen)")
i_afinn_plot <- ggplot(instruments_afinn, aes(paragraph, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Instruments of Yearning\" by paragraph (Finn Årup Nielsen)")

#bing visualization (paragraph cluster)
h_bing_plot <- ggplot(harrowdown_bing, aes(paragraph, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Harrowdown Hill\" by paragraph (Bing Liu)")
p_bing_plot <- ggplot(pyramids_bing, aes(paragraph, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiments of \"My Pyramids\" by paragraph (Bing Liu)")
i_bing_plot <- ggplot(instruments_bing, aes(paragraph, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Instruments of Yearning\" by paragraph (Bing Liu)")

#nrc visualization
sentiment<-c("anger", "anticipation", "disgust", "fear", "joy",
             "negative", "positive", "sadness", "surprise", "trust")

percentage<-as.matrix(pyramids_nrc)[1,]/1352
data<-data.frame(sentiment, percentage)
p_nrc_plot <- ggplot(data, aes(sentiment, percentage, fill=sentiment))+
  labs(title ="Sentiments of \"My Pyramids\" as a percentage of the text")+
  geom_bar(stat="identity", show.legend=FALSE)+
  scale_y_continuous(labels=percent)
               
percentage<-as.matrix(harrowdown_nrc)[1,]/1160
data<-data.frame(sentiment, percentage)
h_nrc_plot <- ggplot(data, aes(sentiment, percentage, fill=sentiment))+
  labs(title ="Sentiments of \"Harrowdown Hill\" as a percentage of the text")+
  geom_bar(stat="identity", show.legend=FALSE)+
  scale_y_continuous(labels=percent)

percentage<-as.matrix(instruments_nrc)[1,]/1499
data<-data.frame(sentiment, percentage)
i_nrc_plot <- ggplot(data, aes(sentiment, percentage, fill=sentiment)) +
  labs(title ="Sentiments of \"Instruments of Yearning\" as a percentage of the text")+
  geom_bar(stat="identity", show.legend=FALSE)+
  scale_y_continuous(labels=percent)

#afinn visualization: word clusters
h_afinn_plot_wds <- ggplot(harrowdown_afinn_wds, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Harrowdown Hill\" by word cluster (Finn Årup Nielsen)")
p_afinn_plot_wds <- ggplot(pyramids_afinn_wds, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiments of \"My Pyramids\" by word cluster (Finn Årup Nielsen)")
i_afinn_plot_wds <- ggplot(instruments_afinn_wds, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Instruments of Yearning\" by word cluster (Finn Årup Nielsen)")

#bing visualization: word clusters
h_bing_plot_wds <- ggplot(harrowdown_bing_wds, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Harrowdown Hill\" by word cluster (Bing Liu)")
p_bing_plot_wds <- ggplot(pyramids_bing_wds, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiments of \"My Pyramids\" by word cluster (Bing Liu)")
i_bing_plot_wds <- ggplot(instruments_bing_wds, aes(index, sentiment, fill = word)) +
  geom_col(show.legend = FALSE)+
  labs(title ="Sentiment of \"Instruments of Yearning\" by word cluster (Bing Liu)")


#view plots
h_afinn_plot
p_afinn_plot
i_afinn_plot

h_bing_plot
p_bing_plot
i_bing_plot

h_afinn_plot_wds
p_afinn_plot_wds
i_afinn_plot_wds

h_bing_plot_wds
p_bing_plot_wds
i_bing_plot_wds

p_nrc_plot
h_nrc_plot
i_nrc_plot
