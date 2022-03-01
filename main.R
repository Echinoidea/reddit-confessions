library(data.table)
library(qdap)
library(tm)

df <- as.data.frame(fread("one-million-reddit-confessions.csv", select = c("selftext", "title"), nrows = 100000))

# Create definition corpus
conf_corpus <- Corpus(VectorSource(df$title[1:100000]))

conf_clean <- conf_corpus

custom_stopwords <- c(stopwords('en'), 'and')

clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(function(x) gsub("â€™", "\'", x)))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, words = custom_stopwords)
  
  return(corpus)
}

conf_clean <- clean_corpus(conf_corpus)

content(conf_corpus[[2]])
content(conf_clean[[2]])

conf_tdm <- TermDocumentMatrix(conf_clean)

conf_m <- as.matrix(conf_tdm)
term_frequency <- rowSums(conf_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)


df_clean <- as.data.frame(conf_clean)
#word_associate(text.var = df_clean$text[1:100], match.string = "friend", 
               # stopwords = c(Top200Words, "amp"), 
               # network.plot = TRUE, cloud.colors = c("gray85", "darkred"), proportional=TRUE, nw.label.proportional = TRUE, wordcloud=TRUE)


library(syuzhet)

df_clean$sentiment <- get_sentiment(df_clean$text, method = "afinn")
average_sentiment <- mean(df_clean$sentiment)


library(dplyr)
# Find all documents containing words friend, love, men, women, family, etc
docs_friend <- filter(df_clean, grepl("friend|friends", text))
docs_boyfriend <- filter(df_clean, grepl("boyfriend", text))
docs_girlfriend <- filter(df_clean, grepl("girlfriend", text))
docs_husband <- filter(df_clean, grepl("husband", text))
docs_wife <- filter(df_clean, grepl("wife", text))
docs_ex <- filter(df_clean, grepl("ex|exs", text))
docs_men <- filter(df_clean, grepl("men|man", text))
docs_women <- filter(df_clean, grepl("women|woman", text))
docs_family <- filter(df_clean, grepl("family", text))
docs_hate <- filter(df_clean, grepl("hate", text))
docs_love <- filter(df_clean, grepl("love", text))
docs_school <- filter(df_clean, grepl("school", text))
docs_mom <- filter(df_clean, grepl("mom|mother", text))
docs_dad <- filter(df_clean, grepl("dad|father", text))
docs_cheat <- filter(df_clean, grepl("cheat|cheated", text))

sentiments_df <- data.frame(
    term = c("friend", "boyfriend", "girlfriend", "husband", "wife", 
           "ex", "men", "women", "family", "hate", 
           "love", "school", "mother", "father", "cheat"),
    sentiment = c(mean(docs_friend$sentiment), 
                mean(docs_boyfriend$sentiment),
                mean(docs_girlfriend$sentiment),
                mean(docs_husband$sentiment),
                mean(docs_wife$sentiment),
                mean(docs_ex$sentiment),
                mean(docs_men$sentiment),
                mean(docs_women$sentiment),
                mean(docs_family$sentiment),
                mean(docs_hate$sentiment),
                mean(docs_love$sentiment),
                mean(docs_school$sentiment),
                mean(docs_mom$sentiment),
                mean(docs_dad$sentiment),
                mean(docs_cheat$sentiment)))

sentiments_df

sentiments_df$variance <- ((average_sentiment - sentiments_df$sentiment) / average_sentiment) * 100

sentiments_df <- sentiments_df %>%
    mutate(pos = variance >= 0)


library(ggplot2)

theme_reddit <- theme(
  plot.background = element_rect(fill = "#201c1c"),
  panel.background = element_rect(fill = "#201c1c", 
                                  colour = "#201c1c", 
                                  size = 0.5, 
                                  linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "#2c2e2e"),
  panel.grid.minor = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "#2c2e2e"),
  text = element_text(colour = "#d0d7d7"),
  axis.text = element_text(colour = "#d0d7d7")
)


ggplot(sentiments_df, aes(x = variance, y = reorder(term, variance), fill = pos, tidyverse::fct_reorder(term, variance))) +
  geom_col(width = 0.5) +
  scale_fill_manual(values=c("#48a5de", "#ea4304")) +
  scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(-350, 350)) +
  labs(x = paste("Percent Difference from Average Sentiment Score (", average_sentiment, ")"),
       y = NULL,
       title = "Variance of Sentiment of 100,000 Reddit Confessions",
       subtitle = "grouped by confessions containing specific terms") +
  theme_bw() +
  theme_reddit +
  theme(legend.position = "none")


sentiment_love <- docs_love$sentiment
sentiment_hate <- docs_hate$sentiment
sentiment_boyfriend <- docs_boyfriend$sentiment
sentiment_girlfriend <- docs_girlfriend$sentiment

max_len <- max(c(length(sentiment_love),
                 length(sentiment_hate),
                 length(sentiment_boyfriend),
                 length(sentiment_girlfriend)))


df_combined <- data.frame(love = c(sentiment_love, rep(NA,  max_len - length(sentiment_love))),
                          boyfriend = c(sentiment_boyfriend, rep(NA, max_len - length(sentiment_boyfriend))),
                          hate = c(sentiment_hate, rep(NA, max_len - length(sentiment_hate))),
                          girlfriend = c(sentiment_girlfriend, rep(NA, max_len - length(sentiment_girlfriend)))
)

library(reshape2)
df_melt <- melt(df_combined)

ggplot(df_melt, aes(value, fill = variable)) +
  geom_density() +
  coord_cartesian(xlim = c(-12, 12)) +
  facet_wrap(~variable) +
  theme_bw()
