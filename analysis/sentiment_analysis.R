library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)
library(tidytext)

dat_raw <- 
  read_csv(here("data", "derived", "All_Opinion_Units_Sentiment_Topic.csv")) 

dat_unsplit <- 
  dat_raw %>% 
  rename(content = Text,
         sentiment = Sentiment,
         probability_sentiment = Probability,
         categories_full = `Categories (Full-text)`,
         probability_unit_full = `Probabilities (Full-text)`
         )

dat <- 
  dat_unsplit %>% 
  mutate(
    category = str_split(categories_full, ":"),
    probability_unit = str_split(probability_unit_full, ":")
  ) %>% 
  unnest() %>% 
  select(-ends_with("full")) %>% 
  rowwise() %>% 
  mutate(
    sentiment_num = switch(sentiment, 
                           "Negative" = -1,
                           "Neutral" = 0,
                           "Positive" = 1)
  ) %>% 
  ungroup()

dat_clean <-
  dat %>% 
  filter(!is.na(probability_unit) & !is.na(probability_unit) & 
           category != "None" &
           probability_sentiment > 0.55 & probability_unit > 0.55)


# Summarise sentiment by category 
sentiment_by_category <- 
  dat_clean %>% 
  group_by(category) %>% 
  summarise(
    mean_sentiment = mean(sentiment_num)
  ) %>% 
  arrange(mean_sentiment)

# Breakdown by category
ggplot(dat_clean) +
  geom_bar(aes(category))

# Breakdown by category and sentiment
ggplot(sentiment_by_category %>% 
         arrange(desc(mean_sentiment))) +
  geom_bar(aes(category, mean_sentiment), stat = "identity") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










# ------ Words ------

dat_tokens_unnested <- 
  dat_clean %>% 
  unnest_tokens(word, content) %>% 
  anti_join(stop_words, "word") %>%
  dobtools::find_nums() %>% 
  filter(contains_num == FALSE) %>% 
  add_count(word) %>% 
  rename(
    n_words_total = n
  )

dat_tokens_unnested <-  
  dat_tokens_unnested %>% 
  # select(-is_num, -contains_num) %>% 
  left_join(tidytext::sentiments %>% 
              rename(word_sentiment = sentiment,
                     score_sentiment = score), 
            by = "word")

# Need full content (document) to bind tf idf 

dat_tokens_unnested %>% 
  group_by(word, n_words_total) %>% 
  summarise(
    mean_sentiment_num = mean(sentiment_num)
  )


# Do words that come at the beginning of the alphabet have higher sentiment?
dat_tokens_unnested_first_letter <- 
  dat_tokens_unnested %>% 
  rowwise() %>% 
  mutate(
    first_letter = substr(word, 1, 1),
    first_letter_num = which(letters == first_letter)
  ) 


# Plot sentiment by first letter
ggplot(dat_tokens_unnested_first_letter) +
  geom_smooth(aes(first_letter_num, sentiment_num))

ggplot(dat_tokens_unnested_first_letter) +
  geom_smooth(aes(first_letter_num, score))




