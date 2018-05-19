library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)

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




ggplot(sentiment_by_category) +
  geom_bar(aes(category, mean_sentiment), stat = "identity")








