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
  select(-ends_with("full")) 

dat_w_nums <- dat %>% 
  rowwise() %>% 
  mutate(
    sentiment_num = switch(sentiment, 
                           "Negative" = -1,
                           "Neutral" = 0,
                           "Positive" = 1)
  ) %>% 
  ungroup()

dat_clean <-
  dat_w_nums %>% 
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


# Filled bars
ggplot(dat_clean) +
  geom_bar(aes(category, fill = sentiment), position = "fill")


ggplot(dat_clean) +
  geom_bar(aes(sentiment, fill = sentiment), position = "dodge")



# How have reviews changed over time?
ggplot(dat_clean %>% 
         distinct(content, .keep_all = TRUE) %>% 
         mutate(row_num = row_number())) +
  geom_smooth(aes(row_num, sentiment_num))





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

# How does MLs's classifications compare to 
two_sentiments <- 
  dat_tokens_unnested %>% 
  group_by(word, sentiment, sentiment_num, n_words_total) %>% 
  summarise(
    mean_word_score = mean(score, na.rm = TRUE)
  ) %>% 
  drop_na(mean_word_score) %>% 
  arrange(desc(mean_word_score))


ggplot(two_sentiments) +
  geom_smooth(aes(mean_word_score, sentiment_num))


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





# - TFIDF per category for positive and negative ratings
# - See how subratings match up with categories
# - Find representative complaints/praise for each category


replace_y <- function (x, replacement = NA_character_) {
  if (is.null(x) || length(x) == 0 || length(x[[1]]) == 0) {
    replacement
  }
  else {
    x
  }
}

category_reg <- 
  str_c(dat_clean$category %>% tolower() %>% unique(), collapse = "|")


loves <- 
  dat_clean %>% 
  filter(
    str_detect(content, "love the")
  ) %>% 
  distinct(content) %>% 
  rowwise() %>% 
  mutate(
    love_the = str_extract(content, "(?<=love the |Love the).*$"),
    love_what = str_extract_all(love_the, category_reg) %>% replace_y() # %>% str_c(collapse = "; ") 
  ) %>% 
  unnest()


search_for <- function(df = dat_clean, col = content, word = "love", prepend_the = FALSE) {
  word_capped <- 
    dobtools::simple_cap(word)
  
  q_col = enquo(col)
  
  look_for <- ifelse(prepend_the == TRUE,
                     glue("{word} the |{word_capped} the"),
                     glue("{word} |{word_capped} "))
  
  out <- 
    df %>% 
    filter(
      str_detect(!!q_col, look_for)
    ) %>% 
    distinct(!!q_col) %>% 
    rowwise() %>% 
    mutate(
      blank_the = str_extract(!!q_col, glue("(?<={word} the |{word_capped} the).*$")),
      blank_what = str_extract_all(blank_the, category_reg) %>% replace_y() # %>% str_c(collapse = "; ") 
    ) %>% 
    unnest()
  
  return(out)
}


search_for(word = "love", prepend_the = TRUE)
search_for(word = "dislike")
search_for(word = "hate")
search_for(word = "use")
search_for(word = "never use")














