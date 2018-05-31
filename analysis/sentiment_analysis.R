library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)
library(tidytext)

dat_raw <- 
  # read_csv(here("data", "derived", "All_Opinion_Units_Sentiment_Topic.csv")) 
  read_csv(here("data", "derived", "full_opinion_units_2.csv"))

dat_unsplit <- 
  dat_raw %>% 
  rowwise() %>% 
  mutate(
    sub_ratings_split = split_subratings(sub_ratings) %>% list(),
    rating_perc = ifelse(is.na(ratings), NA_character_, 
                         parse(text = ratings) %>% eval())
  ) %>% 
  select(-sub_ratings) %>% 
  rename(rating = ratings, 
         sentiment = Sentiment,
         probability_sentiment = `Sentiment Probability`,
         categories_full = `Topic Classification`,
         probability_unit_full = `Topic Probability`
         )



dat <- 
  dat_unsplit %>% 
  # unnest() %>% 
  mutate(
    category = str_split(categories_full, ":"),
    probability_unit = str_split(probability_unit_full, ":")
  ) %>% 
  select(-ends_with("full")) %>%
  unnest(category, probability_unit, .preserve = sub_ratings_split)

# write_rds(dat, here("data", "derived", "dat.rds"))


dat_w_nums <- dat %>% 
  rowwise() %>% 
  mutate(
    doc_identifier = str_c("r", review_num, "p", page_num, sep = "_"),
    sentiment_num = switch(sentiment, 
                           "Negative" = -1,
                           "Neutral" = 0,
                           "Positive" = 1)
  ) %>% 
  ungroup()

uuids <- dat_w_nums %>% 
  arrange(page_num, review_num) %>%
  select(content, doc_identifier) %>% 
  nest(-doc_identifier) %>% 
  mutate(doc_uuid = nrow(.) - row_number() + 1) %>% 
  select(-data)

dat_w_nums <- dat_w_nums %>% 
  left_join(uuids)




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
ggplot(dat_clean) +
  # geom_smooth(aes(-1*(page_num*99 + review_num), sentiment_num),
  #             colour = "blue") +
  # geom_smooth(aes(-1*(page_num*99 + review_num), rating_perc %>% as.numeric()),
  #             colour = "red") +
  geom_smooth(aes(doc_uuid, sentiment_num),
              colour = "blue") +
  geom_smooth(aes(doc_uuid, rating_perc %>% as.numeric()),
              colour = "red")


dat_clean %>% 
  group_by(doc_uuid) %>%
  select(rating_perc)
  summarise(
    mean_rating = mean(rating_perc %>% as.numeric(), na.rm = TRUE)
  )






# ------ Words ------

dat_tokens_unnested <- 
  dat_clean %>% 
    nest(-content, -doc_uuid) %>% 
    unnest_tokens(word, content) %>% 
    anti_join(stop_words, "word") %>%
    dobtools::find_nums() %>% 
    filter(contains_num == FALSE) %>% 
    left_join(tidytext::sentiments %>% 
                rename(word_sentiment = sentiment,
                       score_sentiment = score), 
              by = "word") %>% 
    select(-is_num, -contains_num) 
  
# Initial summary
dat_tokens_unnested_tfidf %>% 
  group_by(word, n_words_total) %>% 
  summarise(
    mean_sentiment_num = mean(sentiment_num)
  )
  
# Counts for overall word and within doc word
dat_tokens_unnested_counts <- 
  dat_tokens_unnested %>% 
  add_count(word) %>% 
  rename(
    n_words_total = n
  ) %>% 
  group_by(doc_uuid) %>% 
  add_count(word) %>% 
  rename(
    n_words_this_doc = n
  )  %>% 
  ungroup()

# Get tfidf
dat_tokens_unnested_tfidf <- 
  dat_tokens_unnested_counts %>% 
  bind_tf_idf(word, doc_uuid, n_words_this_doc)


dat_tokens_unnested_tfidf_unnested <- 
  dat_tokens_unnested_tfidf %>% 
  unnest()


# Most distinctive words per category, per sentiment
dat_tokens_unnested_tfidf_unnested %>% 
  group_by(category, sentiment_num) %>% 
  filter(sentiment_num != 0) %>% 
  filter(tf_idf == max(tf_idf)) %>% 
  select(word, word_sentiment, category, sentiment_num, tf_idf) %>% 
  distinct(word, category, sentiment_num, tf_idf) %>% 
  arrange(category, word)

# Most distinctive words per category
dat_tokens_unnested_tfidf_unnested %>% 
  group_by(category) %>% 
  filter(tf_idf == max(tf_idf)) %>% 
  select(word, word_sentiment, category, tf_idf) %>% 
  distinct(word, category, tf_idf) %>% 
  arrange(category, word)


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














