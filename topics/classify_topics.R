library(here)
library(tidyverse)
devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)

source(here("key.R"))

reviews_with_subratings <- read_csv(here("data", "derived", "all_reviews_slack.csv"))
reviews_with_subratings_unnested <- read_csv(here("data", "derived", "capterra_slack_reviews_with_subratings_unnested.csv"))



replacement <- tribble(
  ~category_id, ~probability, ~label,
  NA_character_, NA_character_, NA_character_
) %>% list()

sample_topics_raw <- 
  monkey_classify(reviews_with_subratings[1:3, ], col = content,
                  classifier_id = classifier_id, unnest = FALSE) 


sample_topics_unnested <- sample_topics_raw %>% 
  rowwise() %>% 
  mutate(
    res = ifelse(length(res)[[1]] == 0, replacement, res) 
  ) %>% unnest(res)




