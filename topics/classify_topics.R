library(here)
library(tidyverse)
devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)

source(here("key.R"))

reviews_with_subratings <- read_csv(here("data", "derived", "all_reviews_slack.csv"))
reviews_with_subratings_unnested <- read_csv(here("data", "derived", "capterra_slack_reviews_with_subratings_unnested.csv"))


replace_nulls <- function(e, rep) {
  if (length(x) == 0 || is.null(x)) {
    
  }
}

replacement <- tribble(
  ~category_id, ~probability, ~label,
  NA_character_, NA_character_, NA_character_
) %>% list()

sample_topics <- 
  monkey_classify(reviews_with_subratings[1:3, ], col = content,
                  classifier_id = classifier_id, unnest = FALSE) %>% 
  mutate(
    res = res %>% map(dobtools::replace_x, replacement = replacement) # %>% list()
  )


foo[which(length(foo) == 0)] <- replacement


sample_topics %>% unnest()



