

library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)

source(here("key.R"))

all_reviews_slack <- read_csv(here("data", "derived", "all_reviews_slack.csv"))

reviews_with_subratings_nested <- read_csv(here("data", "derived", "reviews_with_subratings_nested.rds"))
reviews_with_subratings_unnested <- read_csv(here("data", "derived", "capterra_slack_reviews_with_subratings_unnested.csv"))


opinion_batches_dir <- here("data", "derived", "opinion_batches")

# Make sure behavior is the same
sample_opinions_raw <- 
  monkey_extract(reviews_with_subratings_nested[1:3, ], col = content,
                  extractor_id = extractor_id, unnest = FALSE)

sample_opinions_unnested <- 
  sample_opinions_raw %>% 
  rowwise() %>% 
  mutate(
    res = ifelse(length(res)[[1]] == 0, replacement, res) 
  ) %>% 
  unnest(res)


# Extract a few 
some_opinion_batches_extracted <- 
  reviews_with_subratings_nested[1:10, ] %>% 
  write_batches(dir = opinion_batches_dir, 
                n_texts_per_batch = 2)


# Do all the opinion extracting
opinion_batches_extracted <- 
  reviews_with_subratings_nested %>% 
  write_batches(dir = opinion_batches_dir, 
                n_texts_per_batch = 2)


