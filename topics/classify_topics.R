library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)

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

sample_topics_unnested <- 
  sample_topics_raw %>% 
  rowwise() %>% 
  mutate(
    res = ifelse(length(res)[[1]] == 0, replacement, res) 
  ) %>% 
  unnest(res)



# topics_raw <- 
#   monkey_classify(reviews_with_subratings, col = content,
#                   classifier_id = classifier_id, unnest = FALSE) 

# Processing batch 1 of 13 batches: texts 1 to 200
# Processing batch 2 of 13 batches: texts 200 to 400
# Processing batch 3 of 13 batches: texts 400 to 600
# Processing batch 4 of 13 batches: texts 600 to 800
# Processing batch 5 of 13 batches: texts 800 to 1000
# No results for this call; returning NA.
# Error in cbind_all(x) : Argument 2 must be length 2542, not 200


topic_batches_dir <- here("data", "derived", "topic_batches")


write_batches <- function(df, dir = topic_batches_dir, 
                          n_texts_per_batch,
                          start_row = 1, ...) {
  out <- tibble()
  n_df_rows <- nrow(df)
  
  batch_start_row <- start_row
  batch_end_row <- batch_start_row + n_texts_per_batch
  
  error_log <- ""
  
  while(batch_start_row <= n_df_rows) {
    get_batch <- 
      safely(monkey_classify)
    
    this_batch_nested <- get_batch(df[batch_start_row:batch_end_row, ],
                            col = content,
                            classifier_id = classifier_id, 
                            unnest = FALSE)
    message(glue("Processed rows {batch_start_row} to {batch_end_row}."))
  
    this_batch <- this_batch_nested$result %>% 
      rowwise() %>% 
      mutate(
        res = ifelse(length(res)[[1]] == 0, replacement, res) 
      ) %>% 
      unnest(res)
    
    if (is.null(this_batch_nested$error)) {
      write_csv(this_batch, glue("{dir}/topic_batches_rows_{batch_start_row}_to_{batch_end_row}.csv"))
      
      out <- out %>% 
        bind_rows(this_batch)
      
    } else {
      error_log <- error_log %>% 
        c(glue("Error between rows {batch_start_row} and {batch_end_row} :("))
    }
    
    batch_start_row <- batch_start_row + n_texts_per_batch
    batch_end_row <- batch_start_row + n_texts_per_batch
    
    if (batch_end_row > n_df_rows) {
      batch_end_row <- n_df_rows
    }
  }
  
  return(out)
}



some_topics_batch_classified <- 
  reviews_with_subratings[1:10, ] %>% 
  write_batches(n_texts_per_batch = 2)



topics_raw <- 
  reviews_with_subratings %>% 
  write_batches(n_texts_per_batch = 200)

