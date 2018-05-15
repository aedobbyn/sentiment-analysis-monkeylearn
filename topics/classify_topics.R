library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)

source(here("key.R"))

reviews_with_subratings_nested <- read_csv(here("data", "derived", "reviews_with_subratings_nested.rds"))
reviews_with_subratings_unnested <- read_csv(here("data", "derived", "capterra_slack_reviews_with_subratings_unnested.csv"))

# Default replacement for NULLs
replacement <- tribble(
  ~category_id, ~probability, ~label,
  NA_character_, NA_character_, NA_character_
) %>% list()

# Test out this particular classifier
sample_topics_raw <- 
  monkey_classify(reviews_with_subratings_nested[1:3, ], col = content,
                  classifier_id = classifier_id, unnest = FALSE) 

# Make adjustments when unnesting; replace NULLs with NAs
sample_topics_unnested <- 
  sample_topics_raw %>% 
  rowwise() %>% 
  mutate(
    res = ifelse(length(res)[[1]] == 0, replacement, res) 
  ) %>% 
  unnest(res)


# Helper for doing that adjustment above
unnest_result <- function(df) {
  out <- df %>% 
    rowwise() %>% 
    mutate(
      res = ifelse(length(res)[[1]] == 0, replacement, res) 
    ) %>% 
    unnest(res)
  
  return(out)
}

# Create a trycatch that will return a list with two elements; a result and error, one of which will always be NULL
try_unnest_result <- safely(unnest_result)


# Define directory to put topic batches
topic_batches_dir <- here("data", "derived", "topic_batches")


# Helpers for the trycatch 
get_classification_batch <- 
  safely(monkey_classify)

get_extraction_batch <- 
  safely(monkey_extract)


# Take a dataframe, send batches of `n_texts_per_batch` at a time to the API, store each processed batch in `dir`,
# and log any errors that occur
write_batches <- function(df, id, dir, 
                          n_texts_per_batch,
                          start_row = 1, ...) {
  resp <- tibble()
  n_df_rows <- nrow(df)
  
  batch_start_row <- start_row
  batch_end_row <- batch_start_row + n_texts_per_batch
  
  error_log <- ""
  
  while(batch_start_row <= n_df_rows) {
    
    # We have a classification
    if (substr(id, 1, 3) == "cl_") {
      this_batch_nested <- get_classification_batch(df[batch_start_row:batch_end_row, ],
                                     col = content,
                                     classifier_id = id, 
                                     unnest = FALSE)
    } else if (substr(id, 1, 3) == "ex_") {
      this_batch_nested <- get_extraction_batch(df[batch_start_row:batch_end_row, ],
                                     col = content,
                                     extactor_id = id, 
                                     unnest = FALSE)
    } else {
      message("Not a recognized classifier or extractor id.")
    }
    
    
    message(glue("Processed rows {batch_start_row} to {batch_end_row}."))
  
    this_batch <- this_batch_nested$result %>% 
      try_unnest_result()
    
    if (is.null(this_batch_nested$error) && is.null(this_batch$error)) {
      write_csv(this_batch$result, 
                glue("{dir}/topic_batches_rows_{batch_start_row}_to_{batch_end_row}.csv"))
      
      resp <- resp %>% 
        bind_rows(this_batch$result)
      
    } else {
      error_log <- error_log %>% 
        c(glue("Error between rows {batch_start_row} and {batch_end_row}: 
               {c(this_batch_nested$error, this_batch$error)}"))
      
      message(error_log)
    }
    
    batch_start_row <- batch_start_row + n_texts_per_batch
    batch_end_row <- batch_start_row + n_texts_per_batch
    
    if (batch_end_row > n_df_rows) {
      batch_end_row <- n_df_rows
    }
  }
  
  out <- list(resp = resp, 
              error_log = error_log)
  
  return(out)
}

# Test out with the first 10 texts in batches of 2 texts at a time
some_topics_batch_classified <- 
  reviews_with_subratings_nested[1:10, ] %>% 
  write_batches(dir = topic_batches_dir,
                n_texts_per_batch = 2)


# # # Actually classify all the texts (commented out for safety)
# write_batches_res <- 
#   reviews_with_subratings_nested %>% 
#   write_batches(dir = topic_batches_dir, n_texts_per_batch = 2)

# Check the error log
write_batches_res$error

# Read in all the batches and stick them in one long dataframe
gather_batches <- function(dir, end_row) {
  
  fls <- fs::dir_ls(dir)
  
  list_o_batches <- 
    map(fls, 
        read_csv)
  
  out <- list_o_batches %>% 
    bind_rows()
  
  return(out)
}

all_topics_parcelled <- gather_batches(dir = topic_batches_dir)

# Unnest the subratings
all_topics_parcelled_unnested <- 
  all_topics_parcelled %>% 
  unnest(sub_ratings_df)


# # Save these
# write_rds(all_topics_parcelled, here("data", "derived", "all_topics_parcelled.rds"))
# write_csv(all_topics_parcelled_unnested, here("data", "derived", "all_topics_parcelled_unnested.csv"))


