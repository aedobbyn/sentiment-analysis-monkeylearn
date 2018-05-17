library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)

source(here("key.R"))

reviews_with_subratings_nested <- read_csv(here("data", "derived", "reviews_with_subratings_nested.rds"))
reviews_with_subratings_unnested <- read_csv(here("data", "derived", "capterra_slack_reviews_with_subratings_unnested.csv"))

# Default replacement for NULLs
replacement_classifier <- tribble(
  ~category_id, ~probability, ~label,
  NA_character_, NA_character_, NA_character_
) %>% list()


replacement_extractor <- tribble(
  ~count, ~tag, ~entity,
  NA_character_, NA_character_, NA_character_
) 

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
unnest_result_classifier <- function(df) {
  out <- df %>% 
    rowwise() %>% 
    mutate(
      res = ifelse(length(res)[[1]] == 0, replacement_classifier, res) 
    ) %>% 
    unnest(res)
  
  return(out)
}

unnest_result_extractor <- function(df) {
  out <- df 
  df$res <- df$res %>% 
    map(dobtools::replace_x, replacement = replacement_extractor)
  
  out <- df %>% 
    unnest(res)
  
  return(out)
}
# Create a trycatch that will return a list with two elements; a result and error, one of which will always be NULL
try_unnest_result_extractor <- safely(unnest_result_extractor)
try_unnest_result_classifier <- safely(unnest_result_classifier)

# Define directory to put topic batches
topic_batches_dir <- here("data", "derived", "topic_batches")


# Helpers for the trycatch 
get_classification_batch <- 
  safely(monkey_classify)

get_extraction_batch <- 
  safely(monkey_extract)


# Take a dataframe and a classifier/extractor id, send batches of `n_texts_per_batch` at a time to the API, store each processed batch in `dir`,
# and log any errors that occur
write_batches <- function(df, id, dir, 
                          n_texts_per_batch,
                          start_row = 1,
                          unnest = FALSE,
                          write_out = TRUE, ...) {
  if (substr(id, 1, 3) == "cl_") {
    type_of_problem <- "classification"
  } else if (substr(id, 1, 3) == "ex_") {
    type_of_problem <- "extraction"
  } else {
    stop("Not a recognized classifier or extractor id.")
  }
  
  resp <- tibble()
  n_df_rows <- nrow(df)
  
  batch_start_row <- start_row
  batch_end_row <- batch_start_row + n_texts_per_batch
  
  error_log <- ""
  
  while(batch_start_row <= n_df_rows) {
    
    # We have a classification
    if (type_of_problem == "classification") {
      this_batch_nested <- get_classification_batch(df[batch_start_row:batch_end_row, ],
                                     col = content,
                                     classifier_id = id, 
                                     unnest = unnest)
      this_batch <- this_batch_nested$result %>% 
        try_unnest_result_classifier()
      
    } else if (type_of_problem == "extraction") {
      this_batch_nested <- get_extraction_batch(df[batch_start_row:batch_end_row, ],
                                     col = content,
                                     extactor_id = id, 
                                     unnest = unnest)
      
      this_batch <- this_batch_nested$result %>% 
        try_unnest_result_extractor()
    } 
    
    message(glue("Processed rows {batch_start_row} to {batch_end_row}."))
  
    
    if (is.null(this_batch_nested$error) && is.null(this_batch$error)) {
      if (write_out == TRUE) {
        write_csv(this_batch$result, 
                  glue("{dir}/{type_of_problem}_batches_rows_{batch_start_row}_to_{batch_end_row}.csv"))
      }
    
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

# Derive more specific funcitons
write_extraction_batches <- function(df, n_texts_per_batch = 200, 
                                     dir = opinion_batches_dir, ...) {
  write_batches(df, id = extractor_id, n_texts_per_batch = n_texts_per_batch,
                dir = dir, ...)
}

write_classification_batches <- function(df, n_texts_per_batch = 200, 
                                         dir = topic_batches_dir, ...) {
  write_batches(df, id = classifier_id, n_texts_per_batch = n_texts_per_batch,
                dir = dir, ...)
}
  



# Test out with the first 10 texts in batches of 2 texts at a time
some_topics_batch_classified <- 
  reviews_with_subratings_nested[1:10, ] %>% 
  write_classification_batches(
                n_texts_per_batch = 2)



# # # Actually classify all the texts (commented out for safety)
# write_batches_res <-
#   reviews_with_subratings_nested %>%
#   write_batches(id = classifier_id,
#               dir = topic_batches_dir,
#               n_texts_per_batch = 2)

# Check the error log
write_batches_res$error

# Read in all the batches and stick them in one long dataframe
gather_batches <- function(dir, end_row) {
  
  fls <- fs::dir_ls(dir)
  
  list_o_batches <- 
    map(fls, 
        read_csv)
  
  out <- list_o_batches %>% 
    map_df(as.character) %>% 
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




# Another test

topic_batches_dir_new <- here("data", "derived", "topic_batches_new")

more_topics_batch_classified <- 
  reviews_with_subratings_nested[1:10, ] %>% 
  write_classification_batches(n_texts_per_batch = 2,
                               dir = topic_batches_dir_new)

more_topics_batch_classified_gathered <- 
  gather_batches(dir = topic_batches_dir_new)




# ------------------ Use data pre-extracted ------------------

all_extracted_opinion_units <- 
  read_csv(here("data", "derived", "All_Opinion_Units_Sentiment_Topic.csv")) %>% 
  rename(content = Text)



foo <- all_extracted_opinion_units[1:9, ] %>% 
  write_classification_batches(n_texts_per_batch = 2,
                               dir = topic_batches_dir_new)

bar <- 
  gather_batches(dir = topic_batches_dir_new)


foo_one <- 
  read_csv(glue(topic_batches_dir_new, "/classification_batches_rows_1_to_3.csv")) %>% 
  map_df(as.character)

foo_two <- 
  read_csv(glue(topic_batches_dir_new, "/classification_batches_rows_3_to_5.csv")) %>% 
  map_df(as.character)

foo_five <- 
  read_csv(glue(topic_batches_dir_new, "/classification_batches_rows_9_to_9.csv")) %>% 
  map_df(as.character)


bind_rows(foo_one, foo_two)








foo <- all_extracted_opinion_units[1:9, ] %>% 
  write_classification_batches(n_texts_per_batch = 2,
                               dir = topic_batches_dir_new)




