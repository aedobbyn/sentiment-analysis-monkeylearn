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


unnest_result <- function(df) {
  out <- df %>% 
    rowwise() %>% 
    mutate(
      res = ifelse(length(res)[[1]] == 0, replacement, res) 
    ) %>% 
    unnest(res)
  
  return(out)
}

try_unnest_result <- safely(unnest_result)


write_batches <- function(df, dir = topic_batches_dir, 
                          n_texts_per_batch,
                          start_row = 1, ...) {
  resp <- tibble()
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
      try_unnest_result()
    
    if (is.null(this_batch_nested$error) && is.null(this_batch$error)) {
      write_csv(this_batch$result, glue("{dir}/topic_batches_rows_{batch_start_row}_to_{batch_end_row}.csv"))
      
      resp <- resp %>% 
        bind_rows(this_batch$result)
      
    } else {
      error_log <- error_log %>% 
        c(glue("Error between rows {batch_start_row} and {batch_end_row} :("))
      
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



some_topics_batch_classified <- 
  reviews_with_subratings[1:10, ] %>% 
  write_batches(n_texts_per_batch = 2)


# # Error between texts 600 and 800
topics_raw <-
  reviews_with_subratings[600:nrow(reviews_with_subratings), ] %>%
  write_batches(n_texts_per_batch = 200)



gather_batches <- function(dir = topic_batches_dir,
                           end_row) {

  fls <- fs::dir_ls(dir)
  
  list_o_batches <- 
    map(fls, 
        read_csv)
  
  out <- list_o_batches %>% 
    bind_rows()
  
  return(out)
}

bar <- gather_batches()



topic_batches_dir <- here::here("data", "derived", "topic_batches")

fls <- fs::dir_ls(topic_batches_dir)
fls_tbl <- tibble(x = fls)
fls_tbl <- data.frame(x = fls) %>% 
  as_tibble()

new_file_names <- fls_tbl %>% 
  separate(x, c("dir", "num_chunk"), "rows_") %>% 
  separate(num_chunk, c("start_num", "end_bit"), "_to_") %>% 
  separate(end_bit, c("end_num", "chuck"), "\\.") %>% 
  mutate(
    start_num = as.numeric(start_num),
    end_num = as.numeric(end_num)
  ) %>% 
  rowwise() %>%
  mutate(
    start_num = ifelse(nchar(start_num) >=3, start_num + 600, start_num),
    end_num = ifelse(nchar(end_num) >=3, end_num + 600, end_num),
    full_path = str_c(dir, "rows_", start_num, "_to_", end_num, ".", chuck, sep = "")
  )










