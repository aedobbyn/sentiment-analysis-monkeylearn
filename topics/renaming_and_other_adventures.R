# Keeping this as a record of things that didn't go right the first time
# Commenting out things that if run will actually eat up queries

# # # Error handling # # #
# topics_raw <- 
#   monkey_classify(reviews_with_subratings_nested, col = content,
#                   classifier_id = classifier_id, unnest = FALSE) 

# Processing batch 1 of 13 batches: texts 1 to 200
# Processing batch 2 of 13 batches: texts 200 to 400
# Processing batch 3 of 13 batches: texts 400 to 600
# Processing batch 4 of 13 batches: texts 600 to 800
# Processing batch 5 of 13 batches: texts 800 to 1000
# No results for this call; returning NA.
# Error in cbind_all(x) : Argument 2 must be length 2542, not 200


# # Then another error between texts 600 and 800, so did in two batches
# topics_first_600 <-
#   reviews_with_subratings_nested[1:600, ] %>%
#   write_batches(n_texts_per_batch = 200)
# 
# topics_600_onward <-
#   reviews_with_subratings_nested[600:nrow(reviews_with_subratings_nested), ] %>%
#   write_batches(n_texts_per_batch = 200)
# # Then bind the rows


# ---- Bit o renaming -----
# Due to error between 600 and 800, restarted from 800 but naming scheme started filenames back at 1

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
    start_num = ifelse(nchar(start_num) >=3 | start_num == 601, start_num + 600, start_num),
    end_num = ifelse(nchar(end_num) >=3, end_num + 600, end_num),
    full_path = str_c(dir, "rows_", start_num, "_to_", end_num, ".", chuck, sep = "")
  )

# # # Actually do the renaming # # #
# for (i in seq_along(fls)) {
#   file.rename(fls[i], new_file_names$full_path[i])
# }
# # # # # # # # # # # # # # # # # # 

