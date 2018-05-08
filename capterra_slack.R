library(tidyverse)
library(rvest)
library(dobtools)
library(glue)

slack_url <- "https://www.capterra.com/p/135003/Slack/"

# Make sure we can scrape
robotstxt::paths_allowed(
  domain = "capterra.com",
  path = "/",
  bot = "*"
)

strip_whitespace_newlines <- function(t) {
  out <- t %>% 
    str_replace_all("\\n", " ") %>% 
    trimws() 
  
  return(out)
}

# ------------

# Get overall rating 
slack_rating_overall <- slack_url %>% 
  read_html() %>% 
  html_nodes(".epsilon .rating-decimal") %>% 
  html_text() %>% 
  strip_whitespace_newlines() %>% 
  str_split(pattern = " ") %>%
  as_vector() %>% 
  `[`(1) %>%    # Grab the first element in the vector
  as.numeric()

# Get a vector of individual ratings
slack_all_ratings <- slack_url %>% 
  read_html() %>% 
  html_nodes(".overall-rating") %>% 
  html_text() %>% 
  strip_whitespace_newlines()

# Get a single rating
single_rating <- slack_url %>% 
  read_html() %>% 
  html_nodes("#review-4 .overall-rating") %>% 
  html_text() %>% 
  strip_whitespace_newlines()

# Get a single piece of content
single_content <- slack_url %>% 
  read_html() %>% 
  html_nodes(".cell-review:nth-child(24) > .grid") %>% 
  html_text() %>% 
  strip_whitespace_newlines()

# -------------


scrape_rating <- function(url, i) {
  out <- url %>% 
    read_html() %>% 
    html_nodes(glue("#review-{i} .overall-rating")) %>% 
    html_text() %>% 
    strip_whitespace_newlines()
  
  if (length(out) == 0) {
    out <- NA_character_
  }
  
  return(out)
}

try_scrape_rating <- possibly(scrape_rating, otherwise = NA_character_)

scrape_content <- function(url, i) {
  out <- url %>% 
    read_html() %>% 
    html_nodes(glue(".cell-review:nth-child({i}) .color-text")) %>%  # ".cell-review:nth-child({i}) > .grid"
    html_text() %>% 
    strip_whitespace_newlines()
  
  if (length(out) == 0) {
    out <- NA_character_
  }
  
  return(out)
}

try_scrape_content <- possibly(scrape_content, otherwise = NA_character_)

data_dir <- glue(here::here("data", "raw"), "/")


get_ratings_and_content <- function(url, review_range = 1:50,
                                    sleep = 1,
                                    write_out = TRUE, write_path = data_dir) {
  ifelse(sleep <= 1, 1, sleep)
  
  out <- tibble()
  page <- ifelse(str_detect(url, "page"), 
                 qdapRegex::ex_between(url, "page=", "&"),
                 NA_character_)
  
  for (i in review_range) {
    message(glue("Beginning scrape of page {page}, review {i}"))
    this_rating <- try_scrape_rating(url, i)

    this_cont <- try_scrape_content(url, i)

    this_review <- tibble(
      rating = this_rating,
      content = this_cont,
      page_num = page,
      review_num = i
    ) 
    
    if (write_out == TRUE) {
      write_rds(this_review, path = glue(data_dir, "page_{page}_rating_{i}.rds"))
    }
    
    out <- out %>% 
      bind_rows(this_review)
    
    Sys.sleep(runif(1, sleep - 0.5, sleep + 0.5))
  }
  
  out <- out %>% 
    rowwise() %>% 
    mutate(
      rating_perc = ifelse(is.na(rating), NA_character_, 
                           parse(text = rating) %>% eval()) %>% as.character()
    ) %>% 
    select(rating, rating_perc, content)
  
  return(out)
}


clean_content <- function(t) {
  out <- t %>% 
    t %>% 
    # str_replace_all("[,;:-]", "") %>%   # Remove punctuation except periods
    str_replace_all("[ ]{2,}", "")   # Remove more than 1 space
  
  return(out)
}


split_pro_cons <- function(t) {
  out <- t %>% 
    rowwise() %>% 
    mutate(
      content = content %>% clean_content(),
      pros = str_extract(content, "(?<=Pros:).*?(?=Cons:)")
    )
  
  out <- out %>% 
    rowwise() %>% 
    mutate(
      cons = ifelse(str_detect(content, "Overall:"), 
                    str_extract(content, "(?<=Cons:).*?(?=Overall:)"),
                    str_extract(content, "(?<=Cons:).*")),
      overall = ifelse(str_detect(content, "Overall:"),
                       str_extract(content, "(?<=Overall:).*"),
                       NA_character_)
    )
 
  return(out)
}


first_three_ratings <- 
  get_ratings_and_content(slack_url, 1:3)

first_tbl <- 
  first_three_ratings[1,] %>% 
  split_pro_cons()




many_ratings_raw <- 
  get_ratings_and_content(slack_url, 1:200) 
  
many_ratings <- 
  many_ratings_raw %>% 
  split_pro_cons()

many_ratings_clean <- many_ratings %>% 
  drop_na(rating)

# write_csv(many_ratings_clean,
#           here::here("data", "derived", "capterra_slack_reviews_first_99.csv"))







# First page URL https://www.capterra.com/gdm_reviews?page=1&product_id=135003

pages_want <- 1:45
slack_full_urls <- str_c("https://www.capterra.com/gdm_reviews?page=", pages_want, "&product_id=135003", sep = "")



## Check that it's working
# get_ratings_and_content(slack_full_urls[2], review_range = 1:2)
# foo <- read_rds(glue("{data_dir}page_2_rating_2.rds"))


get_multiple_pages <- function(urls, review_range = 1:99) {
  out <- tibble()
  for (u in urls) {
    this_page <- get_ratings_and_content(u, review_range = review_range)
    out <- out %>% 
      bind_rows(this_page)
  }
  return(out)
}

all_reviews <- get_multiple_pages(slack_full_urls[1:45])


