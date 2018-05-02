library(tidyverse)
library(rvest)
library(dobtools)
library(glue)

slack_url <- "https://www.capterra.com/p/135003/Slack/"

slack_rating_pre <- slack_url %>% 
  read_html() %>% 
  html_nodes(".epsilon .rating-decimal") %>% 
  html_text() %>% 
  str_replace_all("\\n", "") %>% 
  trimws() %>% 
  str_split(pattern = " ") %>%
  as_vector()

slack_rating_overall <- slack_rating_pre[1] %>% 
  as.numeric()



slack_all_ratings <- slack_url %>% 
  read_html() %>% 
  html_nodes(".overall-rating") %>% 
  html_text() %>% 
  str_replace_all("\\n", "") %>% 
  trimws() 


single_rating <- slack_url %>% 
  read_html() %>% 
  html_nodes("#review-4 .overall-rating") %>% 
  html_text() %>% 
  str_replace_all("\\n", "") %>% 
  trimws() 

single_content <- slack_url %>% 
  read_html() %>% 
  html_nodes(".cell-review:nth-child(20) .color-text") %>% 
  html_text() %>% 
  str_replace_all("\\n", "") %>% 
  trimws() 



get_ratings_and_content <- function(url, n_reviews) {
  out <- tibble()
  
  for (i in seq(n_reviews)) {
    this_rating <- url %>% 
      read_html() %>% 
      html_nodes(glue("#review-{i} .overall-rating")) %>% 
      html_text() %>% 
      str_replace_all("\\n", "") %>% 
      trimws() 

    this_cont <- url %>% 
      read_html() %>% 
      html_nodes(glue(".cell-review:nth-child({i}) .color-text")) %>% 
      html_text() %>% 
      str_replace_all("\\n", "") %>% 
      trimws() 

    this_review <- tibble(
      rating = this_rating,
      content = this_cont
    )
    
    out <- out %>% 
      bind_rows(this_review)
  }
  return(out)
}

first_three_ratings <- 
  get_ratings_and_content(slack_url, 3)



clean_content <- function(t) {
  out <- t %>% 
    t %>% 
    str_replace_all("[,;:-]", "") %>%   # Remove punctuation except periods
    str_replace_all("[ ]{2,}", "")   # Remove more than 1 space
  
  return(out)
}


split_pro_cons <- function(t) {
  out <- t %>% 
    mutate(
      content = content %>% clean_content(),
      pros = str_extract(content, "(?<=Pros).*?(?=Cons)"),
      cons = str_extract(content, "(?<=Cons).*")
    )
  return(out)
}


first_three_ratings[1,] %>% split_pro_cons()


