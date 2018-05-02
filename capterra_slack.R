library(tidyverse)
library(rvest)
library(dobtools)

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
  html_nodes(".cell-review:nth-child(6) .palm-one-whole") %>% 
  html_text() %>% 
  str_replace_all("\\n", "") %>% 
  trimws() 


