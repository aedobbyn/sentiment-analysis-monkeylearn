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

slack_rating <- slack_rating_pre[1] %>% 
  as.numeric()