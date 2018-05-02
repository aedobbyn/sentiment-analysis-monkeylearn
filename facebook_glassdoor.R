
facebook_glassdoor_url <- 
  "https://www.glassdoor.com/Reviews/Facebook-Reviews-E40772"

all_review_urls <- facebook_glassdoor_url %>% 
  str_c("_P", 1:10, ".htm")

all_review_urls[1] %>% 
  read_html() %>% 
  html_nodes(".pros") %>% 
  html_text()

all_review_urls[1] %>% 
  read_html() %>% 
  html_nodes(".pros") %>% 
  html_text()


all_review_urls[5] %>% 
  read_html() %>% 
  # html_nodes(".rating+ i .star") %>% 
  html_nodes(".cell , .star") %>% 
  html_text()



# Filled star hex: #0CAA41
# Empty star hex: #f1f2f2


