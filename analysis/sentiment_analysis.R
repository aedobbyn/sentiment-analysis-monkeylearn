library(here)
library(tidyverse)
# devtools::install_github("ropensci/monkeylearn")
library(monkeylearn)
library(glue)

dat_raw <- 
  read_csv(here("data", "derived", "All_Opinion_Units_Sentiment_Topic.csv")) 

dat_unsplit <- 
  dat_raw %>% 
  rename(content = Text,
         sentiment = Sentiment,
         probability_sentiment = Probability,
         categories = `Categories (Full-text)`,
         probability_unit = `Probabilities (Full-text)`
         )

dat <- 
  dat_unsplit %>% 
  mutate(
    categories_nested = str_split(categories, ":"),
    probability_unit_nested = str_split(probability_unit, ":")
  ) %>% 
  unnest()



