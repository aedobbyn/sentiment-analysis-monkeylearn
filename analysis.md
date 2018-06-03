---
title: "MonkeyLearn Sentiment Analysis: Analysis"
output:
  html_document:
    keep_md: true
    toc: false
    theme: yeti
  github_document:
    toc: false
  pdf_document:
    keep_tex: true
    toc: false
---






```r
library(here)
library(tidyverse)
library(rvest)
library(monkeylearn)
library(glue)
library(knitr)
library(dobtools)
library(tidytext)
library(kableExtra)
```



```r
pal <- wesanderson::wes_palette("Rushmore1")

round_dec <- function(x, n_dec = 3) {
  if (is.numeric(x)) {
    x <- x %>% round(digits = 2)
  } 
  x
}

add_kable <- function(df, round_decimals = TRUE, 
                      replace_na = FALSE, capitalize = TRUE, ...) {
  
  if (round_decimals == TRUE) {
    df <- df %>% 
      map_dfc(round_dec)
  }
  
  if (replace_na == TRUE) {
    df <- 
      df %>% dobtools::replace_na_df()
  }
  
  if (capitalize == TRUE) {
    df <- 
      df %>% dobtools::cap_df()
  }
  
  df %>% 
    kable() %>% 
    kable_styling(full_width = F)
}
```





## Analysis

We've got data! 🎉 Let's take a look at it. 


```r
dat %>%
  slice(1:20)
```

```
## # A tibble: 20 x 11
##    rating content           review_num page_num opinion_unit     sentiment
##    <chr>  <chr>                  <int>    <int> <chr>            <chr>    
##  1 5/5    Pros:  Ability t…          1        1 Ability to crea… Positive 
##  2 5/5    Pros:  Ability t…          1        1 It helps improv… Positive 
##  3 5/5    Pros:  Ability t…          1        1 It helps improv… Positive 
##  4 5/5    Pros:  Ability t…          1        1 Since it is int… Positive 
##  5 5/5    Pros:  Ability t…          1        1 Team collaborat… Positive 
##  6 5/5    Pros:  I work ac…          1        2 I work across s… Positive 
##  7 5/5    Pros:  I work ac…          1        2 I enjoy using S… Positive 
##  8 5/5    Pros:  I work ac…          1        2 The only issue … Negative 
##  9 5/5    Pros:  I work ac…          1        2 But for more st… Negative 
## 10 5/5    Pros:  I work ac…          1        2 No problem with… Neutral  
## 11 5/5    Pros:  I work ac…          1        2 though.          Negative 
## 12 5/5    Pros:  I work ac…          1        2 Flawless commun… Positive 
## 13 4/5    Pros:  The thing…          1        3 The thing I lik… Positive 
## 14 4/5    Pros:  The thing…          1        3 I wish there wa… Negative 
## 15 4/5    Pros:  The thing…          1        3 It seems as if … Negative 
## 16 5/5    Pros:  Slack is …          1        4 Slack is a grea… Positive 
## 17 5/5    Pros:  Slack is …          1        4 Slack allow us … Positive 
## 18 5/5    Pros:  Slack is …          1        4 I don't find to… Negative 
## 19 5/5    Pros:  Slack is …          1        4 I don't find to… Negative 
## 20 5/5    Pros:  Slack is …          1        4 The price might… Negative 
## # ... with 5 more variables: probability_sentiment <dbl>,
## #   rating_perc <dbl>, sub_ratings_split <list>, category <chr>,
## #   probability_unit <chr>
```

Since there are multiple rows per review, we'll want a unique identifier for each review. Each `page_num, review_num` combination represents a unique review. We could hash these two values but since we have the benefit of knowing that the reviews happen in chronological order, it seems better to number them, starting with 1 for our oldest review.

For good measure I also created a `doc_identifier` by smushing together the page and review number.


```r
dat <- dat %>%
  mutate(
    doc_identifier = str_c("r", review_num, "p", page_num, sep = "_")
  )

uuids <- dat %>%
  arrange(page_num, review_num) %>%
  nest(-doc_identifier) %>%
  mutate(doc_uuid = nrow(.) - row_number() + 1) %>%
  select(-data)

dat <- dat %>%
  left_join(uuids)
```


There are only three possible sentiments for an opinion unit to have,


```r
dat$sentiment %>% factor() %>% levels()
```

```
## [1] "Negative" "Neutral"  "Positive"
```

so we can assign a number to each type of sentiment in order to be able to represent them on an ordinal scale. 



```r
dat <- dat %>%
  rowwise() %>%
  mutate(
    sentiment_num = switch(sentiment,
      "Negative" = -1,
      "Neutral" = 0,
      "Positive" = 1
    )
  ) %>%
  ungroup()
```





What about categories?


```r
dat$category %>% factor() %>% levels()
```

```
##  [1] "Attachments-Sharing"             "Calls"                          
##  [3] "Channels"                        "Customer Support"               
##  [5] "Desktop"                         "Ease of Use"                    
##  [7] "Emojis"                          "Feedback-Recommendation"        
##  [9] "General"                         "Groups"                         
## [11] "Integrations"                    "Messages"                       
## [13] "Mobile"                          "None"                           
## [15] "Notifications"                   "Other"                          
## [17] "Performance-Quality-Reliability" "Pricing"                        
## [19] "Purpose"                         "Search"                         
## [21] "UI-UX"                           "Web"
```

We can see there are some opinion units labeled with the category "None". It's tough to know how to interpret these, so we can filter out these rows in a new `dat_clean` dataframe. We'll also filter out low-probability sentiments and categories -- anything that the classifier is less than 55% sure is classified correctly.


```r
probability_cutoff <- 0.55

dat_clean <-
  dat %>%
  filter(!is.na(probability_unit) & !is.na(probability_unit) &
    category != "None" &
    probability_sentiment > probability_cutoff & probability_unit > probability_cutoff)
```

After cleaning, we've got 2314 unique opinion units to work with, each with a single sentiment and multiple classifications.


**Initial exploring**

Now let's get the lay of the land by seeing what the breakdown of sentiments is overall.


```r
sentiment_breakdown <-
  dat_clean %>%
  group_by(sentiment) %>%
  count() %>%
  rename(by_sentiment = n) %>%
  ungroup() %>%
  mutate(
    total = sum(by_sentiment),
    sentiment_prop = by_sentiment / total
  )
```


![](analysis_files/figure-html/sentiment_breakdown_graph-1.png)<!-- -->


We can see there are very few reviews that have a Neutral sentiment, which is useful for us. It's easier to draw conclusions about the strengths and weaknesses of a product when most of the feedback is either definitively positive or negative. (That could also be a reflection of the tendency of reviewers to feel more strongly about the product they're reviewing than the general user base. But whether or not these reviews are an unbiased reflection of most users' true feelings about the product is neither here nor there 😆.)


**Overall ratings**

We might ask how users' overall ratings of the product line up with sentiments assigned to each opinion unit by MonkeyLearn. 

It's important to remember that there is a one:many relationship between ratings and opinion units here; each review gets a at most single rating, but reviews are later parceled into multiple opinions.


```r
ratings_by_sentiment <-
  dat_clean %>%
  distinct(doc_uuid, .keep_all = TRUE) %>% 
  group_by(sentiment) %>%
  summarise(
    mean_rating = mean(rating_perc, na.rm = TRUE)
  )

ratings_by_sentiment %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Sentiment </th>
   <th style="text-align:right;"> Mean Rating </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Positive </td>
   <td style="text-align:right;"> 0.94 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Neutral </td>
   <td style="text-align:right;"> 0.96 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Negative </td>
   <td style="text-align:right;"> 0.93 </td>
  </tr>
</tbody>
</table>

There is very little difference in overall ratings of the product across opinion unit sentiments. This indicates that despite critiques (which people are encouraged to think of and express in the Cons section), most overall reviews remain positive.


**Sentiment and Category**

What is the interaction between the two main things of interest here, category and sentiment? Let's get a summary of the mean sentiment (based off of our numerical representation of sentiment) for opinion units that have been classified into each category.



```r
sentiment_by_category <-
  dat_clean %>%
  group_by(category) %>%
  summarise(
    mean_sentiment = mean(sentiment_num)
  ) %>%
  arrange(desc(mean_sentiment))
```

Performance-Quality-Reliability gets the lowest average sentiment, whereas General gets the highest.

Next I want to split these mean sentiment ratings into three equal parts and assign those parts valences that describe the mean sentiment for that category. We'll find the tertiles (a word I thought I made up but turns out it's a thing) of the mean sentiments so we can divid them three groups as they relate to each other.


```r
tertiles <- c(
    sentiment_by_category$mean_sentiment %>% quantile(1/3),
    sentiment_by_category$mean_sentiment %>% quantile(2/3)
    )

sentiment_by_category_summary <-
  tibble(name = names(tertiles), 
         value = tertiles) 

sentiment_by_category_summary %>% 
  add_kable() 
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Name </th>
   <th style="text-align:right;"> Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 33.33333% </td>
   <td style="text-align:right;"> 0.25 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66.66667% </td>
   <td style="text-align:right;"> 0.78 </td>
  </tr>
</tbody>
</table>


We'll use these tertiles as the bounds for assigning valences. 



```r
bad_sentiment_upper_bound <- 
  sentiment_by_category_summary %>% 
  filter(name == sentiment_by_category_summary$name[1]) %>% pull(value)
good_sentiment_lower_bound <- 
  sentiment_by_category_summary %>% 
  filter(name == sentiment_by_category_summary$name[2]) %>% pull(value)

sentiment_by_category <-
  sentiment_by_category %>% 
  mutate(
    sentiment_valence = case_when(
      mean_sentiment < bad_sentiment_upper_bound ~ "Bad",
      mean_sentiment >= bad_sentiment_upper_bound & 
        mean_sentiment <= good_sentiment_lower_bound ~ "Meh",
      mean_sentiment > good_sentiment_lower_bound ~ "Good"
    ) %>% factor()
  )

sentiment_by_category %>%
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Category </th>
   <th style="text-align:right;"> Mean Sentiment </th>
   <th style="text-align:left;"> Sentiment Valence </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> General </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Purpose </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emojis </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Groups </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Integrations </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Feedback-Recommendation </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Attachments-Sharing </td>
   <td style="text-align:right;"> 0.82 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UI-UX </td>
   <td style="text-align:right;"> 0.76 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Channels </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mobile </td>
   <td style="text-align:right;"> 0.73 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Web </td>
   <td style="text-align:right;"> 0.56 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Desktop </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Messages </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Calls </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Search </td>
   <td style="text-align:right;"> -0.32 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Notifications </td>
   <td style="text-align:right;"> -0.49 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pricing </td>
   <td style="text-align:right;"> -0.56 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Performance-Quality-Reliability </td>
   <td style="text-align:right;"> -0.83 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
</tbody>
</table>

```r
sentiment_valence_order <- c("Good", "Meh", "Bad")

sentiment_by_category$sentiment_valence <-
  sentiment_by_category$sentiment_valence %>% fct_relevel(sentiment_valence_order)
```


NB that "Meh" != "Neutral". These category valences are only meaningful relative to one another because categories tend to be rated positively on the whole (0.4210697) on average. That means that even the lower bound of "Meh", 0.2512253 exceeds Neutral, or 0.

Now we can colo(u)r the bars of our plot with those valences. This will be useful when we shake up the order of the categories as we arrange them by different variables while retaining the measure of sentiment per category.

![](analysis_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

This plot is useful, but it doesn't tell us anything about how *often* people complain about the aspects of the product that tend to get low sentiment ratings. We might ask, are the categories that often have a negative sentiment categories that people tend to mention often in their reviews, or are they less frequent?

Let's plot the frequency with which opinion units are categorized into different topics.


```r
category_freq <-
  dat_clean %>%
  group_by(category) %>%
  count(sort = TRUE) %>%
  rename(
    n_opinion_units = n
  ) %>% 
  left_join(sentiment_by_category) 
```

![](analysis_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

There doesn't seem to be a clear relationship between sentiment and number of mentions. That suggests that people aren't systematically disparraging the low-sentiment categories more than they are praising the high-sentiment categories or vice versa.


Now we can weight the category sentiment by the number of times it occurs in an opinion unit. This can give us a better idea of the sentiment in the context of how often it's mentioned. This is important because if a category has very low sentiment but its almost never mentioned, it may be less critical to focus on improving than an only mildly badly rated category with a lot of mentions.


```r
sentiment_by_category_weighted <-
  category_freq %>%
  mutate(
    weighted_sentiment = mean_sentiment * n_opinion_units
  ) %>%
  arrange(desc(weighted_sentiment))

sentiment_by_category_weighted %>%
  head() %>%
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Category </th>
   <th style="text-align:right;"> N Opinion Units </th>
   <th style="text-align:right;"> Mean Sentiment </th>
   <th style="text-align:left;"> Sentiment Valence </th>
   <th style="text-align:right;"> Weighted Sentiment </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:right;"> 1607 </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:left;"> Meh </td>
   <td style="text-align:right;"> 1141 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> General </td>
   <td style="text-align:right;"> 1158 </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:left;"> Good </td>
   <td style="text-align:right;"> 1133 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Integrations </td>
   <td style="text-align:right;"> 562 </td>
   <td style="text-align:right;"> 0.85 </td>
   <td style="text-align:left;"> Good </td>
   <td style="text-align:right;"> 477 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Purpose </td>
   <td style="text-align:right;"> 491 </td>
   <td style="text-align:right;"> 0.92 </td>
   <td style="text-align:left;"> Good </td>
   <td style="text-align:right;"> 454 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Channels </td>
   <td style="text-align:right;"> 598 </td>
   <td style="text-align:right;"> 0.75 </td>
   <td style="text-align:left;"> Meh </td>
   <td style="text-align:right;"> 446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Groups </td>
   <td style="text-align:right;"> 323 </td>
   <td style="text-align:right;"> 0.88 </td>
   <td style="text-align:left;"> Good </td>
   <td style="text-align:right;"> 283 </td>
  </tr>
</tbody>
</table>


![](analysis_files/figure-html/sentiment_by_category_weighted_graph-1.png)<!-- -->

Even when weighting sentiment by frequency, it seems that Slack is generally doing well overall. Medium or high sentiment categories dominate the reviews in general.


**Sub-Ratings**

We can dig into the explicit ratings of different aspects of the platform and compare them to categories assigned by MonkeyLearn. If you'll recall, sub-ratings are these things:

![](./img/sub_ratings.jpg)

First we have to unnest our sub-ratings which until now we've calmly shunted along in the list column we created from the blob of text we got them in..



We'll want to parse these "4/5", "5/5", etc. strings into numbers we can work with, in the same way we did the overall ratings.


```r
parsed_subratings <-
  reviews_with_subratings_unnested %>%
  rowwise() %>%
  mutate(
    subrating_num =
      ifelse(is.na(sub_rating_rating), NA,
        parse(text = sub_rating_rating) %>% eval()
      )
  )

parsed_subratings %>% 
  select(sub_rating_category, sub_rating_rating, subrating_num) %>% 
  head() %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Sub Rating Category </th>
   <th style="text-align:left;"> Sub Rating Rating </th>
   <th style="text-align:right;"> Subrating Num </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:left;"> 5/5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:left;"> 5/5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Features &amp; Functionality </td>
   <td style="text-align:left;"> 5/5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Value for Money </td>
   <td style="text-align:left;"> 5/5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:left;"> 5/5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:left;"> 5/5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

What are the overall mean ratings of each aspect of the platform?


```r
parsed_subratings_summary <-
  parsed_subratings %>%
  drop_na(subrating_num, sub_rating_category) %>%
  group_by(sub_rating_category) %>%
  summarise(
    mean_subrating = mean(subrating_num)
  )

parsed_subratings_summary %>%
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Sub Rating Category </th>
   <th style="text-align:right;"> Mean Subrating </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:right;"> 0.89 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:right;"> 0.93 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Features &amp; Functionality </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Value for Money </td>
   <td style="text-align:right;"> 0.91 </td>
  </tr>
</tbody>
</table>



How do these sub-ratings match up with category ratings we calculated earlier? Some of the subrating names match perfectly with MonkeyLearn categories like "Customer Support" and "Ease of Use", but the other two we'll need to assign an alias to be able to join it up with the mean MonkeyLearn sentiment for that category and compare the two.


```r
parsed_subratings_summary$alias <- c("Customer Support", "Ease of Use", "General", "Pricing")

parsed_subratings_summary %>%
  left_join(sentiment_by_category,
    by = c("alias" = "category")
  ) %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Sub Rating Category </th>
   <th style="text-align:right;"> Mean Subrating </th>
   <th style="text-align:left;"> Alias </th>
   <th style="text-align:right;"> Mean Sentiment </th>
   <th style="text-align:left;"> Sentiment Valence </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:right;"> 0.89 </td>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:right;"> 0.67 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:right;"> 0.93 </td>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:right;"> 0.71 </td>
   <td style="text-align:left;"> Meh </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Features &amp; Functionality </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:left;"> General </td>
   <td style="text-align:right;"> 0.98 </td>
   <td style="text-align:left;"> Good </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Value for Money </td>
   <td style="text-align:right;"> 0.91 </td>
   <td style="text-align:left;"> Pricing </td>
   <td style="text-align:right;"> -0.56 </td>
   <td style="text-align:left;"> Bad </td>
  </tr>
</tbody>
</table>


What's interesting here is that people on average rate each of the four aspects very high. However, when they mention them in reviews the sentiment they attach to them can be much lower. If we take Pricing / Value for Money, for example (which may not actually be analagous concepts but let's roll with it for a minute), the sentiment attached to explicit mentions of the price of the service tend to be negative, though its Value for Money is rated well. I can see two explanations for this. The uninteresting interpretation is that most people use the free version of Slack and so they're getting something for nothing, which is a pretty good value for your money. A slightly more interesting interpretation would be that the "silent majority" on the aspect of pricing actually thinks they're getting a pretty good deal but a vocal minority disagree and that minority are the only ones voicing their dissatisfaction with the pricing model.

In any case, you could see this as evidence that it's important to take both the explicit numbers as well as sentiments into account when considering a certain aspect of a product, and as always, the base rates of users' contributions to both.


<br>


### Down to the word level

Now that we have classifications for each opinion units, we can see how the individual words in opinion units map to the sentiment and category classification they were assigned, and maybe gain some more granular insight about what people like and dislike about the product.

The [`tidytext`](https://github.com/juliasilge/tidytext) package is fantastic for this purpose. We'll use its `unnest_tokens` function to get a long dataframe of all words and then clean them up a bit by filtering out `stop_words` (a dataset included in the package) like "and" and "the". 

I add a few of our own stopwords that come up so often they're also essentially meaningless for most of our purposes.


```r
stop_words <- 
  stop_words %>% 
  bind_rows(
    tibble(
      word = c("slack", "pros", "cons", "overall", "comments"),
      lexicon = rep("ours", length(word))
    )
  )
```


A helper, [`dobtools::find_nums()`](https://github.com/aedobbyn/dobtools/blob/master/R/find_nums.R), mutates on a couple columns: one for whether the word in question is a number (i.e. can be converted to type numeric) and one for whether the word contains a number. If `is_num` is TRUE, then `contains_num` is also always TRUE.



```r
dat_tokenized <-
  dat_clean %>%
  nest(-content, -doc_uuid) %>%
  unnest_tokens(word, content) %>%
  anti_join(stop_words, "word") %>%
  dobtools::find_nums() %>%
  filter(contains_num == FALSE)
```

The `tidytext` package also includes a `sentiments` dataset which we can join on our words to get a classification of the words' sentiment in three different lexicons as well as its score on a scale of -5 (negative) to 5 (positive). (`?tidytext::sentiments` for a full explanation of the dataset.) For example:


```r
sentiments %>% filter(word == "yucky")
```

```
## # A tibble: 1 x 4
##   word  sentiment lexicon score
##   <chr> <chr>     <chr>   <int>
## 1 yucky <NA>      AFINN      -2
```

and 


```r
sentiments %>% filter(word == "yummy")
```

```
## # A tibble: 1 x 4
##   word  sentiment lexicon score
##   <chr> <chr>     <chr>   <int>
## 1 yummy <NA>      AFINN       3
```

(Don't ask me why yummy is more positive than yucky is negative 😆.) Anyway, let's join this on our data set by "word".



```r
dat_tokenized <-
  dat_tokenized %>%
  select(-is_num, -contains_num) %>%
  left_join(tidytext::sentiments %>%
    rename(
      word_sentiment = sentiment,
      score_sentiment = score
    ),
  by = "word"
  )
```



If we're interested in doing an analysis of words that belong to opinion units that were tagged with certain categories or sentiments, we're going to need back our measure of each of those, which were labeled at the opinion unit level. Luckily we can unnest our `dat_tokenized_tfidf` dataframe in place of doing any joining.


```r
dat_tokenized_unnested <-
  dat_tokenized %>%
  unnest()
```



Do the words that make up an opinion unit have a significant effect on the sentiment it's assigned by MonkeyLearn?


```r
lm(
  data = dat_tokenized_unnested %>% drop_na(sentiment_num, score_sentiment),
  sentiment_num ~ score_sentiment
) %>%
  summary() %>%
  tidy() %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Std Error </th>
   <th style="text-align:right;"> Statistic </th>
   <th style="text-align:right;"> P Value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 0.54 </td>
   <td style="text-align:right;"> 0.01 </td>
   <td style="text-align:right;"> 97.32 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> score_sentiment </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.00 </td>
   <td style="text-align:right;"> 10.63 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

Yes -- words with more positive sentiments tend to occur in more positive opinion units.


As a control, we can check whether words that appear at the beginning of the alphabet tend to get higher sentiment scores. (I don't know of a reason to suspect this might be the case which is why I'm treating it as a control but maybe there is a psycholinguist out there who can set me straight?)

We'll extract just the first letter of the word and assign it the number of the alphabet from 1 to 24.


```r
assign_number <- function(l) {
  if (length(l) == 0 || !l %in% letters) {
    n <- NA_integer_
  } else {
    n <- which(letters == l)
  }
  n
}

try_assign_number <- possibly(assign_number, otherwise = NA_integer_)
```



```r
dat_tokenized_first_letter <-
  dat_tokenized %>%
  rowwise() %>%
  mutate(
    first_letter = substr(word, 1, 1),
    first_letter_num = try_assign_number(first_letter)
  )
```

And then plot the word's sentiment as scored on the `AFINN` scale. The dashed horizontal line represents the mean sentiment score for words in our data set.

![](analysis_files/figure-html/unnamed-chunk-26-1.png)<!-- -->



```r
first_letter_lm <- 
  lm(
    data = dat_tokenized_first_letter %>% drop_na(first_letter_num, score_sentiment),
    first_letter_num ~ score_sentiment
  ) %>%
  summary() %>%
  tidy() %>% 
  map(round_dec)
```

(In case you're curious, the statistical relationship isn't significant either, as we'd expect, *b = 0.04, p = 0.34*.)


<br>

### Searching for certain phrases



We might be interested in phrases that follow specific words like "use" or "don't use". Here we ask for everything after `word` and up until the first period.

We might also want to pull out a category or categories if they exist in the phrase. To that end we'll make a regex for all of the categories MonkeyLearn has identified (except Other which seems uninteresting):
 

```r
category_reg <-
  dat_clean$category[-which(dat_clean$category == "Other")] %>%
  tolower() %>%
  unique() %>%
  str_c(collapse = "|")
```


We can make something reusable like:


```r
search_for <- function(df = dat_clean, col = content, word = "love", append_the = FALSE,
                       keep_col = FALSE) {
  word_capped <-
    dobtools::simple_cap(word)

  q_col <- enquo(col)

  look_for <- ifelse(append_the == TRUE,
    glue("{word} the |{word_capped} the "),
    glue("{word} |{word_capped} ")
  )

  out <-
    df %>%
    filter(
      str_detect(!!q_col, look_for)
    ) %>%
    distinct(!!q_col) %>%
    rowwise() %>%
    mutate(
      phrase = str_extract(!!q_col, glue("(?<={look_for}).+$")) %>%
        str_replace_all("(?<=\\.).*", ""),
      phrase_categories = str_extract_all(phrase, category_reg) %>%
        replace_y() %>%
        as_vector() %>%
        unique() %>%
        str_c(collapse = ", ")
    )
  
  if (keep_col == FALSE) {
    out <- out %>% 
      select(-!!q_col)
  }

  return(out)
}
```


We can ask for our word always followed by a "the" so that we know our `phrase` will start with a noun that our `word` is referring to. By default we won't keep the original opinion unit (when `keep_col = FALSE`) to save space but I'll put it in the first one so we can see how `search_for` works.

**love**


```r
search_for(word = "love", append_the = TRUE, keep_col = TRUE) %>%
  dobtools::replace_na_df() %>%
  sample_n(5) %>%
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Content </th>
   <th style="text-align:left;"> Phrase </th>
   <th style="text-align:left;"> Phrase Categories </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Pros:  Simplifies communication between departments of the company in a safe and discreet way. Important for the confidentiality and the coordinated work of the team.  Cons:  The software is heavy and takes up a lot of space, some workers can not install it on their smartphones and it reheats some older computers. It is not an indispensable application at the office because there are other ways to communicate between the teams  Overall:  The ease of use and the tools to make better use of the application, such as bots. I definitely love the birthday bot! </td>
   <td style="text-align:left;"> birthday bot! </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pros:  Slack is so easy to use and so much more fun than other similar products. With the option to integrate apps like Giphy, Slack makes communication with your teams fun. I also love the channels feature - if you want to open a discussion outside of a direct message or direct group message, create a channel with a purpose and allow people to grow the conversation in a thread dedicated to that topic.  Cons:  The free version only stores your most recent 10,000 messages and files, which can be a problem if you need to keep track of those conversations. The paid version eliminates that problem though, so it really isn't that big of a deal.  Overall:  It's fun, it's easy, it helps me stay on task and in the loop. </td>
   <td style="text-align:left;"> channels feature - if you want to open a discussion outside of a direct message or direct group message, create a channel with a purpose and allow people to grow the conversation in a thread dedicated to that topic. </td>
   <td style="text-align:left;"> channels, purpose </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pros:  I love the ease of being able to chat with my coworkers, as well as receive announcements from the entire company. I'm able to get updates, ask questions, and chat about the day with my immediate team and the rest of the company. I also appreciate the add ons, such as lunch roulette or polls, that allow me to better collaborate and get to know my coworkers.  It's also easy to switch between groups, so I can jump from my company's workspace to my class' in a few seconds.   Cons:  I use the free version with my grad school classmates and it takes forever to show that a message has been read and remove the notification. It drives me crazy so I've taken that account off my desktop.   Overall:  increased communication and fun </td>
   <td style="text-align:left;"> ease of being able to chat with my coworkers, as well as receive announcements from the entire company. </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pros: Slack has a simple interface that allows easy communication across large groups.  I initially used it for personal, and has since been incorporated into my current employer.  Love the ability to @someone and get a response without feeling intrusive.              Cons: None that I can think of.  There is a mobile, and desktop interface to ensure you are always up on the latest conversations/communcations. </td>
   <td style="text-align:left;"> ability to @someone and get a response without feeling intrusive. </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pros:  Easy to use. Reliable.   Overall:  Slack is great for communicating both interoffice and with remote office staff. It's easy to use and customize. The channels make it easy to message between teams. The features for reminders are great. Love the emojis ;) </td>
   <td style="text-align:left;"> emojis ;) </td>
   <td style="text-align:left;"> emojis </td>
  </tr>
</tbody>
</table>

By default we won't append "the" after `word`. We can filter to just opinion units that contain our word and then the name of one of our categories following it.

**use**


```r
search_for(word = "use") %>%
  drop_na(phrase_categories) %>%
  dobtools::replace_na_df() %>%
  sample_n(5) %>%
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Phrase </th>
   <th style="text-align:left;"> Phrase Categories </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> the app, the notifications don��t work good. </td>
   <td style="text-align:left;"> notifications </td>
  </tr>
  <tr>
   <td style="text-align:left;"> makes it easy for everyone in the office to pick up and use Slack! It saves your convos, files, and more!  We love creating group chats for big projects and always knowing where the status is!   Cons:  We did have a few issues with notifications and downloading the windows app. </td>
   <td style="text-align:left;"> notifications </td>
  </tr>
  <tr>
   <td style="text-align:left;"> it on your phone or desktop Easy to create a team and channel You can share documents, use emojis, and gifs              Cons: Losing files after a while because of storage There could be more of a variety of gifs Slowing down my computer when my teams are huge </td>
   <td style="text-align:left;"> desktop, emojis </td>
  </tr>
  <tr>
   <td style="text-align:left;"> both the desktop and mobile app to send messages to my colleagues. </td>
   <td style="text-align:left;"> desktop, mobile, messages </td>
  </tr>
  <tr>
   <td style="text-align:left;"> emojis as comments, tag certain people within the comments, and make it fun to have a conversation within the app. </td>
   <td style="text-align:left;"> emojis </td>
  </tr>
</tbody>
</table>

**want**


```r
search_for(word = "want") %>%
  drop_na(phrase_categories) %>%
  dobtools::replace_na_df() %>%
  sample_n(5) %>%
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Phrase </th>
   <th style="text-align:left;"> Phrase Categories </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> and people can interact between them using direct messages             Cons: Communication can get messy if all the people doesn't use it correctly. </td>
   <td style="text-align:left;"> messages </td>
  </tr>
  <tr>
   <td style="text-align:left;"> to be envolved and other times where I don't need to be and I just would like to temporarily quite the notifications. </td>
   <td style="text-align:left;"> notifications </td>
  </tr>
  <tr>
   <td style="text-align:left;"> closed groups that others cannot see. </td>
   <td style="text-align:left;"> groups </td>
  </tr>
  <tr>
   <td style="text-align:left;"> to quickly share a file, image, or web link? Slack them. </td>
   <td style="text-align:left;"> web </td>
  </tr>
  <tr>
   <td style="text-align:left;"> to see a lot of history and have everything searchable. </td>
   <td style="text-align:left;"> search </td>
  </tr>
</tbody>
</table>



#### Going Negative

Let's focus on the places where Slack might want to improve. I'll focus on the baddest of the "Bad" categories we found: Performance-Quality-Reliability . Simple word counts can shed some insight into what things people mention the most when they're talking about ths topic in an opinion unit with negative sentiment.

Let's filter our opinion units to just the negative ones that MonkeyLearn classified as being about Performance-Quality-Reliability.


```r
pqr_negative <-
  dat_clean %>%
  filter(category == "Performance-Quality-Reliability" &
    sentiment == "Negative")
```

Next let's count up the number of times each unique word appears and take a look at the top few.


```r
pqr_complaints <-
  pqr_negative %>%
  unnest_tokens(word, content) %>%
  filter(category == "Performance-Quality-Reliability") %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

pqr_complaints %>% 
  head() %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Word </th>
   <th style="text-align:right;"> N </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> easy </td>
   <td style="text-align:right;"> 189 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> team </td>
   <td style="text-align:right;"> 169 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> communication </td>
   <td style="text-align:right;"> 160 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> app </td>
   <td style="text-align:right;"> 152 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> time </td>
   <td style="text-align:right;"> 124 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> version </td>
   <td style="text-align:right;"> 122 </td>
  </tr>
</tbody>
</table>

A reasonable question to ask might be: is the desktop app or mobile app mentioned more in P-Q-R complaints?
 

```r
pqr_complaints %>%
  filter(word %in% c("desktop", "mobile")) %>% 
  arrange(desc(n)) %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Word </th>
   <th style="text-align:right;"> N </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> mobile </td>
   <td style="text-align:right;"> 59 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> desktop </td>
   <td style="text-align:right;"> 43 </td>
  </tr>
</tbody>
</table>

Okay, so it seems like mobile is more of a problem.

How does that compare to the *base rate* of the mentions of desktop and mobile across all opinion units?


```r
dat_tokenized %>%
  filter(word %in% c("desktop", "mobile")) %>%
  group_by(word) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Word </th>
   <th style="text-align:right;"> N </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> mobile </td>
   <td style="text-align:right;"> 345 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> desktop </td>
   <td style="text-align:right;"> 231 </td>
  </tr>
</tbody>
</table>

So even though mobile makes more appearances in negative reviews than the desktop app, it also makes more appearances overall suggesting that maybe its P-Q-R is not more of a problem than desktop after all.

I'd be interested to know here whether people spend more active time on Slack on mobile or desktop. Do they mention mobile more becuase they use it more or simply because the experience is more noteworthy than the desktop experience?

<br>


#### TF-IDF

[Term frequency inverse document frequency](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) is a measure of how often a word appears in a given document (here, a review) as opposed to overall, in all of the documents. That can give us a sense of how important it is to a given document as compared to a baseline of all words used in the entire corpus.

What we can learn from TF-IDF is, for instance, what words do people often use in reviews when they're talking about a specific aspect of the product that they tend to use less frequently when talking about other aspects of the product?
 
 To get every word's TF-IDF, we need to get within-document counts of each word. We'll also count how often the word is used in all reviews.

<!-- ```{r} -->
<!-- dat_tokenized_counts <-  -->
<!--  dat_tokenized %>%  -->
<!--  add_count(word) %>%  -->
<!--  rename( -->
<!--    n_words_total = n -->
<!--  ) %>%  -->
<!--  group_by(doc_uuid) %>%  -->
<!--  add_count(word) %>%  -->
<!--  rename( -->
<!--    n_words_this_doc = n -->
<!--  )  %>%  -->
<!--  ungroup()  -->
<!-- ``` -->


<!-- Now we can ask `tidytext` to do a mutate and attach the `tf_idf` of each word to our dataframe. -->

<!-- ```{r} -->
<!-- dat_tokenized_tfidf <-  -->
<!--  dat_tokenized_counts %>%  -->
<!--  bind_tf_idf(word, doc_uuid, n_words_this_doc) -->
<!-- ``` -->

A question we might be interested in is: which words are most distinctive to opinion units tagged with each category? For this purpose we can treat each category as its own document (rather than each review as its own document).

We count up the number of words used in each category and then ask `tidytext` to do a mutate and attach the `tf_idf` of each word to our dataframe.


```r
category_tfidf <-
  dat_tokenized_unnested %>%
  group_by(category) %>%
  add_count(word) %>%
  rename(
    n_words_this_category = n
  ) %>%
  ungroup() %>%
  bind_tf_idf(word, category, n_words_this_category) %>%
  select(word, category, tf_idf, opinion_unit, sentiment)
```


What are the words in each category that have the maximum TF-IDF?


```r
category_tfidf_maxes <-
  category_tfidf %>%
  unnest() %>%
  group_by(category) %>%
  filter(tf_idf == max(tf_idf)) %>%
  select(word, sentiment, category, tf_idf) %>%
  distinct(word, category, tf_idf) %>%
  arrange(category, word)

category_tfidf_maxes %>%
  select(category, word) %>% 
  add_kable()
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Category </th>
   <th style="text-align:left;"> Word </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Attachments-Sharing </td>
   <td style="text-align:left;"> failure </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Calls </td>
   <td style="text-align:left;"> bridges </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Channels </td>
   <td style="text-align:left;"> exciting </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Customer Support </td>
   <td style="text-align:left;"> promised </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Desktop </td>
   <td style="text-align:left;"> impresses </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Ease of Use </td>
   <td style="text-align:left;"> patience </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emojis </td>
   <td style="text-align:left;"> incomplete </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Emojis </td>
   <td style="text-align:left;"> keyboards </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Feedback-Recommendation </td>
   <td style="text-align:left;"> amazed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Feedback-Recommendation </td>
   <td style="text-align:left;"> revolutionized </td>
  </tr>
  <tr>
   <td style="text-align:left;"> General </td>
   <td style="text-align:left;"> enjoying </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Groups </td>
   <td style="text-align:left;"> war </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Integrations </td>
   <td style="text-align:left;"> obstacle </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Messages </td>
   <td style="text-align:left;"> immortal </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mobile </td>
   <td style="text-align:left;"> billed </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mobile </td>
   <td style="text-align:left;"> deploys </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mobile </td>
   <td style="text-align:left;"> integrable </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Mobile </td>
   <td style="text-align:left;"> notifies </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Notifications </td>
   <td style="text-align:left;"> buzz </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Other </td>
   <td style="text-align:left;"> vibrant </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Performance-Quality-Reliability </td>
   <td style="text-align:left;"> disturbs </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Pricing </td>
   <td style="text-align:left;"> imposes </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Purpose </td>
   <td style="text-align:left;"> blame </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Search </td>
   <td style="text-align:left;"> granted </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Search </td>
   <td style="text-align:left;"> usefulness </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UI-UX </td>
   <td style="text-align:left;"> disappointment </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Web </td>
   <td style="text-align:left;"> fitting </td>
  </tr>
</tbody>
</table>






This kind of analysis can give us a sense of what people are actually saying and allows the humans to get a feel for where the pain points are and what could be improved.


## Wrap-Up

Here we've built a relatively straightforward pipeline for an analysis of web data. We grab and clean our raw data, feed it to MonkeyLearn for extraction and classification, and then analyze the results. MonkeyLearn allows us to abstract out the machine learning and plug into a simple and reliable API. 

This is an area where companies can compare their own metrics to the same data scraped from reviews of other companies and trained using the same or very similar modules.


Thanks and happy coding!



[^1]: Shoutout to some co-detective work with [Josh](https://www.fieldmuseum.org/blog/open-tree-life-toward-global-synthesis-phylogenetic-knowledge) [Stevens-Stein](https://github.com/jstevensstein)

[^2]: What I mean by that is: this particular topic classifier has a tree-like structure where each leaf belong to a single parent. MonkeyLearn first classifies each text into a top-level supercategory, one of: App-Software, Service, or Other. Once a text is classified at this first level, it then gets classified into one or more children in that supercategory. For instance, the children of App-Software are: Characteristics, Devices-Web, and Features. Finally, each of these has its own children. Take Characteristics: its children, which are leaf categories or terminal nodes for the classifier are Ease of Use, Integrations, Performance-Quality-Reliability, and UI-UX. This means that if a text doesn't appear to reference App-Software, the grandparent of UI-UX, it won't have a chance of being classified as UI-UX. This scheme of course doesn't preclude a text from being classified under multiple terminal nodes.

