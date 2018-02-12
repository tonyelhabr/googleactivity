---
title: "Analysis of Personal Google Activity"
author: ""
date: ""
---

```{r}
library("dplyr")
library("stringr")
```



```{r}
doc_html <- "data-raw/MyActivity.html"
search_archive <- xml2::read_html(doc_html)

date_search <-
  search_archive %>% 
  rvest::html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=<br>)(.*)(?<=PM|AM)") %>%
  lubridate::mdy_hms()

```


```{r}
text_search <-
  search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
  str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
  str_extract(pattern = '(?<=\">)(.*)')
```

