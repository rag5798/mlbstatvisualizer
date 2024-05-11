library(tidyverse)
library(gapminder)
library(dplyr)
library(rvest)

link <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
page <- link %>% read_html()

name <- page %>% html_nodes(".ipc-title--title") %>% html_text()
name <- name[2:251]
#name

span <- page %>% html_nodes(".feoqjK span") %>% html_text()
span

year <- c(span) %>% str_extract("\\d{4}") %>% na.omit()
#year

time <- grep("\\d+h( \\d+m)?|\\d+m", span, value = TRUE)
#time

rating <- span[which(span %in% c("G", "PG", "PG-13", "R", "Not Rated", "Approved", "Passed", "NC-17"))]
rating <- rating %>% append(NA, 81)
rating <- rating %>% append(NA, 76)
rating <- rating %>% append(NA, 57)
#rating

span <- page %>% html_nodes(".fkPBP span") %>% html_text()
rating <- span %>% str_extract("\\b\\d+\\.\\d+\\b") %>% na.omit()
#rating

movies <- data.frame(name, year, time, rating, stringsAsFactors = FALSE)

View(movies)