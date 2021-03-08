# Problem Set 5 R Script
# William Lorton

library(tidyverse)

# Question 3:

library(rvest)

# Scraping some data using SelectorGadget.
# Target is a wikipedia table of FBS Division I college football rivalries. 
# May use this to help make a variable that denotes whether a team beat its 'biggest' rival in a given year.

# Read entire wikipedia page into R first.
cfb.rivals <- read_html("https://en.wikipedia.org/wiki/List_of_NCAA_college_football_rivalry_games")

# Get CSS selector for table using SelectorGadget, and put in quotes for html_node function.
fbs.rivals <- cfb.rivals %>% html_node("#mw-content-text > div.mw-parser-output > table:nth-child(6)")
# Put data into tibble.
fbs.rivals <- fbs.rivals %>% html_table()

# Question 4:

library(cfbscrapR)

# Using an API to get data from the web.
# Going to use the API that goes along with https://collegefootballdata.com/.

# Download R package for CFB data:
# devtools::install_github(repo = "saiemgilani/cfbscrapR")

# Getting data on team records from 2000 season through 2020 season using for loop:
iter <- 1
for (i in 2000:2020) {
  
  # Begin list of data frames
  if (iter == 1) {
    
  list.seasons.records <- list(cfb_game_records(year = i))
  iter <- 2
  next
  
  }
  
  list.seasons.records[iter] <- list(cfb_game_records(year = i))
  iter <- iter + 1
  
}

# df containing every FBS team's record from 2000 season through 2020 season
df <- bind_rows(list.seasons.records)


