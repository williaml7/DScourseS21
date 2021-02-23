# practice web scraping

library(tidyverse)
library(rvest)

# html object from website

m100 = read_html("http://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression") 
m100

# put element tag for what we want in html_node()
# find the area you want on the web page with f12 and copy selector
# html_table() puts in tibble
pre_iaaf = m100 %>% html_node("table:nth-child(8)") %>% html_table()                             

# now we have a data frame of this wikipedia table (the first one on the web page)
class(pre_iaaf)
pre_iaaf

# 1912-1976 records
iaaf_76 <- m100 %>% html_node("table:nth-child(14)") %>% html_table()

# after 1976 records
modern <- m100 %>% html_node("table:nth-child(19)") %>% html_table()

# combine all the dfs
# very dirty, but does what it says
master <- bind_rows(pre_iaaf, iaaf_76, modern)

# polite package for polite web scraping (essentially same as above)
# takes longer and reads robots.txt to make sure it is allowed to take data
library(polite)

m100 <- scrape(bow("http://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression"))
pre_iaaf = m100 %>% html_node("table:nth-child(8)") %>% html_table()


