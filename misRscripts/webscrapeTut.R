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

