# ECON 5253
# William Lorton
# First R exercise for PS4
library(tidyverse)
library(jsonlite)

# (a)
# Download file from within R
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20210219&lang=en"')

# (b)
# Print file to console
system('cat dates.json')

# (c)
# convert file to data frame
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

# (d)
# check object type
class(mydf) # tibble
class(mydf$date) # character

# (e)
# List first 6 rows of the mydf dataframe
head(mydf)
# 1 1     Tiberius, under order of Augustus… en    By place  Roman Em… year
# 2 1     Gaius Caesar and Lucius Aemilius … en    By place  Roman Em… year
# 3 1     Gaius Caesar marries Livilla, dau… en    By place  Roman Em… year
# 4 1     Quirinius becomes a chief advisor… en    By place  Roman Em… year
# 5 1     Areius Paianeius becomes Archon o… en    By place  Roman Em… year
# 6 1     The ''Yuanshi'' era of the Chines… en    By place  Asia      year

# (f)
# Run this script using Rbatch