# ECON 5253
# William Lorton
# Second R exercise for PS4

# make sure to have these packages loaded
library(tidyverse)
library(sparklyr)

# Set up connection to Spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# Create tibble called df1 that loads in the iris data set
df1 <- as_tibble(iris)

# Copy tibble into Spark, calling it df
df <- copy_to(sc, df1)

# Verify the dfs are different types
class(df1) # regular tibble (tbl_df, tbl, data.frame)
class(df) # spark tibble (tbl_spark, tbl_sql, tbl_lazy, tbl)

# checking if column names are different between df1 and df
colnames(df1) # uses . as a separator 
colnames(df) # uses _ as a separator
# Spark may not be able to use . (or other special characters other than _) when naming variables

# apply the RDD/SQL operation: select
# List first 6 rows of the Sepal_Length and Species columns from df
df %>% select(Sepal_Length, Species) %>% head %>% print

# Another RDD operation: filter
# List first 6 rows of all columns of df where Sepal_Length > 5.5
df %>% filter(Sepal_Length > 5.5) %>% head %>% print

# Putting the two previous commands in one dplyr pipeline
df %>% select(Sepal_Length, Species) %>% filter(Sepal_Length > 5.5) %>% head %>% print

# Using another RDD operation: group_by
# Compute avg. sepal length and number of observations, by each of the three iris species
# Note that n() gives number of observations in current group
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>%
       head %>% print

# Another RDD operation: sort
# Sort df2 by species name in ascending order
df2 %>% arrange(Species) %>% head %>% print

