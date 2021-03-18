# Problem Set 6 R script
# William Lorton

library(rvest)
library(tidyverse)
library(cfbscrapR)

# Begin with this data from the previous problem set using the cfbscrapR package:
# Getting data on FBS team records using the following function:

get.fbsSeasonData <- function(years = 2000:2020) {
  
  require(cfbscrapR)

  iter <- 1
  for (i in years) {
    
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
  return(df)

}

# e.g., 2000 season through 2020 season data:
df <- get.fbsSeasonData(years = 2000:2020)

# Now I'm going to add on data for whether a given team won the national title in a given season:
# Read entire wikipedia page into R first.
champions <- read_html("https://www.sports-reference.com/cfb/years/index.html")

# Get table nodes and put tables into list (note that there's only one table on this particular page):
tables <- html_nodes(champions, "table")
# Put table into df.
champions <- data.frame(html_table(tables[1]))

# Remove years I don't care about right now (only want 2000-2020 season for this demonstration).
champions <- champions[1:21,]

# Remove all text after team name for the national.champion string variable:
champions$National.Champion <- gsub(pattern = "\\s*\\([^\\)]+\\)", replacement = "", champions$National.Champion)

# Add national champ data to main df:
# An example using the 2000 season:
# If team won national championship, variable will equal 1, else 0.

df.2000 <- df %>%
            filter(year == 2000)

champions.2000 <- champions %>%
                    filter(Year == 2000)

champ.data <- ifelse(df.2000$team == champions.2000$National.Champion, yes = 1, no = 0)

df.2000$national.champion <- champ.data

# The goal of course will be to have this national champion variable for every season that I want to look at.
# I could possibly write a loop for this, but there may be a better way of going about it; I will think about it 
# more later. One of the main challenges is the fact that there can be a different number of FBS teams in different
# seasons, which makes a repetition of the national champion for every row in the main df followed by a simple 
# row-wise string comparison difficult.


# Some visualizations:

# convert conference to categorical:
df$conference <- as.factor(df$conference)
levels(df$conference)

# avg wins by conference:
df.vis1 <- df %>%
            group_by(conference) %>%
            summarise(avg.total.wins = mean(total_wins))

# barchart for the above:
ggplot(data = df.vis1, aes(x = reorder(conference, avg.total.wins), y = avg.total.wins)) +
  geom_bar(stat = "identity") +
  labs(x = "Conference",
       y = "Average Total Wins Per Team",
       title = "Average Total Wins Per Team by FBS conference (2000-2020)") +
       theme(plot.title = element_text(hjust = 0.5)) +
       theme_bw() +
       theme(axis.text.x = element_text(size = 11.5, angle = 5))
  
# which team has done the best at home in terms of average total home wins per season from 2000 to 2020?
df.vis2 <- df %>%
            group_by(team) %>%
            summarise(avg.home.wins = mean(home_wins))

# arrange df in descending order by avg.home.wins:
df.vis2 <- df.vis2 %>%
            arrange(desc(avg.home.wins))

ggplot(data = head(df.vis2, 10), aes(x = reorder(team, avg.home.wins), y = avg.home.wins)) +
  geom_bar(stat = "identity") +
  labs(x = "Team",
       y = "Average Home Wins",
       title = "Top Ten Teams in Average Number of Home Wins per Season (2000-2020)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11.5, angle = 5))

# and which teams have done the best on the road from 2000 to 2020?

df.vis3 <- df %>%
  group_by(team) %>%
  summarise(avg.away.wins = mean(away_wins))

# arrange df in descending order by avg.away.wins:
df.vis3 <- df.vis3 %>%
  arrange(desc(avg.away.wins))

ggplot(data = head(df.vis3, 10), aes(x = reorder(team, avg.away.wins), y = avg.away.wins)) +
  geom_bar(stat = "identity") +
  labs(x = "Team",
       y = "Average Away Wins",
       title = "Top Ten Teams in Average Number of Away Wins per Season (2000-2020)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 11.5, angle = 5))

  

  



