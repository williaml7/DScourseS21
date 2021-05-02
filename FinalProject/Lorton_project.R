# ECON 5253: Project
# May 10, 2021
# William Lorton

library(tidyverse)
library(rvest)
library(cfbfastR)
library(readxl)

##### Purpose #######################################################################################################
# This script contains all data gathering/cleaning, data summary generation, and data analysis completed in support 
# of the paper encapsulating this project (Lorton_project.pdf).
####################################################################################################################

# Get all P5 school names (remember to omit Maryland, Rutgers, and Louisville for the BCS era, keep for Playoff era):

# Read entire wikipedia page into R first.
p5.schools <- read_html("https://en.wikipedia.org/wiki/Power_Five_conferences")

# Get CSS selector for table and put in quotes for html_node function.
p5.schools <- p5.schools %>% html_node("#mw-content-text > div.mw-parser-output > div:nth-child(10) > table > tbody > tr > td:nth-child(1) > table")
# Put data into tibble.
p5.schools <- p5.schools %>% html_table()
# clean up
p5.schools[16,1] <- "Notre Dame"
p5.schools[8,1] <- "Miami"
colnames(p5.schools) <- c("ACC", "B1G", "B12", "PAC12", "SEC")
p5.schools <- p5.schools[-1,]
p5.schools <-  data.frame(c(p5.schools$ACC, p5.schools$B1G, p5.schools$B12, p5.schools$PAC12,
                            p5.schools$SEC))
colnames(p5.schools) <- "school"

# All P5 schools in CFB playoff era; 65 schools
p5.schools.playoff <- p5.schools %>% filter(school != "")
# All P5 schools in BCS era (same as Playoff, but without Maryland and Rutgers); 63 schools
p5.schools.bcs <- p5.schools %>% filter(school != "" & school != "Maryland" & school != "Rutgers" 
                                        & school != "Louisville") 
rm(p5.schools)

### COLLEGE FOOTBALL DATA: BCS ERA ################################################################################

# First data set: BCS era Power Five Conference Teams.

# Conferences: ACC, Big 12, PAC-12 (PAC-10), Big 10, SEC.
# Note that some of the teams in the data set were not actually part of a P5 conference for the entirety of the BCS
# era. We include every season for any team that does belong to the P5 by the end of the BCS era. The teams that
# were added some at some point in this era are TCU, West Virginia, Utah, Pittsburgh, and Syracuse. Maryland,
# Rutgers, and Louisville were not added until 2014 which is beyond the BCS era. There are 62 P5 teams if we
# count Notre Dame.

# We also include Notre Dame despite the fact that it technically has no conference affiliation for football.

# Seasons: 1998 season through 2013 season.

# Variables:
## Overall Win-Loss Ratio
## Final AP poll top 15 dummy.
## Final AP poll top 5 dummy.
## National title winner dummy (through BCS selection system).
## School name (to control for time-invariant school characteristics in the fixed effects model).

# Getting data on team records from 1998 season through 2013 season:
# Note that "PAC" refers to the Pac-12 (from 2011 onwards) and "Pac-10" refers to the Pacific 10 (prior to 2011).
# The Pacific conference added Utah and Colorado in 2011 to make 12 teams in the league.
# "Ind" is used to get Notre Dame.
iter <- 1
for (yr in 1998:2013) {
  
  for (school in p5.schools.bcs$school) {
  # Begin list of data frames
  if (iter == 1) {
    
    list.seasons.records <- list(cfbd_game_records(year = yr, team = school))
    iter <- 2
    next
    
  }
  
  list.seasons.records[iter] <- list(cfbd_game_records(year = yr, team = school))
  iter <- iter + 1
  }
  
}

# df containing every P5 team's record from 1998 season through 2013 season
df.bcs <- bind_rows(list.seasons.records)

# save since it takes ~10 minutes to scrape this in the loops!
save(df.bcs, file = "record_BCS.Rda")
# load("record_BCS.Rda")

# Remove variables I don't need.
df.bcs <- df.bcs %>% select(year, team, total_wins, total_losses)
# create overall win percentage variable.
df.bcs <- df.bcs %>% mutate(winpct = total_wins / (total_losses + total_wins))
df.bcs <- df.bcs %>% select(year, team, winpct)

# Make national title winner dummy variable.
bcs.champs <- read.csv("bcs_champs.csv", header = F)
bcs.champs <- bcs.champs %>% select(V1, V2)
bcs.champs$V2 <- gsub(pattern = "\\s*\\([^\\)]+\\)", replacement = "", bcs.champs$V2)
bcs.champs$V2[11] <- "LSU"
bcs.champs <- bcs.champs %>% select(V2) %>% slice(rep(1:n(), each = 62))
bcs.champs <- bcs.champs %>% map_df(rev)

df.bcs$champ <- bcs.champs$V2
df.bcs <- df.bcs %>% rowwise() %>% mutate(wontitle = ifelse(champ == team, 1, 0))
df.bcs <- df.bcs %>% select(year, team, winpct, wontitle)

# Make dummy variables for top 15 in final AP poll and top 5 in final AP poll
iter <- 1
for (yr in 1998:2013) {
  
    # Begin list of data frames
    if (iter == 1) {
      
      list.seasons.AP <- list(cfbd_rankings(year = yr, season_type = "postseason"))
      iter <- 2
      next
      
    }
    
    list.seasons.AP[iter] <- list(cfbd_rankings(year = yr, season_type = "postseason"))
    iter <- iter + 1
  
  
}

df.ap <- bind_rows(list.seasons.AP)
df.ap <- df.ap %>% filter(poll == "AP Top 25") %>% select(season, school, rank)

colnames(df.bcs)[2] <- "school"
colnames(df.ap)[1] <- "year"

# Making the dummies in Excel is much easier considering the way we have the data frame formatted.
#write_csv(df.ap, "ap_bcs.csv")
#write_csv(df.bcs, "df_bcs.csv")
# load the excel file below to get all of the steps for df.bcs that we've done above.
df.bcs <- read_excel("df_bcs.xlsx")
colnames(df.bcs)[1] <- "year"
colnames(df.bcs)[2] <- "school"
colnames(df.bcs)[6] <- "rank"
df.bcs <- df.bcs %>% select(year, school, winpct, wontitle, rank, top15, top5)

###### COLLEGE FOOTBALL DATA: PLAYOFF ERA ##########################################################################

# Second data set: Playoff era Power Five Conference Teams.

# Conferences: ACC, Big 12, PAC-12, Big 10, SEC.
# Maryland, Rutgers, and Louisville were added starting at the beginning of the playoff era (2014). There are 
# 65 P5 teams if we count Notre Dame in the playoff era.

# We include Notre Dame despite the fact that it technically has no conference affiliation for football.

# Seasons: 2014 season through 2020 season.

# Variables:
## Overall Win-Loss Ratio
## Final AP poll top 15 dummy.
## Final AP poll top 5 dummy.
## National title winner dummy (through Playoff system).
## School name (to control for time-invariant school characteristics in the fixed effects model).

# Getting data on team records from 2014 season through 2020 season:
iter <- 1
for (yr in 2014:2020) {
  
  for (conf in c("ACC", "B12", "B1G", "SEC", "PAC", "Ind")) {
    # Begin list of data frames
    if (iter == 1) {
      
      list.seasons.records <- list(cfbd_game_records(year = yr, conference = conf))
      iter <- 2
      next
      
    }
    
    list.seasons.records[iter] <- list(cfbd_game_records(year = yr, conference = conf))
    iter <- iter + 1
  }
  
}

# df containing every P5 team's record from 2014 season through 2020 season
df.playoff <- bind_rows(list.seasons.records)

# Remove independents that aren't Notre Dame
df.playoff <- df.playoff %>% filter(conference %in% c("ACC", "Big 12", "SEC", "Pac-12", "Big Ten") | team == "Notre Dame")

# Remove variables I don't need.
df.playoff <- df.playoff %>% select(year, team, total_wins, total_losses)
# create overall win percentage variable.
df.playoff <- df.playoff %>% mutate(winpct = total_wins / (total_losses + total_wins))
df.playoff <- df.playoff %>% select(year, team, winpct)

# Make national title winner dummy variable.
# All post-season AP #1 teams were national title winners, so we'll make our dummy based upon that.

# Make dummy variables for top 15 in final AP poll and top 5 in final AP poll
iter <- 1
for (yr in 2014:2020) {
  
  # Begin list of data frames
  if (iter == 1) {
    
    list.seasons.AP <- list(cfbd_rankings(year = yr, season_type = "postseason"))
    iter <- 2
    next
    
  }
  
  list.seasons.AP[iter] <- list(cfbd_rankings(year = yr, season_type = "postseason"))
  iter <- iter + 1
  
  
}

df.ap <- bind_rows(list.seasons.AP)
df.ap <- df.ap %>% filter(poll == "AP Top 25") %>% select(season, school, rank)

colnames(df.playoff)[2] <- "school"
colnames(df.ap)[1] <- "year"

# Making the dummies in Excel is much easier considering the way we have the data frame formatted.
#write_csv(df.ap, "ap_playoff.csv")
#write_csv(df.playoff, "df_playoff.csv")
# load the excel file below to get all of the steps for df.playoff that we've done above.
df.playoff <- read_excel("df_playoff.xlsx")
colnames(df.playoff)[1] <- "year"
colnames(df.playoff)[2] <- "school"
colnames(df.playoff)[5] <- "rank"
df.playoff <- df.playoff %>% select(year, school, winpct, rank, top15, top5, wontitle)



