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

# Getting data on team records from 1998 season through 2013 season on the 62 teams that were P5 by 2013:
# Warning: this loop can take ~10 minutes to complete; it is recommended to load the df that will result
# below. 

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
# save df.bcs since it takes ~10 minutes to scrape this in the loops above!
# save(df.bcs, file = "record_BCS.Rda"); load this data frame directly below.
load("record_BCS.Rda")

# Remove variables I don't need.
df.bcs <- df.bcs %>% select(year, team, total_wins, total_losses)
# create overall win percentage variable.
df.bcs <- df.bcs %>% mutate(winpct = total_wins / (total_losses + total_wins))
df.bcs <- df.bcs %>% select(year, team, winpct)

# Make national title winner dummy variable.
# Read in df of champions 1998-2013 and clean up.
bcs.champs <- read.csv("bcs_champs.csv", header = F)
bcs.champs <- bcs.champs %>% select(V1, V2)
bcs.champs$V2 <- gsub(pattern = "\\s*\\([^\\)]+\\)", replacement = "", bcs.champs$V2)
# LSU was the champion via BCS in 2003, though UCS was voted #1 in two final season polls.
bcs.champs$V2[11] <- "LSU"
bcs.champs <- bcs.champs %>% select(V2) %>% slice(rep(1:n(), each = 62))
bcs.champs <- bcs.champs %>% map_df(rev)
# put the name of the title winner in each year along the rows of df.bcs and set wontitle
# equal to 1 if the name of the champion matches the team name in the row; 0 else.
df.bcs$champ <- bcs.champs$V2
df.bcs <- df.bcs %>% rowwise() %>% mutate(wontitle = ifelse(champ == team, 1, 0))
df.bcs <- df.bcs %>% select(year, team, winpct, wontitle)

# Make dummy variables for top 15 in final AP poll and top 5 in final AP poll.
# First get historical data on final rankings.
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

# prepare for merge of ranking and other football data already gathered.
colnames(df.bcs)[2] <- "school"
colnames(df.ap)[1] <- "year"

# Merge df.ap and df.bcs to make dummies for final AP ranking.
df.bcs <- merge(x = df.bcs, y = df.ap, by = c("year", "school"), all.x = TRUE)
# replace NA ranks ("unranked) with 1000
df.bcs <- df.bcs %>% mutate_all(~replace(., is.na(.), 1000))
# top15 dummy
df.bcs <- df.bcs %>% mutate(top15 = ifelse(rank <= 15, 1, 0))
# top5 dummy
df.bcs <- df.bcs %>% mutate(top5 = ifelse(rank <= 5, 1, 0))

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
# Names in string vector below refer to conference affiliation. "Ind" refers to independent teams and is used for the 
# purpose of getting info on Notre Dame.
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
# All post-season AP #1 teams were national title winners, so we'll make our dummy based upon that this time.
# Make dummy variables for top 15 in final AP poll and top 5 in final AP poll below as well.
# First, get data on final AP rankings for this time period.
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

# Prepare df.playoff and df.ap for merge.
colnames(df.playoff)[2] <- "school"
colnames(df.ap)[1] <- "year"
# merge df.ap and df.playoff
df.playoff <- merge(x = df.playoff, y = df.ap, by = c("year", "school"), all.x = TRUE)
# replace NA ranks ("unranked) with 1000
df.playoff <- df.playoff %>% mutate_all(~replace(., is.na(.), 1000))
# top15 dummy
df.playoff <- df.playoff %>% mutate(top15 = ifelse(rank <= 15, 1, 0))
# top5 dummy
df.playoff <- df.playoff %>% mutate(top5 = ifelse(rank <= 5, 1, 0))
# wontitle dummy; all final AP #1 teams won the title in this period.
df.playoff <- df.playoff %>% mutate(wontitle = ifelse(rank == 1, 1, 0))

###### IPEDS Data #################################################################################################

# Data from Integrated Postsecondary Education Data System (IPEDS).
# See https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx?goToReportId=5
# Select the appropriate institutions (those that we just got CFB data on) and variables as described below.

# Variables:

# (Note: the most recent fall period includes students who enrolled early, during the preceding summer).

# 75th percentile SAT scores for the most recent cohort of fall enrollees available; include Math, Writing, and 
# Critical Reading and look to average these.

# Number of applications submitted in the most recent fall period.

# Acceptance rate in most recent fall period.

# Yield (proportion of admits who enrolled) in most recent fall period.

# Number of students who enrolled in most recent fall period.

# Could try to use additional variables to control for school quality: student-faculty ratio, average salaries
# of full professors, average total annual cost of attendance OS and IS, graduation rate, number of national merit
# scholars, pct of graduate students enrolled, pct of faculty with a doctorate, pct of students who go on
# to graduate school, amount of grant dollars awarded, etc., but will quickly run into missing data issues
# and/or multicollinearity issues. We will assume that overall school quality isn't changing over the time
# frames of our data sets.

# Data for test scores are only available starting in 2001, thus our data set must begin here instead of 1998. The SAT
# changed format several times over the period of this analysis. There was one period where a relatively large number of
# schools were not submitting writing section scores; the average SAT variable is computed using only the math and 
# reading section scores in this period.

ipeds.2019 <- read_csv("2019.csv")
colnames(ipeds.2019)[2] <- "school"
colnames(ipeds.2019)[5:13] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                "SATreadingWriting75", "SATmath75", "ACTcomposite75")
ipeds.2019 <- ipeds.2019 %>% mutate(avgSAT75 = (SATreadingWriting75 + SATmath75)/2)
ipeds.2019 <- ipeds.2019 %>% mutate(yield = yield/100)
ipeds.2019 <- ipeds.2019 %>% mutate(admitRate = admitRate/100)
ipeds.2019 <- ipeds.2019 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2018 <- read_csv("2018.csv")
colnames(ipeds.2018)[2] <- "school"
colnames(ipeds.2018)[5:13] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                "SATreadingWriting75", "SATmath75", "ACTcomposite75")
ipeds.2018 <- ipeds.2018 %>% mutate(avgSAT75 = (SATreadingWriting75 + SATmath75)/2)
ipeds.2018 <- ipeds.2018 %>% mutate(yield = yield/100)
ipeds.2018 <- ipeds.2018 %>% mutate(admitRate = admitRate/100)
ipeds.2018 <- ipeds.2018 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2017 <- read_csv("2017.csv")
colnames(ipeds.2017)[2] <- "school"
colnames(ipeds.2017)[5:13] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                "SATreadingWriting75", "SATmath75", "ACTcomposite75")
ipeds.2017 <- ipeds.2017 %>% mutate(avgSAT75 = (SATreadingWriting75 + SATmath75)/2)
ipeds.2017 <- ipeds.2017 %>% mutate(yield = yield/100)
ipeds.2017 <- ipeds.2017 %>% mutate(admitRate = admitRate/100)
ipeds.2017 <- ipeds.2017 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2016 <- read_csv("2016.csv")
colnames(ipeds.2016)[2] <- "school"
colnames(ipeds.2016)[5:13] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                "SATreadingWriting75", "SATmath75", "ACTcomposite75")
ipeds.2016 <- ipeds.2016 %>% mutate(avgSAT75 = (SATreadingWriting75 + SATmath75)/2)
ipeds.2016 <- ipeds.2016 %>% mutate(yield = yield/100)
ipeds.2016 <- ipeds.2016 %>% mutate(admitRate = admitRate/100)
ipeds.2016 <- ipeds.2016 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2015 <- read_csv("2015.csv")
colnames(ipeds.2015)[2] <- "school"
colnames(ipeds.2015)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2015 <- ipeds.2015 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2015 <- ipeds.2015 %>% mutate(yield = yield/100)
ipeds.2015 <- ipeds.2015 %>% mutate(admitRate = admitRate/100)
ipeds.2015 <- ipeds.2015 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2014 <- read_csv("2014.csv")
colnames(ipeds.2014)[2] <- "school"
colnames(ipeds.2014)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2014 <- ipeds.2014 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2014 <- ipeds.2014 %>% mutate(yield = yield/100)
ipeds.2014 <- ipeds.2014 %>% mutate(admitRate = admitRate/100)
ipeds.2014 <- ipeds.2014 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2013 <- read_csv("2013.csv")
colnames(ipeds.2013)[2] <- "school"
colnames(ipeds.2013)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2013 <- ipeds.2013 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2013 <- ipeds.2013 %>% mutate(yield = yield/100)
ipeds.2013 <- ipeds.2013 %>% mutate(admitRate = admitRate/100)
ipeds.2013 <- ipeds.2013 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2012 <- read_csv("2012.csv")
colnames(ipeds.2012)[2] <- "school"
colnames(ipeds.2012)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2012 <- ipeds.2012 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2012 <- ipeds.2012 %>% mutate(yield = yield/100)
ipeds.2012 <- ipeds.2012 %>% mutate(admitRate = admitRate/100)
ipeds.2012 <- ipeds.2012 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2011 <- read_csv("2011.csv")
colnames(ipeds.2011)[2] <- "school"
colnames(ipeds.2011)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2011 <- ipeds.2011 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2011 <- ipeds.2011 %>% mutate(yield = yield/100)
ipeds.2011 <- ipeds.2011 %>% mutate(admitRate = admitRate/100)
ipeds.2011 <- ipeds.2011 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2010 <- read_csv("2010.csv")
colnames(ipeds.2010)[2] <- "school"
colnames(ipeds.2010)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2010 <- ipeds.2010 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2010 <- ipeds.2010 %>% mutate(yield = yield/100)
ipeds.2010 <- ipeds.2010 %>% mutate(admitRate = admitRate/100)
ipeds.2010 <- ipeds.2010 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2009 <- read_csv("2009.csv")
colnames(ipeds.2009)[2] <- "school"
colnames(ipeds.2009)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2009 <- ipeds.2009 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2009 <- ipeds.2009 %>% mutate(yield = yield/100)
ipeds.2009 <- ipeds.2009 %>% mutate(admitRate = admitRate/100)
ipeds.2009 <- ipeds.2009 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2008 <- read_csv("2008.csv")
colnames(ipeds.2008)[2] <- "school"
colnames(ipeds.2008)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2008 <- ipeds.2008 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2008 <- ipeds.2008 %>% mutate(yield = yield/100)
ipeds.2008 <- ipeds.2008 %>% mutate(admitRate = admitRate/100)
ipeds.2008 <- ipeds.2008 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2007 <- read_csv("2007.csv")
colnames(ipeds.2007 )[2] <- "school"
colnames(ipeds.2007 )[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2007  <- ipeds.2007 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2007 <- ipeds.2007 %>% mutate(yield = yield/100)
ipeds.2007 <- ipeds.2007 %>% mutate(admitRate = admitRate/100)
ipeds.2007 <- ipeds.2007 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2006 <- read_csv("2006.csv")
colnames(ipeds.2006)[2] <- "school"
colnames(ipeds.2006)[c(4,6:14)] <- c("state", "admitRate", "yield", "applications", "admissions", "enrollments",
                                     "SATreading75", "SATmath75", "SATwriting75", "ACTcomposite75")
ipeds.2006 <- ipeds.2006 %>% mutate(avgSAT75 = (SATreading75 + SATmath75)/2)
ipeds.2006 <- ipeds.2006 %>% mutate(yield = yield/100)
ipeds.2006 <- ipeds.2006 %>% mutate(admitRate = admitRate/100)
ipeds.2006 <- ipeds.2006 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2005 <- read_csv("2005.csv")
colnames(ipeds.2005)[2] <- "school"
colnames(ipeds.2005)[c(4:10)] <- c("state", "applications", "admissions", "enrollments",
                                     "SATverbal75", "SATmath75", "ACTcomposite75")
ipeds.2005 <- ipeds.2005 %>% mutate(avgSAT75 = (SATverbal75 + SATmath75)/2)
ipeds.2005 <- ipeds.2005 %>% mutate(yield = round(enrollments / admissions, 2))
ipeds.2005 <- ipeds.2005 %>% mutate(admitRate = round(admissions / applications, 2))
ipeds.2005 <- ipeds.2005 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2004 <- read_csv("2004.csv")
colnames(ipeds.2004)[2] <- "school"
colnames(ipeds.2004)[c(4:10)] <- c("state", "applications", "admissions", "enrollments",
                                   "SATverbal75", "SATmath75", "ACTcomposite75")
ipeds.2004 <- ipeds.2004 %>% mutate(avgSAT75 = (SATverbal75 + SATmath75)/2)
ipeds.2004 <- ipeds.2004 %>% mutate(yield = round(enrollments / admissions, 2))
ipeds.2004 <- ipeds.2004 %>% mutate(admitRate = round(admissions / applications, 2))
ipeds.2004 <- ipeds.2004 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2003 <- read_csv("2003.csv")
colnames(ipeds.2003)[2] <- "school"
colnames(ipeds.2003)[c(4:10)] <- c("state", "applications", "admissions", "enrollments",
                                   "SATverbal75", "SATmath75", "ACTcomposite75")
ipeds.2003 <- ipeds.2003 %>% mutate(avgSAT75 = (SATverbal75 + SATmath75)/2)
ipeds.2003 <- ipeds.2003 %>% mutate(yield = round(enrollments / admissions, 2))
ipeds.2003 <- ipeds.2003 %>% mutate(admitRate = round(admissions / applications, 2))
ipeds.2003 <- ipeds.2003 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2002 <- read_csv("2002.csv")
colnames(ipeds.2002)[2] <- "school"
colnames(ipeds.2002)[c(4:10)] <- c("state", "applications", "admissions", "enrollments",
                                   "SATverbal75", "SATmath75", "ACTcomposite75")
ipeds.2002 <- ipeds.2002 %>% mutate(avgSAT75 = (SATverbal75 + SATmath75)/2)
ipeds.2002 <- ipeds.2002 %>% mutate(yield = round(enrollments / admissions, 2))
ipeds.2002 <- ipeds.2002 %>% mutate(admitRate = round(admissions / applications, 2))
ipeds.2002 <- ipeds.2002 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

ipeds.2001 <- read_csv("2001.csv")
colnames(ipeds.2001)[2] <- "school"
colnames(ipeds.2001)[c(4:10)] <- c("state", "applications", "admissions", "enrollments",
                                   "SATverbal75", "SATmath75", "ACTcomposite75")
ipeds.2001 <- ipeds.2001 %>% mutate(avgSAT75 = (SATverbal75 + SATmath75)/2)
ipeds.2001 <- ipeds.2001 %>% mutate(yield = round(enrollments / admissions, 2))
ipeds.2001 <- ipeds.2001 %>% mutate(admitRate = round(admissions / applications, 2))
ipeds.2001 <- ipeds.2001 %>% select("unitid", "school", "year", "state", "admitRate", "yield", "applications", 
                                    "admissions", "enrollments", "avgSAT75", 
                                    "ACTcomposite75")

# Create full ipeds data set.
ipeds.df <- bind_rows(ipeds.2001, ipeds.2002, ipeds.2003, ipeds.2004, ipeds.2005, ipeds.2006, ipeds.2007,
                   ipeds.2008, ipeds.2009, ipeds.2010, ipeds.2011, ipeds.2012, ipeds.2013, ipeds.2014,
                   ipeds.2015, ipeds.2016, ipeds.2017, ipeds.2018, ipeds.2019)

# save ipeds.df csv file
# write_csv(ipeds.df, "ipeds_df.csv")
ipeds.df <- read_csv("ipeds_df.csv")

########## MERGE CFB AND IPEDS DATA SETS AND CREATE LAGGED FOOTBALL SUCCESS VARIABLES ##################################################

# Prepare to merge IPEDS with BCS CFB data

# Remove Maryland, Rutgers, and Louisville for BCS era (as said before, these aren't added to P5 until 2014).
ipeds.df.bcs <- ipeds.df %>% filter(school != "University of Maryland-College Park" & school != "University of Louisville" & school != "Rutgers University-New Brunswick")
# Remove years after 2013
ipeds.df.bcs <- ipeds.df.bcs %>% filter(year <= 2013)

# Rename schools in CFB data to the names used in ipeds to prepare for merge.
df.bcs$school <- as.factor(df.bcs$school)
df.bcs$school <- fct_recode(df.bcs$school, 
                           "The University of Alabama" = "Alabama", 
                           "University of Arizona" = "Arizona", 
                           "Arizona State University-Tempe" = "Arizona State",
                           "University of Arkansas" = "Arkansas",
                           "Auburn University" = "Auburn",
                           "Baylor University" = "Baylor",
                           "Boston College" = "Boston College",
                           "University of California-Berkeley" = "California",
                           "Clemson University" = "Clemson",
                           "University of Colorado Boulder" = "Colorado",
                           "Duke University" = "Duke",
                           "University of Florida" = "Florida",
                           "Florida State University" = "Florida State",
                           "University of Georgia" = "Georgia",
                           "Georgia Institute of Technology-Main Campus" = "Georgia Tech",
                           "University of Illinois at Urbana-Champaign" = "Illinois",
                           "Indiana University-Bloomington" = "Indiana",
                           "University of Iowa" = "Iowa",
                           "Iowa State University" = "Iowa State",
                           "University of Kansas" = "Kansas",
                           "Kansas State University" = "Kansas State",
                           "University of Kentucky" = "Kentucky",
                           "Louisiana State University and Agricultural & Mechanical College" = "LSU",
                           "University of Miami" = "Miami",
                           "University of Michigan-Ann Arbor" = "Michigan",
                           "Michigan State University" = "Michigan State",
                           "University of Minnesota-Twin Cities" = "Minnesota",
                           "Mississippi State University" = "Mississippi State",
                           "University of Missouri-Columbia" = "Missouri",
                           "North Carolina State University at Raleigh" = "NC State",
                           "University of Nebraska-Lincoln" = "Nebraska",
                           "University of North Carolina at Chapel Hill" = "North Carolina",
                           "Northwestern University" = "Northwestern",
                           "University of Notre Dame" = "Notre Dame",
                           "Ohio State University-Main Campus" = "Ohio State",
                           "University of Oklahoma-Norman Campus" = "Oklahoma",
                           "Oklahoma State University-Main Campus" = "Oklahoma State",
                           "University of Mississippi" = "Ole Miss",
                           "University of Oregon" = "Oregon",
                           "Oregon State University" = "Oregon State",
                           "Pennsylvania State University-Main Campus" = "Penn State",
                           "University of Pittsburgh-Pittsburgh Campus" = "Pittsburgh",
                           "Purdue University-Main Campus" = "Purdue",
                           "University of South Carolina-Columbia" = "South Carolina",
                           "Stanford University" = "Stanford",
                           "Syracuse University" = "Syracuse",
                           "Texas Christian University" = "TCU",
                           "The University of Tennessee-Knoxville" = "Tennessee",
                           "The University of Texas at Austin" = "Texas",
                           "Texas A & M University-College Station" = "Texas A&M",
                           "Texas Tech University" = "Texas Tech",
                           "University of California-Los Angeles" = "UCLA",
                           "University of Southern California" = "USC",
                           "University of Utah" = "Utah",
                           "Vanderbilt University" = "Vanderbilt",
                           "University of Virginia-Main Campus" = "Virginia",
                           "Virginia Polytechnic Institute and State University" = "Virginia Tech",
                           "Wake Forest University" = "Wake Forest",
                           "University of Washington-Seattle Campus" = "Washington",
                           "Washington State University" = "Washington State",
                           "West Virginia University" = "West Virginia",
                           "University of Wisconsin-Madison" = "Wisconsin")

# Merge IPEDS and cfb data frames for BCS era.
df.bcs.merged <- merge(x = df.bcs, y = ipeds.df.bcs, by = c("year", "school"), all = TRUE)

# Make lagged CFB success variables.
# 1 year ago results are always 62 rows back since there are 62 P5 teams in BCS era.
# 2 years ago results are always 62*2 rows back.
# 3 years ago results are always 62*3 rows back.

# winpct lags
df.bcs.merged <- df.bcs.merged %>% mutate(winpctLag1 = dplyr::lag(winpct, n=(62)))
df.bcs.merged <- df.bcs.merged %>% mutate(winpctLag2 = dplyr::lag(winpct, n=(62*2)))
df.bcs.merged <- df.bcs.merged %>% mutate(winpctLag3 = dplyr::lag(winpct, n=(62*3)))

# wontitle lags
df.bcs.merged <- df.bcs.merged %>% mutate(wontitleLag1 = dplyr::lag(wontitle, n=(62)))
df.bcs.merged <- df.bcs.merged %>% mutate(wontitleLag2 = dplyr::lag(wontitle, n=(62*2)))
df.bcs.merged <- df.bcs.merged %>% mutate(wontitleLag3 = dplyr::lag(wontitle, n=(62*3)))

# top15 lags
df.bcs.merged <- df.bcs.merged %>% mutate(top15Lag1 = dplyr::lag(top15, n=(62)))
df.bcs.merged <- df.bcs.merged %>% mutate(top15Lag2 = dplyr::lag(top15, n=(62*2)))
df.bcs.merged <- df.bcs.merged %>% mutate(top15Lag3 = dplyr::lag(top15, n=(62*3)))

# top5 lags
df.bcs.merged <- df.bcs.merged %>% mutate(top5Lag1 = dplyr::lag(top5, n=(62)))
df.bcs.merged <- df.bcs.merged %>% mutate(top5Lag2 = dplyr::lag(top5, n=(62*2)))
df.bcs.merged <- df.bcs.merged %>% mutate(top5Lag3 = dplyr::lag(top5, n=(62*3)))

# Prepare to merge IPEDS data with playoff era CFB data.

ipeds.df.playoff <- ipeds.df %>% filter(year >= 2014)

# Rename schools in CFB data to the names used in IPEDS to prepare for merge.
df.playoff$school <- as.factor(df.playoff$school)
df.playoff$school <- fct_recode(df.playoff$school, 
                            "The University of Alabama" = "Alabama", 
                            "University of Arizona" = "Arizona", 
                            "Arizona State University-Tempe" = "Arizona State",
                            "University of Arkansas" = "Arkansas",
                            "Auburn University" = "Auburn",
                            "Baylor University" = "Baylor",
                            "Boston College" = "Boston College",
                            "University of California-Berkeley" = "California",
                            "Clemson University" = "Clemson",
                            "University of Colorado Boulder" = "Colorado",
                            "Duke University" = "Duke",
                            "University of Florida" = "Florida",
                            "Florida State University" = "Florida State",
                            "University of Georgia" = "Georgia",
                            "Georgia Institute of Technology-Main Campus" = "Georgia Tech",
                            "University of Illinois at Urbana-Champaign" = "Illinois",
                            "Indiana University-Bloomington" = "Indiana",
                            "University of Iowa" = "Iowa",
                            "Iowa State University" = "Iowa State",
                            "University of Kansas" = "Kansas",
                            "Kansas State University" = "Kansas State",
                            "University of Kentucky" = "Kentucky",
                            "Louisiana State University and Agricultural & Mechanical College" = "LSU",
                            "University of Miami" = "Miami",
                            "University of Michigan-Ann Arbor" = "Michigan",
                            "Michigan State University" = "Michigan State",
                            "University of Minnesota-Twin Cities" = "Minnesota",
                            "Mississippi State University" = "Mississippi State",
                            "University of Missouri-Columbia" = "Missouri",
                            "North Carolina State University at Raleigh" = "NC State",
                            "University of Nebraska-Lincoln" = "Nebraska",
                            "University of North Carolina at Chapel Hill" = "North Carolina",
                            "Northwestern University" = "Northwestern",
                            "University of Notre Dame" = "Notre Dame",
                            "Ohio State University-Main Campus" = "Ohio State",
                            "University of Oklahoma-Norman Campus" = "Oklahoma",
                            "Oklahoma State University-Main Campus" = "Oklahoma State",
                            "University of Mississippi" = "Ole Miss",
                            "University of Oregon" = "Oregon",
                            "Oregon State University" = "Oregon State",
                            "Pennsylvania State University-Main Campus" = "Penn State",
                            "University of Pittsburgh-Pittsburgh Campus" = "Pittsburgh",
                            "Purdue University-Main Campus" = "Purdue",
                            "University of South Carolina-Columbia" = "South Carolina",
                            "Stanford University" = "Stanford",
                            "Syracuse University" = "Syracuse",
                            "Texas Christian University" = "TCU",
                            "The University of Tennessee-Knoxville" = "Tennessee",
                            "The University of Texas at Austin" = "Texas",
                            "Texas A & M University-College Station" = "Texas A&M",
                            "Texas Tech University" = "Texas Tech",
                            "University of California-Los Angeles" = "UCLA",
                            "University of Southern California" = "USC",
                            "University of Utah" = "Utah",
                            "Vanderbilt University" = "Vanderbilt",
                            "University of Virginia-Main Campus" = "Virginia",
                            "Virginia Polytechnic Institute and State University" = "Virginia Tech",
                            "Wake Forest University" = "Wake Forest",
                            "University of Washington-Seattle Campus" = "Washington",
                            "Washington State University" = "Washington State",
                            "West Virginia University" = "West Virginia",
                            "University of Wisconsin-Madison" = "Wisconsin",
                            "University of Maryland-College Park" = "Maryland",
                            "Rutgers University-New Brunswick" = "Rutgers",
                            "University of Louisville" = "Louisville")

# merge IPEDS and cfb for playoff era.
df.playoff.merged <- merge(x = df.playoff, y = ipeds.df.playoff, by = c("year", "school"), all = TRUE)
# Get rid of 2020 since there is no IPEDS data yet.
df.playoff.merged <- df.playoff.merged %>% filter(year != 2020)

# make lagged CFB success variables.
# Only do one lag for this period since there isn't very much data.
# 1 year ago results are always 65 rows back since there are 65 P5 teams in playoff era.

# winpct lags
df.playoff.merged  <- df.playoff.merged  %>% mutate(winpctLag1 = dplyr::lag(winpct, n=(65)))

# wontitle lags
df.playoff.merged  <- df.playoff.merged  %>% mutate(wontitleLag1 = dplyr::lag(wontitle, n=(65)))

# top15 lags
df.playoff.merged  <- df.playoff.merged  %>% mutate(top15Lag1 = dplyr::lag(top15, n=(65)))

# top5 lags
df.playoff.merged  <- df.playoff.merged  %>% mutate(top5Lag1 = dplyr::lag(top5, n=(65)))

########## GET HIGH SCOOL GRADUATE & MEDIAN HH INCOME DATA ###################################################################################

# Number of public high school graduates by state 1998-2019 from NCES. 
# See https://nces.ed.gov/programs/digest/d16/tables/dt16_219.20.asp and other tables.
# Median HH income from Census Bureau. See Table H-8 at https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html.

# CSV file contains a data set that I combined in Excel from the above websites.
hsgradsHHincome.df <- read_csv("hsgradsHHincome.csv")
# drop DC.
hsgradsHHincome.df <- hsgradsHHincome.df %>% filter(state != "District Of Columbia")

# merge state grad and income data df with df.bcs.merged.
df.bcs.merged <- merge(x = df.bcs.merged, y = hsgradsHHincome.df, by = c("year", "state"), all.x = TRUE)

# merge state grad and income data df with df.playoff.merged
df.playoff.merged <- merge(x = df.playoff.merged, y = hsgradsHHincome.df, by = c("year", "state"), all.x = TRUE)

########## FINAL PREPARATIONS AND SAVE ###################################################################################

# filter to the years that have all the lags we want in both data sets.
# 3 lags in BCS data set; full data starts in 2001.
df.bcs.merged <- df.bcs.merged %>% filter(year >= 2001)
# only 1 lag in playoff data set since there isn't much data; full data starts in 2015.
df.playoff.merged <- df.playoff.merged %>% filter(year >= 2015)

# Convert variables to proper type in both data sets.

df.bcs.merged$year <- as.factor(df.bcs.merged$year)
df.bcs.merged$state <- as.factor(df.bcs.merged$state)
df.bcs.merged$wontitle <- as.factor(df.bcs.merged$wontitle)
df.bcs.merged$wontitleLag1 <- as.factor(df.bcs.merged$wontitleLag1)
df.bcs.merged$wontitleLag2 <- as.factor(df.bcs.merged$wontitleLag2)
df.bcs.merged$wontitleLag3 <- as.factor(df.bcs.merged$wontitleLag3)
df.bcs.merged$rank <- as.factor(df.bcs.merged$rank)
df.bcs.merged$top5 <- as.factor(df.bcs.merged$top5)
df.bcs.merged$top5Lag1 <- as.factor(df.bcs.merged$top5Lag1)
df.bcs.merged$top5Lag2 <- as.factor(df.bcs.merged$top5Lag2)
df.bcs.merged$top5Lag3 <- as.factor(df.bcs.merged$top5Lag3)
df.bcs.merged$top15 <- as.factor(df.bcs.merged$top15)
df.bcs.merged$top15Lag1 <- as.factor(df.bcs.merged$top15Lag1)
df.bcs.merged$top15Lag2 <- as.factor(df.bcs.merged$top15Lag2)
df.bcs.merged$top15Lag3 <- as.factor(df.bcs.merged$top15Lag3)
df.bcs.merged$unitid <- as.factor(df.bcs.merged$unitid)

df.playoff.merged$year <- as.factor(df.playoff.merged$year)
df.playoff.merged$state <- as.factor(df.playoff.merged$state)
df.playoff.merged$wontitle <- as.factor(df.playoff.merged$wontitle)
df.playoff.merged$wontitleLag1 <- as.factor(df.playoff.merged$wontitleLag1)
df.playoff.merged$rank <- as.factor(df.playoff.merged$rank)
df.playoff.merged$top5 <- as.factor(df.playoff.merged$top5)
df.playoff.merged$top5Lag1 <- as.factor(df.playoff.merged$top5Lag1)
df.playoff.merged$top15 <- as.factor(df.playoff.merged$top15)
df.playoff.merged$top15Lag1 <- as.factor(df.playoff.merged$top15Lag1)
df.playoff.merged$unitid <- as.factor(df.playoff.merged$unitid)

# Save these two data frames; NOTE: IMPUTATION HAS NOT BEEN PERFORMED ON THESE DATA SETS.
# df.bcs.merged contains the panel data set for the BCS era (includes P5 CFB data, institutional data,
# and state income and high school graduate data).
save(df.bcs.merged, file = "bcs_merged_noImpute.Rda")
# df.playoff.merged contains the panel data set for the Playoff era (includes P5 CFB data, institutional data,
# and state income and high school graduate data).
save(df.playoff.merged, file = "playoff_merged_noImpute.Rda")

# Please see the script named Lorton_project_reproduce.R for data imputation and analysis.
