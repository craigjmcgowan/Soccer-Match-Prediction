# Packages
library(tidyverse)
library(engsoccerdata)
library(readxl)

### Read in data for different leagues from a variety of sources ###

### England 

# Combine historical and current England data together
all_england <- rbind(england, england_current()) %>%
  rename_all(tolower)

# Premier League only
prem_league <- all_england %>%
  filter(division == 1, season >= 1992)

# Championship only
championship <- all_england %>%
  filter(division == 2)

# Save England files to Rdata
save(all_england, prem_league, championship, 
     file = "Data/England.Rdata")

### USA

# Read USA data from 2012 onwards
usa <- read_excel("Data/USA.xlsx")

names(usa) <- tolower(names(usa))

# Save USA files to Rdata
save(usa, 
     file = "Data/USA.Rdata")