# Packages
library(tidyverse)
# devtools::install_github('jalapic/engsoccerdata', username = "jalapic")
library(engsoccerdata)
library(readxl)
library(lubridate)

### Read in data for different leagues from a variety of sources ###

### England 
# Pull all footballdata.co.uk datasets
prem <- list()
champ <- list()
league1 <- list()
league2 <- list()
seasons <- c("9394", "9495", "9596", "9697", "9798", "9899", "9900",
                  "0001", "0102", "0203", "0304", "0405", "0506", "0607", 
                  "0708", "0809", "0910", "1011", "1112", "1213", "1314",
                  "1415", "1516", "1617", "1718")
for(this_season in seasons) {
  # Premier League
  prem[[this_season]] = read.csv(paste0("http://www.football-data.co.uk/mmz4281/",
                                        this_season, "/E0.csv"),
                                 stringsAsFactors = FALSE) %>%
    rename_all(tolower) %>%
    rename("home" = hometeam, "away" = awayteam,
           "hg" = fthg, "ag" = ftag, "res" = ftr) 
  
  # Championship
  champ[[this_season]] = read.csv(paste0("http://www.football-data.co.uk/mmz4281/",
                                        this_season, "/E1.csv"),
                                 stringsAsFactors = FALSE) %>%
    rename_all(tolower) %>%
    rename("home" = hometeam, "away" = awayteam,
           "hg" = fthg, "ag" = ftag, "res" = ftr) 
  
  # League One
  league1[[this_season]] = read.csv(paste0("http://www.football-data.co.uk/mmz4281/",
                                         this_season, "/E2.csv"),
                                  stringsAsFactors = FALSE) %>%
    rename_all(tolower) %>%
    rename("home" = hometeam, "away" = awayteam,
           "hg" = fthg, "ag" = ftag, "res" = ftr) 
  
  # League Two
  league2[[this_season]] = read.csv(paste0("http://www.football-data.co.uk/mmz4281/",
                                         this_season, "/E3.csv"),
                                  stringsAsFactors = FALSE) %>%
    rename_all(tolower) %>%
    rename("home" = hometeam, "away" = awayteam,
           "hg" = fthg, "ag" = ftag, "res" = ftr) 
}

prem_league <- bind_rows(prem, .id = "season") %>%
  select(-contains("x."), -x) %>%
  mutate(date = as.Date(date, "%d/%m/%y"),
         # Fix messed up dates in 2002-2003 season
         date = case_when(
           season == "0203" & year(date) == 2020 ~ ymd(format(date, "2002-%m-%d")),
           TRUE ~ date
         )) %>%
  filter(!is.na(date))

save(prem_league, file = "Data/Prem.Rdata")

championship <- bind_rows(champ, .id = "season") %>%
  select(-contains("x."), -x) %>%
  mutate(date = as.Date(date, "%d/%m/%y"),
         # Fix messed up dates in 2002-2003 season
         date = case_when(
           season == "0203" & year(date) == 2020 ~ ymd(format(date, "2002-%m-%d")),
           TRUE ~ date
         )) %>%
  filter(!is.na(date))

save(championship, file = "Data/Championship.Rdata")

league_one <- bind_rows(league1, .id = "season") %>%
  select(-contains("x."), -x) %>%
  mutate(date = as.Date(date, "%d/%m/%y"),
         # Fix messed up dates in 2002-2003 season
         date = case_when(
           season == "0203" & year(date) == 2020 ~ ymd(format(date, "2002-%m-%d")),
           TRUE ~ date
         )) %>%
  filter(!is.na(date))

save(league_one, file = "Data/League_one.Rdata")

league_two <- bind_rows(league2, .id = "season") %>%
  select(-contains("x."), -x) %>%
  mutate(date = as.Date(date, "%d/%m/%y"),
         # Fix messed up dates in 2002-2003 season
         date = case_when(
           season == "0203" & year(date) == 2020 ~ ymd(format(date, "2002-%m-%d")),
           TRUE ~ date
         )) %>%
  filter(!is.na(date))

save(league_two, file = "Data/League_two.Rdata")

save(prem_league, championship, league_one, league_two,
     file = "Data/Football-data_England.Rdata")

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

# Premier league promotions and relegations
prem_prom_rel <- list(
  "9495" = tibble("new" = c("Crystal Palace", "Leicester", "Nott'm Forest"),
                  "gone" = c("Sheffield United", "Oldham", "Swindon")),
  "9596" = tibble("new" = c("Bolton", "Middlesbrough", NA, NA),
                  "gone" = c("Crystal Palace", "Leicester", "Norwich", "Ipswich")),
  "9697" = tibble("new" = c("Leicester", "Sunderland", "Derby"),
                  "gone" = c("Man City", "QPR", "Bolton")),
  "9798" = tibble("new" = c("Bolton", "Barnsley", "Crystal Palace"),
                  "gone" = c("Sunderland", "Middlesbrough", "Nott'm Forest")),
  "9899" = tibble("new" = c("Nott'm Forest", "Middlesbrough", "Charlton"),
                  "gone" = c("Bolton", "Barnsley", "Crystal Palace")),
  "9900" = tibble("new" = c("Sunderland", "Bradford", "Watford"),
                  "gone" = c("Nott'm Forest", "Charlton", "Blackburn")),
  "0001" = tibble("new" = c("Charlton", "Man City", "Ipswich"),
                  "gone" = c("Watford", "Wimbledon", "Sheffield Weds")),
  "0102" = tibble("new" = c("Fulham", "Blackburn", "Bolton"),
                  "gone" = c("Man City", "Coventry", "Bradford")),
  "0203" = tibble("new" = c("Man City", "West Brom", "Birmingham"),
                  "gone" = c("Ipswich", "Derby", "Leicester")),
  "0304" = tibble("new" = c("Portsmouth", "Leicester", "Wolves"),
                  "gone" = c("West Brom", "West Ham", "Sunderland")),
  "0405" = tibble("new" = c("Norwich", "West Brom", "Crystal Palace"),
                  "gone" = c("Leicester", "Wolves", "Leeds")),
  "0506" = tibble("new" = c("Sunderland", "Wigan", "West Ham"),
                  "gone" = c("Crystal Palace", "Norwich", "Southampton")),
  "0607" = tibble("new" = c("Reading", "Sheffield United", "Watford"),
                  "gone" = c("Birmingham", "West Brom", "Sunderland")),
  "0708" = tibble("new" = c("Sunderland", "Birmingham", "Derby"),
                  "gone" = c("Sheffield United", "Charlton", "Watford")),
  "0809" = tibble("new" = c("West Brom", "Hull", "Stoke"),
                  "gone" = c("Reading", "Birmingham", "Derby")),
  "0910" = tibble("new" = c("Wolves", "Birmingham", "Burnley"),
                  "gone" = c("Newcastle", "Middlesbrough", "West Brom")),
  "1011" = tibble("new" = c("Newcastle", "West Brom", "Blackpool"),
                  "gone" = c("Burnley", "Hull", "Portsmouth")),
  "1112" = tibble("new" = c("QPR", "Norwich", "Swansea"),
                  "gone" = c("Birmingham", "Blackpool", "West Ham")),
  "1213" = tibble("new" = c("Reading", "West Ham", "Southampton"),
                  "gone" = c("Bolton", "Blackburn", "Wolves")),
  "1314" = tibble("new" = c("Hull", "Cardiff", "Crystal Palace"),
                  "gone" = c("Wigan", "Reading", "QPR")),
  "1415" = tibble("new" = c("Leicester", "Burnley", "QPR"),
                  "gone" = c("Norwich", "Fulham", "Cardiff")),
  "1516" = tibble("new" = c("Bournemouth", "Watford", "Norwich"),
                  "gone" = c("Hull", "Burnley", "QPR")),
  "1617" = tibble("new" = c("Burnley", "Middlesbrough", "Hull"),
                  "gone" = c("Aston Villa", "Norwich", "Newcastle")),
  "1718" = tibble("new" = c("Newcastle", "Brighton", "Huddersfield"),
                  "gone" = c("Sunderland", "Middlesbrough", "Hull")),
  "1819" = tibble("new" = c("Wolves", "Cardiff", "Fulham"),
                  "gone" = c("Swansea", "Stoke", "West Brom"))
)

### USA

# Read USA data from 2012 onwards
usa <- read_excel("Data/USA.xlsx")

names(usa) <- tolower(names(usa))

# Save USA files to Rdata
save(usa, 
     file = "Data/USA.Rdata")