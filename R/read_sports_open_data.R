library(httr)
library(tidyverse)

# Set universal parameters for calls to API
url = 'https://sportsop-soccer-sports-open-data-v1.p.mashape.com/'
source("R/sports_open_data_key.R")

# Helper functions ------------------------------------------------------------

# Summarize match results
sum_match <- function(match) {
  tibble("date" = match$date_match,
         "home_team" = match$home$team,
         "home_goals" = match$home$goals,
         "away_team" = match$away$team,
         "away_goals" = match$away$goals)
}

# Pull a given weeks of matches
pull_prem_week <- function(round_slug, season) {
  
  # Set request URL
  request = paste0('v1/leagues/premier-league/seasons/',
                   season, '/rounds/',
                   round_slug, '/matches')
  
  # Pull data from API
  temp = GET(paste0(url, request),
             add_headers("X-Mashape-Key" = key),
             accept_json())
  
  # Stop if error
  stop_for_status(temp)
  
  # Create tibble of week's matches
  bind_rows(map(content(temp, "parsed")$data$matches, sum_match)) %>%
    mutate(match_week = as.integer(str_extract(round_slug, "[0-9][0-9]|[0-9]")),
           season = season)
}




# Pull matches from Prem 17/18 season -----------------------------------------
rounds <- 1:38
round_slugs <- paste0("round-", rounds)

# prem_1718 <- map(round_slugs, pull_week) %>% 
#   bind_rows()

# Create table after a given week
prem_1718_long <- prem_1718 %>%
  # Reformat home games
  mutate(location = "home") %>%
  mutate(result = case_when(home_goals > away_goals ~ "win",
                            home_goals < away_goals ~ "loss",
                            home_goals == away_goals ~ "draw",
                            TRUE ~ NA_character_)) %>%
  select("team" = "home_team", "opponent" = "away_team", result, 
         "goals_for" = "home_goals", "goals_against" = "away_goals", 
         location, date, season) %>%
  # Reformat away games
  bind_rows(prem_1718 %>%
              mutate(location = "away") %>%
              mutate(result = case_when(home_goals < away_goals ~ "win",
                                        home_goals > away_goals ~ "loss",
                                        home_goals == away_goals ~ "draw",
                                        TRUE ~ NA_character_)) %>%
              select("team" = "away_team", "opponent" = "home_team", result,
                     "goals_for" = "away_goals","goals_against" = "home_goals",
                     location, date, season))


# Need tibble with columns:
# Team, opp, result, goals for, goals against, home/away, date
prem_table <- prem_1718_long %>%
  group_by(team) %>%
  summarize(points = sum(3 * (result == "win") + (result == "draw")),
            record = paste(sum(result == "win"), sum(result == "loss"),
                           sum(result == "draw"), sep = "-"),
            goals_for = sum(goals_for),
            goals_against = sum(goals_against)) %>%
  arrange(desc(points))


