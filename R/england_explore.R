library(tidyverse)

# Load data
load("Data/England.Rdata")

# Helper functions ------------------------------------------------------------

# Recent form - total goals for and away for specified preceeding matches
recent_form <- function(df, num_weeks) {

  df %>%
    select(date, season, "team" = home, "goals_for" = hgoal, 
           "goals_against" = vgoal) %>%
    bind_rows(df %>%
                select(date, season, "team" = visitor, 
                       "goals_for" = vgoal, "goals_against" = hgoal)) %>%
    arrange(team, date) %>%
    group_by(season, team) %>%
    mutate(goals_for_lag = lag(zoo::rollsum(goals_for, num_weeks, 
                                            na.pad = TRUE,
                                            align = "right"), 1),
           goals_against_lag = lag(zoo::rollsum(goals_against, num_weeks, 
                                                na.pad = TRUE,
                                                align = "right"), 1)) %>%
    ungroup()
  
}


# Plot result vs home/away goals scored/against ratio to that point in season ----

# Create dataset with total goals scored and goals against
# for each team to that point
goal_sum <- prem_league %>%
  select(date, season, "team" = home, "goals_for" = hgoal, 
         "goals_against" = vgoal) %>%
  bind_rows(prem_league %>%
              select(date, season, "team" = visitor, 
                     "goals_for" = vgoal, "goals_against" = hgoal)) %>%
  arrange(team, date) %>%
  group_by(team, season) %>%
  mutate(goals_for_lag = lag(cumsum(goals_for), k = 1, default = NA),
         goals_against_lag = lag(cumsum(goals_against), k = 1, default = NA)) %>%
  ungroup() %>%
  select(date, season, team, goals_for_lag, goals_against_lag) 


# Join previous goal ratios to active match results
prem_results <- prem_league %>%
  # Join home goal ratio
  left_join(goal_sum %>%
              rename("home_for_lag" = goals_for_lag, 
                     "home_against_lag" = goals_against_lag),
            by = c("date", "season", "home" = "team")) %>%
  # Join away goal ratio
  left_join(goal_sum %>%
              rename("visitor_for_lag" = goals_for_lag, 
                     "visitor_against_lag" = goals_against_lag),
            by = c("date", "season", "visitor" = "team"))

# Regression
away_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                  visitor_against_lag,
                data = prem_results %>%
                  mutate(out = ifelse(result == "A", 1, 0)))

draw_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                  visitor_against_lag,
                data = prem_results %>%
                  mutate(out = ifelse(result == "D", 1, 0)))

home_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                  visitor_against_lag,
                data = prem_results %>%
                  mutate(out = ifelse(result == "H", 1, 0)))

# Predicted output
prem_predict <- prem_results %>%
  mutate(pred_home = predict(home_reg, ., type = "response"),
         pred_away = predict(away_reg, ., type = "response"),
         pred_draw = predict(draw_reg, ., type = "response"),
         pred_result = case_when(
           pred_home > pred_away & pred_home > pred_draw ~ "H",
           pred_away > pred_home & pred_away > pred_draw ~ "A",
           pred_draw > pred_home & pred_draw > pred_home ~ "D",
           TRUE ~ NA_character_))



