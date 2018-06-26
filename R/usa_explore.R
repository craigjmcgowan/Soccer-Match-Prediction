library(tidyverse)
library(nnet)

# Load data
load("Data/USA.Rdata")

# Helper functions ------------------------------------------------------------

# Recent form - total goals for and away for specified preceeding matches
recent_form <- function(df, num_weeks) {

  df %>%
    select(date, season, "team" = home, "goals_for" = hg, 
           "goals_against" = ag) %>%
    bind_rows(df %>%
                select(date, season, "team" = away, 
                       "goals_for" = ag, "goals_against" = hg)) %>%
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


recent_form_spec <- function(df, num_weeks) {
  
  df %>%
    select(date, season, "team" = home, "home_goals_for" = hg, 
           "home_goals_against" = ag) %>%
    bind_rows(df %>%
                select(date, season, "team" = away, 
                       "away_goals_for" = ag, "away_goals_against" = hg)) %>%
    # Add zeroes for home/away goals as needed
    mutate_at(c("home_goals_for", "home_goals_against",
                "away_goals_for", "away_goals_against"),
              function(x) ifelse(is.na(x), 0, x)) %>%
    arrange(team, date) %>%
    group_by(season, team) %>%
    mutate(home_goals_for_lag = lag(zoo::rollsum(home_goals_for, num_weeks, 
                                            na.pad = TRUE,
                                            align = "right"), 1),
           home_goals_against_lag = lag(zoo::rollsum(home_goals_against, num_weeks, 
                                                na.pad = TRUE,
                                                align = "right"), 1),
           away_goals_for_lag = lag(zoo::rollsum(away_goals_for, num_weeks, 
                                                     na.pad = TRUE,
                                                     align = "right"), 1),
           away_goals_against_lag = lag(zoo::rollsum(away_goals_against, num_weeks, 
                                                     na.pad = TRUE,
                                                     align = "right"), 1)) %>%
    ungroup()
  
}


# Cumulative goals scored to that point in the season -------------------------

# Create dataset with total goals scored and goals against
# for each team to that point
cum_goal_sum <- usa %>%
  select(date, season, "team" = home, "goals_for" = hg, 
         "goals_against" = ag) %>%
  bind_rows(usa %>%
              select(date, season, "team" = away, 
                     "goals_for" = ag, "goals_against" = hg)) %>%
  arrange(team, date) %>%
  group_by(team, season) %>%
  mutate(goals_for_lag = lag(cumsum(goals_for), k = 1, default = NA),
         goals_against_lag = lag(cumsum(goals_against), k = 1, default = NA)) %>%
  ungroup() %>%
  select(date, season, team, goals_for_lag, goals_against_lag) 

# Recent form - 6 matches
usa_6matches <- recent_form(usa, 6)

# Recent form home/away goals - 6 matches
usa_home_away <- recent_form_spec(usa, 6)

# Join previous goal ratios to active match results
usa_results <- usa %>%
  mutate(res = factor(res)) %>%
  # Join cumulative home goals
  left_join(cum_goal_sum %>%
              rename("cum_home_for_lag" = goals_for_lag, 
                     "cum_home_against_lag" = goals_against_lag),
            by = c("date", "season", "home" = "team")) %>%
  # Join cumulative away goals
  left_join(cum_goal_sum %>%
              rename("cum_away_for_lag" = goals_for_lag, 
                     "cum_away_against_lag" = goals_against_lag),
            by = c("date", "season", "away" = "team")) %>%
  # Join recent form home goals
  left_join(usa_6matches %>%
              select("recent_home_for_lag" = goals_for_lag, 
                     "recent_home_against_lag" = goals_against_lag,
                     date, season, team),
            by = c("date", "season", "home" = "team")) %>%
  # Join recent form away goals
  left_join(usa_6matches %>%
              select("recent_away_for_lag" = goals_for_lag, 
                     "recent_away_against_lag" = goals_against_lag,
                     date, season, team),
            by = c("date", "season", "away" = "team")) %>%
  # Join recent form home goals by location
  left_join(usa_home_away %>%
              select("home_homefor_lag" = home_goals_for_lag, 
                     "home_homeagainst_lag" = home_goals_against_lag,
                     "home_awayfor_lag" = away_goals_for_lag,
                     "home_awayagainst_lag" = away_goals_against_lag,
                     date, season, team),
            by = c("date", "season", "home" = "team")) %>%
  # Join recent form away goals by location
  left_join(usa_home_away %>%
              select("away_awayfor_lag" = away_goals_for_lag, 
                     "away_awayagainst_lag" = away_goals_against_lag,
                     "away_homefor_lag" = home_goals_for_lag,
                     "away_homeagainst_lag" = home_goals_against_lag,
                     date, season, team),
            by = c("date", "season", "away" = "team")) %>%
  # Remove 2018 season in progress
  filter(season != 2018)

# Simple logistic regression
logistic_error <- tibble()
for (this_season in unique(usa_cumsum_results$season)) {
  
  # Split data into train and test sets
  train <- filter(usa_cumsum_results, season != this_season)
  test <- filter(usa_cumsum_results, season == this_season)
  
  away_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                    visitor_against_lag,
                  data = mutate(train, out = ifelse(res == "A", 1, 0)),
                  family = "binomial")
  draw_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                    visitor_against_lag,
                  data = mutate(train, out = ifelse(res == "D", 1, 0)),
                  family = "binomial")
  home_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                    visitor_against_lag,
                  data = mutate(train, out = ifelse(res == "H", 1, 0)),
                  family = "binomial")
  
  # Predicted output
  error <- train %>%
    mutate(pred_home = predict(home_reg, ., type = "response"),
           pred_away = predict(away_reg, ., type = "response"),
           pred_draw = predict(draw_reg, ., type = "response"),
           pred_result = case_when(
             pred_home > pred_away & pred_home > pred_draw ~ "H",
             pred_away > pred_home & pred_away > pred_draw ~ "A",
             pred_draw > pred_home & pred_draw > pred_home ~ "D",
             TRUE ~ NA_character_
           )) %>%
    summarize(season_out = this_season,
              train_per = sum(pred_result == res) / n()) %>%
    # Test error
    left_join(test %>%
                mutate(pred_home = predict(home_reg, ., type = "response"),
                       pred_away = predict(away_reg, ., type = "response"),
                       pred_draw = predict(draw_reg, ., type = "response"),
                       pred_result = case_when(
                         pred_home > pred_away & pred_home > pred_draw ~ "H",
                         pred_away > pred_home & pred_away > pred_draw ~ "A",
                         pred_draw > pred_home & pred_draw > pred_home ~ "D",
                         TRUE ~ NA_character_
                       )) %>%
                summarize(season_out = this_season,
                          test_per = sum(pred_result == res) / n()),
              by = "season_out")

  # Attach results to summary doc
  logistic_error <- bind_rows(logistic_error, error)
}

mean(logistic_error$train_per)
mean(logistic_error$test_per)
mean(nnet_error$train_per)
mean(nnet_error$test_per)


# Simple neural network
nnet_error <- tibble()
for (this_season in unique(usa_cumsum_results$season)) {
  
  # Split data into train and test sets
  train <- filter(usa_cumsum_results, season != this_season)
  test <- filter(usa_cumsum_results, season == this_season)

  nnet <- nnet(res ~ home_for_lag + home_against_lag + visitor_for_lag + 
                 visitor_against_lag,
               data = train, size = 4, maxit = 200)
  
  # Predicted output
  error <- train %>%
    mutate(pred_result = predict(nnet, ., type = "class")) %>%
    summarize(season_out = this_season,
              train_per = sum(pred_result == res) / n()) %>%
    # Test error
    left_join(test %>%
                mutate(pred_result = predict(nnet, ., type = "class")) %>%
                summarize(season_out = this_season,
                          test_per = sum(pred_result == res) / n()),
              by = "season_out")
  
  # Attach results to summary doc
  nnet_error <- bind_rows(nnet_error, error)
}

# Test different numbers of neurons in neural network
# Simple neural network
nnet_error <- tibble()
for (i in 3:25) {
  season_error <- tibble()
  for (this_season in unique(usa_cumsum_results$season)) {
  
    # Split data into train and test sets
    train <- filter(usa_cumsum_results, season != this_season)
    test <- filter(usa_cumsum_results, season == this_season)
    
    nnet <- nnet(res ~ home_for_lag + home_against_lag + visitor_for_lag + 
                   visitor_against_lag,
                 data = train, size = 4, maxit = 200)
    
    # Predicted output
    error <- train %>%
      mutate(pred_result = predict(nnet, ., type = "class")) %>%
      summarize(season_out = this_season,
                train_per = sum(pred_result == res) / n()) %>%
      # Test error
      left_join(test %>%
                  mutate(pred_result = predict(nnet, ., type = "class")) %>%
                  summarize(season_out = this_season,
                            test_per = sum(pred_result == res) / n()),
                by = "season_out")
    
    # Attach results to summary doc
    season_error <- bind_rows(season_error, error)
 
  }
  nnet_error <- season_error %>%
    summarize(size = i,
              avg_train = mean(train_per),
              avg_test = mean(test_per)) %>%
    bind_rows(nnet_error, .)
  
}

ggplot(data = nnet_error) +
  geom_line(aes(x = size, y = avg_train), color = "red") +
  geom_line(aes(x = size, y = avg_test), color = "blue")

usa_predict %>%
  filter(!is.na(pred_home_odds), pred_home_odds < avgh) %>%
  
  do(broom::tidy(table(.$res)))
  # Lost money

usa_predict %>%
  filter(!is.na(pred_draw_odds), pred_draw_odds < avgd) %>%
  do(broom::tidy(table(.$res)))
  # Lost money

usa_predict %>%
  filter(!is.na(pred_home_odds), pred_away_odds < avga) %>%
  do(broom::tidy(table(.$res)))




# Recent form - past 6 matches ------------------------------------------------


# Join previous goal ratios to active match results
usa_6match_results <- usa %>%
  mutate(res = factor(res)) %>%
  # Join home goal ratio
  left_join(usa_6matches %>%
              select("home_for_lag" = goals_for_lag, 
                     "home_against_lag" = goals_against_lag,
                     date, season, team),
            by = c("date", "season", "home" = "team")) %>%
  # Join away goal ratio
  left_join(usa_6matches %>%
              select("visitor_for_lag" = goals_for_lag, 
                     "visitor_against_lag" = goals_against_lag,
                     date, season, team),
            by = c("date", "season", "away" = "team")) %>%
  na.omit()

ggplot(usa_6match_results, aes(x = visitor_for_lag, y = visitor_against_lag, color = res)) +
  geom_jitter()

# Regression
away_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                  visitor_against_lag,
                data = usa_6match_results %>%
                  mutate(out = ifelse(res == "A", 1, 0)))

draw_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                  visitor_against_lag,
                data = usa_6match_results %>%
                  mutate(out = ifelse(res == "D", 1, 0)))

home_reg <- glm(out ~ home_for_lag + home_against_lag + visitor_for_lag + 
                  visitor_against_lag,
                data = usa_6match_results %>%
                  mutate(out = ifelse(res == "H", 1, 0)))

match6_nnet <- nnet(res ~ home_for_lag + home_against_lag + visitor_for_lag + 
                      visitor_against_lag,
                    data = usa_6match_results, size = 6, maxit = 200)

table()

# Predicted output
usa_6match_predict <- usa_6match_results %>%
  mutate(pred_home = predict(home_reg, ., type = "response"),
         pred_away = predict(away_reg, ., type = "response"),
         pred_draw = predict(draw_reg, ., type = "response"),
         pred_home_odds = 1/pred_home,
         pred_away_odds = 1/pred_away,
         pred_draw_odds = 1/pred_draw,
         pred_result = case_when(
           pred_home > pred_away & pred_home > pred_draw ~ "H",
           pred_away > pred_home & pred_away > pred_draw ~ "A",
           pred_draw > pred_home & pred_draw > pred_home ~ "D",
           TRUE ~ NA_character_
         ),
         nn_pred_result = predict(match6_nnet, ., type = "class")) %>%
  filter(!is.na(pred_home_odds))

# Matches classfied correctly
usa_6match_predict %>%
  group_by(nn_pred_result) %>%
  summarize(n = n(),
            per_acc = sum(res == nn_pred_result) / n)




usa_6match_predict %>%
  filter(pred_home_odds < maxh) %>%
  do(broom::tidy(table(.$res)))
# Lost money

usa_6match_predict %>%
  filter(pred_draw_odds < maxd) %>%
  do(broom::tidy(table(.$res)))
# Lost money

usa_6match_predict %>%
  filter(pred_away_odds < maxa) %>%
  do(broom::tidy(table(.$res)))





test <- usa_predict %>%
  filter(pred_home_odds < maxh) %>%
  mutate(diff = maxh - pred_home_odds) %>%
  select(home, away, res, maxh, pred_home_odds, diff) %>%
  mutate(diff_cat = case_when(
    diff < 0.5 ~ "< 0.5",
    diff < 1 ~ "0.5-1",
    diff < 2 ~ "1-2",
    TRUE ~ "> 2"
  ))
table(test$diff_cat, test$res)

ggplot(data = test, aes(x = res, y = diff)) +
  geom_point(aes(color = res))

# 6 matches recent form - 52.8% accuracy - only 2 away wins (both wrong)
# 4 matches recent form - 52.4% accuracy - all predicted home wins
# 8 matches recent form - 52.8% accuracy - only 1 away win (wrong)
# 



# Recent form - past 6 matches split by home/away goals------------------------
usa_home_away <- recent_form_spec(usa, 6)

# Join previous goal ratios to active match results
usa_home_away_results <- usa %>%
  mutate(res = factor(res)) %>%
  # Join home goal ratios
  left_join(usa_home_away %>%
              select("home_homefor_lag" = home_goals_for_lag, 
                     "home_homeagainst_lag" = home_goals_against_lag,
                     "home_awayfor_lag" = away_goals_for_lag,
                     "home_awayagainst_lag" = away_goals_against_lag,
                     date, season, team),
            by = c("date", "season", "home" = "team")) %>%
  # Join away goal ratio
  left_join(usa_home_away %>%
              select("away_awayfor_lag" = away_goals_for_lag, 
                     "away_awayagainst_lag" = away_goals_against_lag,
                     "away_homefor_lag" = home_goals_for_lag,
                     "away_homeagainst_lag" = home_goals_against_lag,
                     date, season, team),
            by = c("date", "season", "away" = "team")) %>%
  na.omit()


# Regression
away_reg <- glm(out ~ home_homefor_lag + home_homeagainst_lag +
                  home_awayfor_lag + home_awayagainst_lag +
                  away_awayfor_lag + away_awayagainst_lag +
                  away_homefor_lag + away_homeagainst_lag,
                data = usa_home_away_results %>%
                  mutate(out = ifelse(res == "A", 1, 0)),
                family = "binomial")
summary(away_reg)
# Home goals against @ home and away goals for @ home only sig

draw_reg <- glm(out ~ home_homefor_lag + home_homeagainst_lag +
                  home_awayfor_lag + home_awayagainst_lag +
                  away_awayfor_lag + away_awayagainst_lag +
                  away_homefor_lag + away_homeagainst_lag,
                data = usa_home_away_results %>%
                  mutate(out = ifelse(res == "D", 1, 0)),
                family = "binomial")
summary(draw_reg)
# Away goals against on road only sig

home_reg <- glm(out ~ home_homefor_lag + home_homeagainst_lag +
                  home_awayfor_lag + home_awayagainst_lag +
                  away_awayfor_lag + away_awayagainst_lag +
                  away_homefor_lag + away_homeagainst_lag,
                data = usa_home_away_results %>%
                  mutate(out = ifelse(res == "H", 1, 0)),
                family = "binomial")
summary(home_reg)

# Home goals for and against @ home, away goals for and against @ home sig

# Single layer neural network
match6spec_nnet <- nnet(res ~ home_homefor_lag + home_homeagainst_lag +
                          home_awayfor_lag + home_awayagainst_lag +
                          away_awayfor_lag + away_awayagainst_lag +
                          away_homefor_lag + away_homeagainst_lag,
                        data = usa_home_away_results, size = 10, maxit = 500)

table(predict(match6spec_nnet, usa_home_away_results, type = "class"))


# Predicted output
usa_home_away_predict <- usa_home_away_results %>%
  mutate(pred_home = predict(home_reg, ., type = "response"),
         pred_away = predict(away_reg, ., type = "response"),
         pred_draw = predict(draw_reg, ., type = "response"),
         pred_home_odds = 1/pred_home,
         pred_away_odds = 1/pred_away,
         pred_draw_odds = 1/pred_draw,
         pred_result = case_when(
           pred_home > pred_away & pred_home > pred_draw ~ "H",
           pred_away > pred_home & pred_away > pred_draw ~ "A",
           pred_draw > pred_home & pred_draw > pred_home ~ "D",
           TRUE ~ NA_character_
         ),
         nn_pred_result = predict(match6spec_nnet, ., type = "class")
  ) %>%
  filter(!is.na(pred_home_odds))

table(usa_home_away_predict$pred_result, 
      usa_home_away_predict$nn_pred_result)


# Matches classfied correctly
usa_home_away_predict %>%
  group_by(nn_pred_result) %>%
  summarize(n = n(),
            per_acc = sum(res == nn_pred_result) / n)




usa_home_away_predict %>%
  filter(pred_home_odds < maxh) %>%
  do(broom::tidy(table(.$res)))
# Lost money

usa_home_away_predict %>%
  filter(pred_draw_odds < maxd) %>%
  do(broom::tidy(table(.$res)))
# Lost money

usa_home_away_predict %>%
  filter(pred_away_odds < maxa) %>%
  do(broom::tidy(table(.$res)))





test <- usa_home_away_predict %>%
  filter(pred_home_odds < maxh) %>%
  mutate(diff = maxh - pred_home_odds) %>%
  select(home, away, res, maxh, pred_home_odds, diff) %>%
  mutate(diff_cat = case_when(
    diff < 0.5 ~ "< 0.5",
    diff < 1 ~ "0.5-1",
    diff < 2 ~ "1-2",
    TRUE ~ "> 2"
  ))
table(test$diff_cat, test$res)

ggplot(data = test, aes(x = res, y = diff)) +
  geom_point(aes(color = res))

# 6 matches recent form - 52.8% accuracy - only 2 away wins (both wrong)
# 4 matches recent form - 52.4% accuracy - all predicted home wins
# 8 matches recent form - 52.8% accuracy - only 1 away win (wrong)
# 