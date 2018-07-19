library(tidyverse)

# Function to update off/def rankings and calculate adjusted goals
adjust_update <- function(x, init, up_weight, avg_goals, home_adv,
                          season_order = NULL, date_order) {
  
  # Set up data as needed
  if(!is.null(season_order)) {
    x <- mutate(x, season = factor(season, levels = season_order),
                n_unique_season =  as.numeric(season))
  }
  
  if(is.numeric(avg_goals)) {
    avg_goals_tbl <- tibble(season = x$season, avg_goals = avg_goals)
  } else {
    avg_goals_tbl <- mutate(avg_goals, 
                            season = factor(season, levels = season_order))
  }
  
  if(is.numeric(home_adv)) {
    home_adv_tbl <- tibble(season = x$season, home_adv = home_adv)
  } else {
    home_adv_tbl <- mutate(home_adv, 
                           season = factor(season, levels = season_order))
  }
  
  # Create indicator for unique dates in each season
  x <- x %>%
    arrange(date) %>%
    group_by(season) %>%
    mutate(n_unique_date = as.numeric(date) - min(as.numeric(date)) + 1) %>%
    ungroup()
  
  # Initialize ratings objects to store results
  cur_rank <- init
  season_adj_goals <- list()
  season_ratings <- list()
  
  # Loop through seasons and weeks
  for(i in 1:max(x$n_unique_season)) {
    
    this_season_adj_goals <- tibble()
    
    for (j in 1:max(x$n_unique_date[x$n_unique_season == i])) {
      temp_x <- filter(x, n_unique_season == i, n_unique_date == j) %>% 
        # Merge in current ranking
        left_join(rename(cur_rank, h_off = off_rating, h_def = def_rating),
                  by = c("home" = "team")) %>%
        left_join(rename(cur_rank, a_off = off_rating, a_def = def_rating),
                  by = c("away" = "team"))%>%
        # Join home field advantage and average goals
        left_join(home_adv_tbl, by = "season") %>%
        left_join(avg_goals_tbl, by = "season") %>%
        # Calculate adjusted goals scored/allowed
        mutate(home_ags = ((hg - home_adv/2 - a_def) / 
                             (max(0.25, a_def * 0.424 + 0.548))) *
                 (avg_goals * 0.424 + 0.548) + avg_goals,
               home_aga = ((ag + home_adv/2 - a_off) / 
                             (max(0.25, a_off * 0.424 + 0.548))) *
                 (avg_goals * 0.424 + 0.548) + avg_goals,
               away_ags = ((ag + home_adv/2 - h_def) / 
                             (max(0.25, h_def * 0.424 + 0.548))) *
                 (avg_goals * 0.424 + 0.548) + avg_goals,
               away_aga = ((hg - home_adv/2 - h_off) / 
                             (max(0.25, h_off * 0.424 + 0.548))) *
                 (avg_goals * 0.424 + 0.548) + avg_goals) %>%
        # Remove variables not wanted in output
        select(-n_unique_date, -n_unique_season, -home_adv, -avg_goals)
      
      # Bind adjusted goals stats
      this_season_adj_goals <- bind_rows(this_season_adj_goals, temp_x)
      
      # Update rankings
      cur_rank <- temp_x %>%
        select(team = home, temp_off = home_ags, temp_def = home_aga) %>%
        bind_rows(temp_x %>%
                    select(team = away, temp_off = away_ags, temp_def = away_aga)) %>%
        right_join(cur_rank, by = "team") %>%
        mutate(off_rating = ifelse(is.na(temp_off), off_rating,
                                   (off_rating * (1 - up_weight)) + (temp_off * up_weight)),
               def_rating = ifelse(is.na(temp_def), def_rating,
                                   (def_rating * (1 - up_weight)) + (temp_def * up_weight))) %>%
        select(-temp_off, -temp_def)
    }
    # Save season adjusted goals and final season rankings
    season_adj_goals[[as.character(this_season_adj_goals$season[1])]] <-
      this_season_adj_goals
    
    season_ratings[[as.character(this_season_adj_goals$season[1])]] <-
      cur_rank
    
    # Initialize ratings for start of next season
    if (i != max(x$n_unique_season)) {
      # List of teams in league next season
      temp_rank <- tibble(team = unique(x$home[x$n_unique_season == (i + 1)]))
      
      # Average final off/def ratings for teams being relegated
      drop_off_rating <- mean(cur_rank$off_rating[!cur_rank$team %in% temp_rank$team])
      drop_def_rating <- mean(cur_rank$def_rating[!cur_rank$team %in% temp_rank$team])
      
      # Update ratings for new season
      cur_rank <- left_join(temp_rank, cur_rank, by = "team") %>% 
        mutate(off_rating = ifelse(is.na(off_rating), drop_off_rating,
                                   off_rating),
               def_rating = ifelse(is.na(def_rating), drop_def_rating,
                                   def_rating))
    }
    
  }
  
  list("data" = season_adj_goals, "ratings" = season_ratings)
}

# Premier league adjusted goals and ratings -----

# Load data
load("Data/prem.Rdata")

# Initial data set up
prem_reduce <- prem_league %>%
  select(season, date, home, away, hg, ag, res)

# Calculate season averages to use as initial starting values
season_averages <- prem_reduce %>%
  group_by(season) %>%
  summarize(avg_goals = mean((hg + ag) / 2),
            mean_hg = mean(hg),
            mean_ag = mean(ag),
            home_adv = mean_hg - mean_ag)

# Initial team ratings
init_ratings <- tibble(
  team = unique(prem_reduce$home[prem_reduce$season == "9394"]),
  off_rating = season_averages$avg_goals[season_averages$season == "9394"],
  def_rating = off_rating 
)

avg_goals <- select(season_averages, season, avg_goals)
home_adv <- select(season_averages, season, home_adv)

# Specify season order
season_order <- c("9394", "9495", "9596", "9697", "9798", "9899", "9900",
                  "0001", "0102", "0203", "0304", "0405", "0506", "0607",
                  "0708", "0809", "0910", "1011", "1112", "1213", "1314",
                  "1415", "1516", "1617", "1718", "1819")

# Calculate adjusted goals and ratings
prem_adjust <- adjust_update(x = prem_reduce,
                             init = init_ratings,
                             up_weight = (20 / sum(1:20)),
                             avg_goals = avg_goals,
                             home_adv = home_adv,
                             season_order = season_order)

prem_adjust_goals <- bind_rows(prem_adjust$data)

save(prem_adjust, prem_adjust_goals, file = "Data/Prem_Adjusted.Rdata")
