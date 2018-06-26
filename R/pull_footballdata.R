# Pull all footballdata.co.uk datasets
library(tidyverse)

prem <- list()
prem_seasons <- c("9394", "9495", "9596", "9697", "9798", "9899", "9900",
                  "0001", "0102", "0203", "0304", "0405", "0506", "0607", 
                  "0708", "0809", "0910", "1011", "1112", "1213", "1314",
                  "1415", "1516", "1617", "1718")
for(this_season in prem_seasons) {
  prem[[this_season]] = read.csv(paste0("http://www.football-data.co.uk/mmz4281/",
                                        this_season, "/E0.csv"),
                                 stringsAsFactors = FALSE) %>%
    rename_all(tolower) %>%
    rename("hg" = fthg, "ag" = ftag, "res" = ftr)
}

prem_combined = bind_rows(prem, .id = "season") %>%
  select(-contains("x."), -x) %>%
  mutate(date = as.Date(date, "%d/%m/%y"))

save(prem_combined, file = "Data/Prem.Rdata")


