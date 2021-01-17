library(tidyverse)

wd = getwd()

race_df = read_csv(paste0(wd, '/race-result-race.csv'))

horse_df = read_csv((paste0(wd, '/race-result-horse.csv')))

#### EDA
#EW Rules

# 1-4, 1
#5-7, 2




unique(race_df$race_course)

full_df <- horse_df %>%
  dplyr:: left_join(subset(race_df, select = -c(sectional_time, incident_report)), 
                    by = 'race_id') %>%
  dplyr:: mutate(declared_horse_weight = as.numeric(declared_horse_weight),
                 length_behind_winner = ifelse(length_behind_winner == '-', 0, as.numeric(length_behind_winner)),
                 winner = ifelse(finishing_position ==1, 
                                 TRUE, 
                                 FALSE))

### Find favorite

bet = 1

fave_df = full_df %>%
  dplyr:: mutate(win_odds = as.numeric(win_odds)) %>%
  dplyr:: group_by(race_id) %>%
  dplyr:: filter(win_odds == min(win_odds, na.rm = T)) %>%
  dplyr:: select(finishing_position, horse_name, win_odds, race_id, race_date) %>%
  dplyr:: arrange(race_date) %>%
  dplyr:: mutate(outcome = ifelse(finishing_position ==1, bet*win_odds, -bet))

fave_df$cum_outcome = cumsum(fave_df$outcome)

ggplot(fave_df, aes(race_date,cum_outcome)) + 
  geom_line() + 
  theme_economist() + 
  scale_color_economist() +
  ggtitle("Returns from betting on the favourite")


### Random Forest Algorithm
library(tidymodels)

set.seed(123)
trees_split <- initial_split(full_df, strata = race_id)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


