library(tidyverse)

wd = getwd()

race_df = read_csv(paste0(wd, '/race-result-race.csv'))

horse_df = read_csv((paste0(wd, '/race-result-horse.csv')))

#### EDA
#EW Rules - https://myracing.com/guides/guide-to-racing/many-places-horse-race/

# 1-4, 1
# 5-7, 2 at 1/4
# 8+ (non-handicap), 3 at 1/5
# 12-15(handicap), 3 at 1/4
# 16+ (handicap), 4 at 1/4

bet = 1

full_df <- horse_df %>%
  dplyr:: left_join(subset(race_df, select = -c(sectional_time, incident_report)), 
                    by = 'race_id') %>%
  dplyr:: add_count(race_id,
                    name = 'n_horses') %>%
  dplyr:: mutate(win_odds = as.numeric(win_odds),
                 declared_horse_weight = as.numeric(declared_horse_weight),
                 length_behind_winner = ifelse(length_behind_winner == '-', 0, as.numeric(length_behind_winner)),
                 winner = ifelse(finishing_position ==1, 
                                 TRUE, 
                                 FALSE),
                 each_way_outcome = case_when(
                   (finishing_position == 1) ~ 1/2*(bet * win_odds) + 1/2*(bet*1/4*win_odds),
                   between(n_horses,5,7) & between(finishing_position,2,3) ~ bet * win_odds*1/4,
                   (n_horses>7) & between(finishing_position,2,4) ~ bet * win_odds*1/4,
                   (7<n_horses) & between(finishing_position,2,4) & (!str_detect(full_df$race_name[1],'HANDICAP')) ~ bet * win_odds*1/5,
                   between(n_horses,12,15) & between(finishing_position,2,4) & (str_detect(full_df$race_name[1],'HANDICAP')) ~ bet * win_odds*1/4,
                   (15<n_horses) & between(finishing_position,2,5) & (str_detect(full_df$race_name[1],'HANDICAP')) ~ bet * win_odds*1/4,
                   TRUE~0 
                 ),
                 each_way_outcome = each_way_outcome - bet,
                 win_outcome = ifelse(finishing_position ==1, bet*win_odds, -bet))

  

### Find favorite

fave_df = full_df %>%
  dplyr:: mutate(win_odds = as.numeric(win_odds)) %>%
  dplyr:: group_by(race_id) %>%
  dplyr:: filter(win_odds == min(win_odds, na.rm = T)) %>%
  dplyr:: select(finishing_position, horse_name, win_odds, race_id, race_date, race_number) %>%
  dplyr:: arrange(race_date, race_number) %>%
  dplyr:: mutate(outcome = ifelse(finishing_position ==1, bet*win_odds, -bet))

fave_df$cum_outcome = 100 + cumsum(fave_df$outcome)

ggplot(fave_df, aes(race_date, cum_outcome)) + 
  geom_line() + 
  ggtitle("Returns from betting on the favourite")

fave_return = (fave_df[[nrow(fave_df),length(fave_df)]]-100)/100

fave_sr = #find sharpe ratio (find average monthly returns, find standard deviation of returns, divide one by the other)

### Random Forest Algorithm
library(tidymodels)

set.seed(123)
trees_split <- initial_split(full_df, strata = race_id)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


