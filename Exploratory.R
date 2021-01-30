library(tidyverse)
library(lubridate)

wd = getwd()

race_df = read_csv(paste0(wd, '/race-result-race.csv'))
horse_df = read_csv((paste0(wd, '/race-result-horse.csv')))

#### EDA
#EW Rules - https://myracing.com/guides/guide-to-racing/many-places-horse-race/

bet = 1
starting_pot = 100

full_df <- horse_df %>%
  dplyr:: left_join(subset(race_df, select = -c(sectional_time, incident_report)), 
                    by = 'race_id') %>%
  # Add number of horses
  dplyr:: add_count(race_id,
                    name = 'n_horses') %>%
  tidyr:: drop_na(win_odds) %>% 
  dplyr:: group_by(race_id) %>%
  # Get odds prediction for each race
  dplyr:: mutate(win_odds = as.numeric(win_odds),
                 predicted_position = rank(win_odds)) %>%
  dplyr:: ungroup() %>%
  # Get various odds returns
  dplyr:: mutate(declared_horse_weight = as.numeric(declared_horse_weight),
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
                 each_way_outcome = each_way_outcome - bet, # remove initial bet amount
                 win_outcome = ifelse(finishing_position ==1, bet*win_odds, -bet)) # remove initial bet amount

### Find outcomes from strategies

returns_df = full_df %>%
  dplyr:: mutate(bet_fave_win_outcome = ifelse(predicted_position == 1, win_outcome, 0),
                 bet_fave_ew_outcome = ifelse(predicted_position == 1, each_way_outcome, 0),
                 bet_second_ew_outcome = ifelse(predicted_position == 2, each_way_outcome, 0),
                 bet_third_ew_outcome = ifelse(predicted_position == 3, each_way_outcome, 0)) %>%
  dplyr:: select(race_date, race_number, 
                 bet_fave_win_outcome, bet_fave_ew_outcome, bet_second_ew_outcome, bet_third_ew_outcome) %>%
  dplyr:: arrange(race_date, race_number)

returns_df$cum_fave_win_outcome = 100 + cumsum(returns_df$bet_fave_win_outcome)
returns_df$cum_fave_ew_outcome = 100 + cumsum(returns_df$bet_fave_ew_outcome)
returns_df$cum_second_ew_outcome = 100 + cumsum(returns_df$bet_second_ew_outcome)
returns_df$cum_third_ew_outcome = 100 + cumsum(returns_df$bet_third_ew_outcome)

returns_df = returns_df %>%
  dplyr:: select(race_date,race_number, 
                 cum_fave_win_outcome, cum_fave_ew_outcome, cum_second_ew_outcome, cum_third_ew_outcome) %>%
  tidyr:: gather(key = 'bet_type',
                 value = "cumulative_performance",
                 -race_date, -race_number) %>%
  tidyr:: drop_na()

####

ggplot(returns_df, 
       aes(x = race_date, y = cumulative_performance, col = bet_type)) + 
  geom_line() + 
  labs(title = "Returns from betting on the favourite", 
          subtitle = 'With £100 starting pot and £1 bets') 

first_race_date = min(full_df$race_date)
last_race_date = max(full_df$race_date)
last_race_number = max(full_df[full_df$race_date == last_race_date, 'race_number'])
total_months = interval(first_race_date, last_race_date) %/% months(1) 

finalReturnFind = function(df = returns_df, type){
  
   res = df %>%
    dplyr:: filter(race_date == last_race_date & 
                   race_number == last_race_number &
                   bet_type == type) %>%
    dplyr:: select(cumulative_performance) %>%
    utils:: head(1)
  
   total_return_pct = ((res[[1]] - starting_pot)/starting_pot)*100
   
   monthly_return_pct = total_return_pct/total_months # CHECK THIS
   
  return(list(total_return_pct, monthly_return_pct))
  
}

fave_win_final_returns = finalReturnFind(type = 'cum_fave_win_outcome')
fave_ew_final_returns = finalReturnFind('cum_fave_ew_outcome')
fave_second_ew_final_returns = finalReturnFind('cum_second_ew_outcome')
fave_third_ew_final_returns = finalReturnFind('cum_third_ew_outcome')



#fave_sr = #find sharpe ratio (find average monthly returns, find standard deviation of returns, divide one by the other)

### Random Forest Algorithm
library(tidymodels)

set.seed(123)
trees_split <- initial_split(full_df, strata = race_id)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)


