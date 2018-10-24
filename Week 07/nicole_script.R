#--------------------------------------------------------------
#Question 1:
#--------------------------------------------------------------
# Keeps track of wins and losses up to max number of bets or no bankroll
game_play = function(game, bankroll, num_bets, max_bets, bet)
{
  wins = 0
  losses = 0
  while (bankroll > 0 & num_bets < max_bets) 
  {
    roll = rbinom(1, 1, 0.5)
    if(roll == 1)
    {
      bankroll = bankroll + bet
      wins = wins + 1
      num_bets = num_bets + 1
    } else
    {
      bankroll = bankroll - bet
      losses = losses + 1
      num_bets = num_bets + 1
    }
  }
  game_matrix(game, wins, losses, bankroll)
}

# Saves a matrix with the individual trials as rows. 
#Saves in GLOBAL environment
game_matrix = function(entry, num_win, num_loss, money_left)
{
  trial[entry,1] <<- num_win + num_loss
  trial[entry,2] <<- num_win/trial[entry,1]
  trial[entry,3] <<- num_loss/trial[entry, 1]
  trial[entry, 4] <<- money_left
}

#Repeats everything for a set # of iterations. Saves results in matrix
#Prints probabilities, mean and variance
many_trials = function(bankroll, bet, num_bets, max_bets, iter, mean_fun, var_fun)
{
  for (game in 1:iter) 
  {
    bankroll2 = bankroll
    losses = 0
    wins = 0 
    game_play(game, bankroll2, num_bets, max_bets, bet)
  }
  return(c(calc_prob_busted(trial, iteration = iter), mean_time_busted(trial), 
           mean = mean_fun(trial$bankroll), var = var_fun(trial$bankroll)))
}

#Calculates probability of busting for a given number of maximum bets
calc_prob_busted = function(data, iteration)
{
  data %>%
    filter(bankroll <= 0) %>%
    summarise(busted_prob = n()/iteration)
}

#calculates the mean time you go bust
mean_time_busted = function(data)
{
  data %>%
    filter(bankroll <= 0) %>%
    summarise(mean_time = mean(total))
}

#Creates empty trial matrix saves to GLOBAL environment
empty_trial_mat = function(iter)
{
  trial <<- as.data.frame(matrix(data = 0, nrow = iter, ncol = 4))
  colnames(trial) <<- c("total", "prob of winning", "prob of losing", "bankroll")
}

#Creates empty matrix and runs everything for 5000 trials, 100 hands
set.seed(1234)
empty_trial_mat(5000)
many_trials(1000, 100, 0, 100, 5000, mean, var)

#Creates empty matrix and runs everything for 5000 trials, 500 hands
empty_trial_mat(5000)
many_trials(1000, 100, 0, 500, 5000, mean, var)

#Mean time of bust 5000 hands
#Creates empty matrix and runs everything for 5000 trials, 5000 hands
empty_trial_mat(5000)
many_trials(1000, 100, 0, 5000, 5000, mean, var)

#---------------------------------------------------------------------------------
# Question 2: 
#---------------------------------------------------------------------------------
markov_play = function(game, bankroll, win_prob, num_bets, max_bets, bet)
{
  wins = 0
  losses = 0
  while (num_bets < max_bets & win_prob <= 1) 
  {
    roll = rbinom(1, 1, win_prob)
    if(roll == 1 & win_prob < 1)
    {
      bankroll = bankroll + bet
      wins = wins + 1
      num_bets = num_bets + 1
      win_prob = win_prob + 0.01
    } else if(roll == 0)
    {
      bankroll = bankroll - bet
      losses = losses + 1
      num_bets = num_bets + 1
      win_prob = 0.48
    } else if(roll == 1 & win_prob == 1)
    {
      bankroll = bankroll + bet
      wins = wins + 1
      num_bets = num_bets + 1
    }
  }
 game_matrix(game, wins, losses, bankroll)
}

markov_trials = function(bankroll, win_prob, bet, num_bets, max_bets, iter, mean_fun, var_fun)
{
  for (game in 1:iter) 
  {
    bankroll2 = bankroll
    losses = 0
    wins = 0 
    markov_play(game, bankroll2, win_prob, num_bets, max_bets, bet)
  }
  return(c(calc_prob_busted(trial, iteration = iter), mean_time_busted(trial), 
           mean = mean_fun(trial$bankroll), var = var_fun(trial$bankroll)))
}

empty_trial_mat(100)
markov_trials(1000, 0.48, 100, 0, 100000, 100, mean, var)
range(trial$bankroll)

## Part B: 
install.packages('rlist')
library('rlist')
prob = seq(from = 0, to = 1, by = 0.1)
results = list()
for(i in 1:length(prob))
  {
    results[[i]] = markov_trials(1000, prob[i], 100, 0, 100000, 100, mean, var)
  }
  

