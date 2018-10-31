#-----------------------------------------------------------------------
# Building of various functions
#-----------------------------------------------------------------------
# Keeps track of wins and losses up to max number of bets or no bankroll
play = function(game, bankroll, win_prob, num_bets, hands, bet, incr)
{
  wins = 0
  losses = 0
  start = win_prob
  while (bankroll > 0 & (num_bets < hands) & (win_prob <= 1)) 
  {
    roll = rbinom(1, 1, win_prob)
    if((roll == 1) & (win_prob < 1)) 
    {
      bankroll = bankroll + bet
      wins = wins + 1
      num_bets = num_bets + 1
      win_prob = win_prob + incr
    } else if(roll == 0)
    {
      bankroll = bankroll - bet
      losses = losses + 1
      num_bets = num_bets + 1
      win_prob = start
    } else if(roll == 1 & win_prob == 1)
    {
      bankroll = bankroll + bet
      wins = wins + 1
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

#Repeats everything for a set # of iterations. Saves results in matrix
#Prints probabilities, mean and variance
replicate_hands = function(bankroll, win_prob, bet, num_bets, hands, iter, incr, mean_fun, var_fun)
{
  for (game in 1:iter) 
  {
    losses = 0
    wins = 0 
    play(game, bankroll, win_prob, num_bets, hands, bet, incr)
  }
  return(c(calc_prob_busted(trial, iteration = iter), mean_time_busted(trial), 
           mean = mean_fun(trial$bankroll), var = var_fun(trial$bankroll)))
}
#-----------------------------------------------------------------------
#Question 1
#-----------------------------------------------------------------------
#Creates empty matrix and runs everything for 5000 trials, 100 hands
set.seed(1234)
empty_trial_mat(5000)
replicate_hands(bankroll = 1000, win_prob = 0.5, bet = 100, num_bets = 0, hands = 100, iter = 5000, incr = 0, mean_fun =  mean, var_fun = var)

#Creates empty matrix and runs everything for 5000 trials, 500 hands
empty_trial_mat(5000)
replicate_hands(bankroll = 1000, win_prob = 0.5, bet = 100, num_bets = 0, hands = 500, iter = 5000, incr = 0, mean_fun =  mean, var_fun = var)

#Mean time of bust 5000 hands
#Creates empty matrix and runs everything for 5000 trials, 5000 hands
empty_trial_mat(5000)
replicate_hands(bankroll = 1000, win_prob = 0.5, bet = 100, num_bets = 0, hands = 5000, iter = 5000, incr = 0, mean_fun =  mean, var_fun = var)

#-----------------------------------------------------------------------
#Question 2: American Roulette
#-----------------------------------------------------------------------
#Creates empty matrix and runs everything for 5000 trials, 100 hands
set.seed(1234)
empty_trial_mat(5000)
replicate_hands(bankroll = 1000, win_prob = 18/38, bet = 100, num_bets = 0, hands = 100, iter = 5000, incr = 0, mean_fun =  mean, var_fun = var)

#Creates empty matrix and runs everything for 5000 trials, 500 hands
empty_trial_mat(5000)
replicate_hands(bankroll = 1000, win_prob = 18/38, bet = 100, num_bets = 0, hands = 500, iter = 5000, incr = 0, mean_fun =  mean, var_fun = var)

#Mean time of bust 5000 hands
#Creates empty matrix and runs everything for 5000 trials, 5000 hands
empty_trial_mat(5000)
replicate_hands(bankroll = 1000, win_prob = 18/38, bet = 100, num_bets = 0, hands = 5000, iter = 5000, incr = 0, mean_fun =  mean, var_fun = var)

#-----------------------------------------------------------------------
#Question 3a
#-----------------------------------------------------------------------
#Markov example. win_prob incr. by 1% for each win. Goes back to 48% for losses
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.48, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun =  mean, var_fun = var)

#-----------------------------------------------------------------------
#Question 3b
#-----------------------------------------------------------------------
#start at win_prob = 50%
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.50, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun =  mean, var_fun = var)

#start at win_prob = 46%
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.46, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun =  mean, var_fun = var)

#sets inital values before loop
prob = seq(from = .46, to = .5, by = 0.001)
count = 1
money = 500000
empty_trial_mat(100)
test = replicate_hands(bankroll = money, win_prob = prob[1], bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun = mean, var_fun =  var)

#searches for probability that gives the expectation to be +/- 4% of the starting bankroll
while(test[[3]] > 1.04*money | test[[3]] < 0.96*money)
{
  count = count + 1
  test = replicate_hands(bankroll = money, win_prob = prob[count], bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun = mean, var_fun =  var)
  print(prob[count])
}

#-----------------------------------------------------------------------
#Question 3c
#-----------------------------------------------------------------------
#start at incr = 0.05%
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.48, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.005, mean_fun =  mean, var_fun = var)

#start at incr = 2%
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.48, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.02, mean_fun =  mean, var_fun = var)

#sets inital values before loop
increment = seq(from = 0.005, to = 0.02, by = 0.0001)
money = 500000
count = 1
empty_trial_mat(100)
test = replicate_hands(500000, 0.48, 100, 0, 100000, 100, incr = increment[1], mean, var)

#searches for increment that gives the expectation +/- 4% of the starting bankroll
while(test[[3]] > 1.04*money | test[[3]] < 0.96*money)
{
  count = count + 1
  test = replicate_hands(bankroll = money, win_prob = 0.48, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = increment[count], mean_fun = mean, var_fun = var)
  print(increment[count])
}

#-----------------------------------------------------------------------
#Question 4
#-----------------------------------------------------------------------
boot_ci = function(data, n, func)
{
  boot_stat = apply(replicate(n, sample(data[,4], dim(data)[1], replace = TRUE)), MARGIN = 2, func)
  quantile(boot_stat, c(0.025, 0.975))
}

#Finds 95% confidence interval of expected bankroll based on first Markov question with win_prob = .489
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.489, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun =  mean, var_fun = var)
boot_ci(trial, 1000, mean)

#-----------------------------------------------------------------------
#Question 5
#-----------------------------------------------------------------------
#Bootstrap variance based on 3b
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.489, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.01, mean_fun =  mean, var_fun = var)
boot_ci(trial, 1000, var)

#Bootstrap variance based on 3c
#3b has a much higher variance and wider confidence intervals
empty_trial_mat(100)
replicate_hands(bankroll = 500000, win_prob = 0.48, bet = 100, num_bets = 0, hands = 100000, iter = 100, incr = 0.0167, mean_fun =  mean, var_fun = var)
boot_ci(trial, 1000, var)



