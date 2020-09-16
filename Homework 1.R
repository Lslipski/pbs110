#HW Parts 1-5

version #(Parts 1, 2)

# load anna_data.csv into dataframe (Part 3)
anna = read.csv("data_files/anna_data.csv")

# take a look
head(anna)

# create log(trials) variable and take a look (Part 4)
anna$log_trials = log(anna$trials)
head(anna)

# create histogram of original trials variable (Part 5)
hist(anna$trials)

# create histogram of new log_trials variable (Part 5)
hist(anna$log_trials)

#Part 6

# create single_game function to play one game of flipping until one person gets heads
single_game = function(){
    # Initialize persons A and B and loop counter
    A = 0
    B = 0
    i = 1
    # create coin
    coin = c(0, 1)
    
    # loop until someone flips 10 heads
    while (A < 10 & B < 10) {
    flip = sample(coin, size=1, replace=TRUE)
    # if heads is flipped
    if(flip == 1) {
        # even turns go to B 
        if((i %% 2) == 0) {
            B = B + 1
        # odd turns go to A
        } else {
            A = A + 1
        }
    }
    i = i+1
    }
    
    if(A == 10) {
        return("A")
    } else {
        return("B")
    }
}


# define simulate_games function to iterate over a game of coin flipping until one person gets 10 heads
# num_games parameter tells us how many games to play
simulate_games = function(num_games){
    
    winner_list = replicate(num_games, single_game()) 
    # return A divided by B, i.e. the probability of going first and winning the game over num_games iterations
    win_tab = table(winner_list)
    A_win = win_tab["A"]
    #print(A_win)
    B_win = win_tab["B"]
    #print(B_win)
    prob_A_Win = (A_win / B_win)
    return(prob_A_Win)
}

# run different numbers of simulations to see how the probability of winning if going first asymptotes
sample_sizes = c(10, 100, 1000, 10000, 20000, 50000, 75000, 100000, 200000)
df = data.frame(matrix(ncol=2, nrow=0))

for (size in 1:length(sample_sizes)){
    A_to_B_ratio = simulate_games(sample_sizes[size])
    new_result = c(sample_sizes[size], as.double(paste(A_to_B_ratio)))
    df = rbind(df, new_result)
    head(df)
    
    print(paste("For ", sample_sizes[size], " games played, Proportion of A Wins: ", A_to_B_ratio))
}

cols = c("num_simulations", "proportion_win_go_first")
names(df) = cols

plot(df$num_simulations, df$proportion_win_go_first, main="Probability of First-Turn-Winning by Number of Simulated Games"
    ,xlim=c(0, 200000))

#It appears that over many simulations of flipping the coin until one person flips 10 heads, the odds are in favor of the person #who flipped first winning. This advantage appears to be about 0.14 or 14%.