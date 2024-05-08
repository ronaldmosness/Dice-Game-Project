# Function to simulate a player's turn
simulate_turn <- function(player, pot, initial_pot, starting_coins) {
  die_roll <- sample(1:6, 1)  # Simulate rolling a 6-sided die
  
  if (die_roll == 1) {
    # Do nothing
    return(list(player = player, pot = pot))
  } else if (die_roll == 2) {
    # Player takes all coins in the pot
    player$coins <- player$coins + pot
    pot <- 0
    return(list(player = player, pot = pot))
  } else if (die_roll == 3) {
    # Player takes half of the coins in the pot (rounded down)
    coins_taken <- floor(pot / 2)
    player$coins <- player$coins + coins_taken
    pot <- pot - coins_taken
    return(list(player = player, pot = pot))
  } else {
    # Player puts a coin in the pot
    player$coins <- player$coins - 1
    pot <- pot + 1
    return(list(player = player, pot = pot))
  }
}

# Function to simulate the game
simulate_game <- function(initial_pot, starting_coins) {
  player_A <- list(coins = starting_coins)  # Player A starts with specified coins
  player_B <- list(coins = starting_coins)  # Player B starts with specified coins
  pot <- initial_pot  # Initial number of coins in the pot
  cycles <- 0  # Initialize the number of cycles
  
  while (player_A$coins > 0 || player_B$coins > 0) {
    result_A <- simulate_turn(player_A, pot, initial_pot, starting_coins)  # Simulate player A's turn
    player_A <- result_A$player
    pot <- result_A$pot
    
    if (player_A$coins == 0) {
      break  # Player A loses, end the game
    }
    
    result_B <- simulate_turn(player_B, pot, initial_pot, starting_coins)  # Simulate player B's turn
    player_B <- result_B$player
    pot <- result_B$pot
    
    if (player_B$coins == 0) {
      break  # Player B loses, end the game
    }
    
    cycles <- cycles + 1  # Increment the number of cycles
  }
  
  return(cycles)
}

# Run the simulation for different combinations of initial pot size and starting coins
num_simulations <- 100  # Number of simulations to perform

initial_pot_sizes <- c(2, 4, 6)  # Different initial pot sizes
starting_coins <- c(4, 6, 8)  # Different starting coins

result_matrix <- matrix(0, nrow = length(initial_pot_sizes), ncol = length(starting_coins))

for (i in 1:length(initial_pot_sizes)) {
  for (j in 1:length(starting_coins)) {
    cycle_lengths <- vector("numeric", num_simulations)
    
    for (k in 1:num_simulations) {
      cycle_lengths[k] <- simulate_game(initial_pot_sizes[i], starting_coins[j])
    }
    
    expected_cycles <- mean(cycle_lengths)
    result_matrix[i, j] <- expected_cycles
  }
}

# Print the results
colnames(result_matrix) <- starting_coins
rownames(result_matrix) <- initial_pot_sizes

cat("Expected number of cycles:\n")
print(result_matrix)


# Plot the histograms
par(mfrow = c(length(initial_pot_sizes), length(starting_coins)))
for (i in 1:length(initial_pot_sizes)) {
  for (j in 1:length(starting_coins)) {
    hist(
      cycle_lengths,
      breaks = "FD",
      col = "skyblue",
      main = paste(
        "(Initial Pot:",
        initial_pot_sizes[i],
        ",StartingCoins:", starting_coins[j], ")"
      ),
      xlab = "Cycle Length",
      ylab = "Frequency"
    )
  }
}
