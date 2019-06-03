library(Cairo)

## For streak of length 1:
simtosses <- 1e4 # number of simulations
n <- 3 # number of tosses per simulation
p <- .5 # probability of a succes (Tails/successful shot)

results <- rep(NA, simtosses) # initializing result vector with as many entries as simulations
for (sim in 1:simtosses) {
  tosses <- rbinom(n, 1, p)
  candidates <- which(tosses == 1) + 1
  observed_candidates <- candidates[candidates <= n]
  results[sim] <- sum(tosses[observed_candidates]) / length(observed_candidates)
}
expected_heads <- mean(na.omit(results))
expected_heads

## For more general ks
get_expected_heads <- function(n, k = 3, p = 0.5, simtosses = 1e4) {
  results <- rep(NA, simtosses) # initializing result vector with as many entries as simulations
  for (sim in 1:simtosses) {
    tosses <- rbinom(n, 1, p) # simulating n tosses with 0 and 1 as possible results for each toss and probability p that result is equal to 1
    runs <- rle(tosses) # run length encoding of the tosses
    # calculating the number of tails after k successive heads that are possible at most based on the run length encoding information
    # this is just the number of runs of heads while the length of the run is greater than k
    n_tails_after <- length(which(runs$values == 1 & runs$lengths >= k))
    # calculating the number of heads that occur after k successive heads
    n_heads_after <- sum(runs$lengths[which(runs$values == 1 & runs$lengths >= k)] - k)

    # accounting for the edge case: If the sequence ends with a streak where normally the next toss would be evaluated then one
    # too many tails were counted in "n_tails_after". In that case we can just subtract 1 from "n_tails_after"
    if (n %in% cumsum(runs$lengths)[which(runs$values == 1 & runs$lengths >= k)]) {
      n_tails_after <- n_tails_after - 1
    }
    # calculating the proprotion of heads after k successive heads
    results[sim] <- n_heads_after / (n_heads_after + n_tails_after)
  }
  # taking the mean of all simulations
  expected_heads <- mean(na.omit(results))
  return(expected_heads)
}

# the procedure is the same as in the function get_expected_heads. Therefore, comments are largely omitted
get_expected_tails <- function(n, k = 3, p = 0.5, simtosses = 1e4) {
  results <- rep(NA, simtosses)
  for (sim in 1:simtosses) {
    tosses <- rbinom(n, 1, p)
    runs <- rle(tosses)
    n_tails_after <- length(which(runs$values == 1 & runs$lengths >= k))
    n_heads_after <- sum(runs$lengths[which(runs$values == 1 & runs$lengths >= k)] - k)
    if (n %in% cumsum(runs$lengths)[which(runs$values == 1 & runs$lengths >= k)]) {
      n_tails_after <- n_tails_after - 1
    }
    results[sim] <- n_tails_after / (n_heads_after + n_tails_after)
  }
  expected_tails <- mean(na.omit(results))
  return(expected_tails)
}

get_expected_diff <- function(n, k = 3, p = 0.5, simtosses = 1e4){♦
  
  
  
}
