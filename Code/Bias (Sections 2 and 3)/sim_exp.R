# This file contains functions enables the user to simulate coin flips/random experiments with binary outcome
# for any number of trials, any streak length, and any success probability. It is really helpful to obtain a sense of intuition
# Note that the using the code provided in graphs_calc.R one can also plot a graph for the below functions.

# function to get expected heads after k successive heads in a sequence of n tosses and success probability p
expected_heads_after_heads <- function(n, k = 3, p = 0.5, simtosses = 1e4) {
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


# function to get expected heads after k successive tails in a sequence of n tosses and success probability p
# the procedure is the same as in the function get_expected_heads. Therefore, comments are largely omitted
expected_heads_after_tails <- function(n, k = 3, p = 0.5, simtosses = 1e4) {
  results <- rep(NA, simtosses)
  for (sim in 1:simtosses) {
    tosses <- rbinom(n, 1, p)
    runs <- rle(tosses)
    n_heads_after <- length(which(runs$values == 0 & runs$lengths >= k))
    n_tails_after <- sum(runs$lengths[which(runs$values == 0 & runs$lengths >= k)] - k)
    if (n %in% cumsum(runs$lengths)[which(runs$values == 0 & runs$lengths >= k)]) {
      n_heads_after <- n_heads_after - 1
    }
    results[sim] <- n_heads_after / (n_heads_after + n_tails_after)
  }
  expected_tails <- mean(na.omit(results))
  return(expected_tails)
}

# function that estimates the expected difference difference in proportions
# for large enough n this gives a very good approximation, for n < 10 not so much
get_expected_diff <- function(n, k = 3, p = 0.5, simtosses = 1e4) {
  return(expected_heads_after_heads(n, k, p, simtosses) - expected_heads_after_tails(n, k, p, simtosses))
}
