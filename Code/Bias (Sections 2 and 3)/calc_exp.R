
update_plus <- function(dict1, dict2) {
  # reducing dict1 and dict2 to entries with positive probability
  if (any(dict1$values == 0)) {
    a <- which(dict1$values == 0)
    dict1$keys <- dict1$keys[-a]
    dict1$values <- dict1$values[-a]
  }
  if (any(dict2$values == 0)) {
    b <- which(dict2$values == 0)
    dict2$keys <- dict2$values[-b]
    dict2$values <- dict2$values[-b]
  }
  # creating and returning the combined dictionary
  if (length(intersect(dict1$keys, dict2$keys)) == 0) {
    dict3 <- list(keys = c(dict1$keys, dict2$keys), values = c(dict1$values, dict2$values))
    return(dict3)
  }
  else {
    c <- intersect(dict1$keys, dict2$keys)
    d <- match(c, dict1$keys)
    e <- match(c, dict2$keys)
    val <- dict1$values[d] + dict2$values[e]
    dict3 <- list(keys = c(c, dict1$keys[-d], dict2$keys[-e]), values = c(val, dict1$values[-d], dict2$values[-e]))
    return(dict3)
  }
}


update_power <- function(dict, m_prime, p_prime) {
  # reducing dict1 and dict2 to entries with positive probability
  if (any(dict$values == 0)) {
    a <- which(dict$values == 0)
    dict$keys <- dict$keys[-a]
    dict$values <- dict$values[-a]
  }
  for (i in 1:length(dict$keys)) {
    dict$keys[[i]] <- dict$keys[[i]] + m_prime
  }
  b <- rep(p_prime, length(dict$keys))
  dict$values <- dict$values * b
  return(dict)
}

# Note that in R matrix entries start with 1 instead of 0 like in other programming languages, i.e. Python
Count_Distribution <- function(N, k, p) {
  D <- as.list(numeric((k + 1) * (N + 1)))
  dim(D) <- c(k + 1, N + 1)
  q <- 1 - p
  for (n in 0:N) {
    L <- min(n, k)
    for (l in L:0) {
      r <- n - l
      if (r == 0) {
        D[[l + 1, r + 1]] <- list(keys = list(c(0, 0)), values = 1)
      }
      else if (r > 0) {
        if (l < k) {
          D[[l + 1, r + 1]] <- update_plus(update_power(D[[1, r]], c(0, 0), q), update_power(D[[l + 2, r]], c(0, 0), p))
        }
        else if (l == k) {
          D[[l + 1, r + 1]] <- update_plus(update_power(D[[1, r]], c(1, 0), q), update_power(D[[k + 1, r]], c(0, 1), p))
        }
      }
    }
  }
  return(D)
}

Count_Distribution(3, 1, 0.5) -> foo

# proportion of successes after k consecutive successes
exp_prop <- function(N, k, p) {
  # find out which sample of outcomes is relevant
  D <- Count_Distribution(N, k, p)
  # find relevant dictionary
  D_rel <- D[[1, N + 1]]
  # finding list Element wich has key (0,0)
  for (i in 1:length(D_rel$keys)) {
    if (D_rel$keys[[i]][1] == 0 && D_rel$keys[[i]][2] == 0) {
      a <- i
    }
    else {
      next
    }
  }
  den <- 1 - D_rel$values[a]
  num <- 0
  for (i in 1:length(D_rel$keys)) {
    if (D_rel$keys[[i]][2] == 0) {
      next
    }
    else {
      coeff <- D_rel$keys[[i]][2] / (D_rel$keys[[i]][1] + D_rel$keys[[i]][2])
      num <- num + (coeff * D_rel$values[[i]])
    }
  }
  return(num / den)
}



# proportion of successes after k consecutive failures
exp_prop2 <- function(N, k, p) {
  return(1 - exp_prop(N, k, 1 - p))
}

#exp_prop(100, 3, 0.5) - exp_prop2(100, 3, 0.5) #-0.079
#exp_prop(100, 3, 0.6) - exp_prop2(100, 3, 0.6) #-0.089
#exp_prop(200, 3, 0.5) - exp_prop2(200, 3, 0.5) #-0.037
# Note that in R matrix entries start with 1 instead of 0 like in other programming languages, i.e. Python
Count_Distribution_diff <- function(N, k, p) {
  D <- as.list(numeric((k + 1) * (k + 1) * (N + 1)))
  dim(D) <- c(k + 1, k + 1, N + 1)
  q <- 1 - p
  for (n in 0:N) {
    L <- min(n, k)
    for (l in L:0) {
      r <- n - l
      l_0 <- l
      l_1 <- 0
      if (r == 0) {
        D[[l_0 + 1, l_1 + 1, r + 1]] <- list(keys = list(c(0, 0, 0, 0)), values = 1)
      }
      else if (r > 0) {
        if (max(l_0, l_1) < k) {
          D[[l_0 + 1, l_1 + 1, r + 1]] <- update_plus(update_power(D[[l_0 + 2, 1, r]], c(0, 0, 0, 0), q), update_power(D[[1, l_1 + 2, r]], c(0, 0, 0, 0), p))
        }
        else if (l_0 == k) {
          D[[l_0 + 1, l_1 + 1, r + 1]] <- update_plus(update_power(D[[k + 1, 1, r]], c(1, 0, 0, 0), q), update_power(D[[1, 2, r]], c(0, 1, 0, 0), p))
        }
      }
      l_1 <- l
      l_0 <- 0
      if (r == 0) {
        D[[l_0 + 1, l_1 + 1, r + 1]] <- list(keys = list(c(0, 0, 0, 0)), values = 1)
      }
      else if (r > 0) {
        if (max(l_0, l_1) < k) {
          D[[l_0 + 1, l_1 + 1, r + 1]] <- update_plus(update_power(D[[l_0 + 2, 1, r]], c(0, 0, 0, 0), q), update_power(D[[1, l_1 + 2, r]], c(0, 0, 0, 0), p))
        }
        else if (l_1 == k) {
          D[[l_0 + 1, l_1 + 1, r + 1]] <- update_plus(update_power(D[[2, 1, r]], c(0, 0, 1, 0), q), update_power(D[[1, k + 1, r]], c(0, 0, 0, 1), p))
        }
      }
    }
  }
  return(D)
}


exp_diff <- function(N, k, p) {
  # find out which sample of outcomes is relevant
  D <- Count_Distribution_diff(N, k, p)
  # find relevant dictionary
  D_rel <- D[[1, 1, N + 1]]
  # finding list Element wich has key (0,0)
  a <- c()
  for (i in 1:length(D_rel$keys)) {
    if ((D_rel$keys[[i]][1] == 0 && D_rel$keys[[i]][2] == 0) || (D_rel$keys[[i]][3] == 0 && D_rel$keys[[i]][4] == 0)) {
      a <- c(a, i)
    }
    else {
      next
    }
  }
  den <- 1 - sum(D_rel$values[a])
  num1 <- 0
  for (i in 1:length(D_rel$keys)) {
    if ((D_rel$keys[[i]][2] == 0) || (D_rel$keys[[i]][3] == 0 && D_rel$keys[[i]][4] == 0)) {
      next
    }
    else {
      coeff1 <- D_rel$keys[[i]][2] / (D_rel$keys[[i]][1] + D_rel$keys[[i]][2])
      num1 <- num1 + (coeff1 * D_rel$values[[i]])
    }
  }
  num2 <- 0
  for (i in 1:length(D_rel$keys)) {
    if ((D_rel$keys[[i]][4] == 0) || (D_rel$keys[[i]][1] == 0 && D_rel$keys[[i]][2] == 0)) {
      next
    }
    else {
      coeff2 <- D_rel$keys[[i]][4] / (D_rel$keys[[i]][3] + D_rel$keys[[i]][4])
      num2 <- num2 + (coeff2 * D_rel$values[[i]])
    }
  }
  num <- num2 - num1
  return(num / den)
}










# exp_diff <- function(N,k,p){
#  q <- 1-p
#  D1 <- Count_Distribution(N, k, p)
#  D2 <- Count_Distribution(N, k, q)
#  D2_rel <- D2[[1, N+1]]
#  for(i in 1:length(D2_rel$keys)){
#    temp1 <- D2_rel$keys[[i]][1]
#    temp2 <- D2_rel$keys[[i]][2]
#    D2_rel$keys[[i]][1] <- temp2
#    D2_rel$keys[[i]][2] <- temp1
#  }

#  return(D2_rel)
# }
