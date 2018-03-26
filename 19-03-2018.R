# TITANIC DATASET

x <- matrix(1:4, c(2,2))
# sum rows of matrix
apply(x, 1, sum)
# sum cols
apply(x, 2, sum)

# sex and survival
t <- apply(Titanic, c(2, 4), sum)
p <- prop.table(t, 1)

barplot(p,1)
barplot(t(p), 1)

# class and survival
t2 <- apply(Titanic, c(1,4), sum)
p2 <- prop.table(t2, 1)

barplot(p2)
barplot(t(p2), 1, beside=T, legend=c("DEAD", "ALIVE"))


# FUNCTIONS IN R
myfunc = function(a, b) { a + b }

# Random values - throw a dice
# replace = T means with repetition
sample(1:6, replace=T, 10)

# custom probabilities
sample(0:1, replace = T, 10, prob=c(5/6, 1/6))

# week4 problem 1
# throw dice 100 times
sample(1:6, replace=T, 100)

dice <- function(n = 100) {
  # repeat throwing a die 100 times
  s <- sample(1:6, replace=T, n)
  # number of 6s
  return(sum(s==6))
}

throw_dice <- function(n = 10) {
  prob <- 0
  r = rep.int(0, n)
  for ( i in 1 : n ) {
    x = x + dice()
    r[i] = x / (100*i)
  }
 
  plot(1:n, r, type='l', ylim=c(1/4, 1/8))
  abline(h = 1/6, col='red')
  return(r)
}

# problem 2
birthday_problem <- function(p) {
  n <- 0
  prob <- 1
  while(prob > 1 - p ){
    prob <- prob * ((365 - n) / 365)
    n <- n + 1
  }
  
  return(n)
}

# problem 3
tennis_problem <- function(p1, p2) {}

# problem 4
# we are looking for EETET
heads_and_tails_five_times <- function() {
  # 1 is heads, 2 is tails
  s <- sample(1:2, replace=T, 5)
  return(s)
}

number_of_desired_permutation <- function(n=100) {
  count <- 0
  for(i in 1:n) {
    s <- heads_and_tails_five_times()
    if (all(s == c(1, 1, 2, 1, 2))) count <- count + 1
  }
  
  return(count);
}

required_throws <- function() {
  values <- c(0)
  n <- 1
  while(mean(values) < 1) {
    values <- append(number_of_desired_permutation(n), values)
    n <- n + 1
  }
  
  return(n)
}

mean_required_throws <- function(n = 100) {
  values <- c()
  for (i in 1:n) {
    values <- append(required_throws(), values)
  }
  
  return(mean(values))
}