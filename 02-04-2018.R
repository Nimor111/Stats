# дискретни разпределения - крайно множество от стойности, вероятност да се попадне
# в точка

# непрекъснати разпределения - функция на плътност - вероятността е стойността на 
# функцията на плътност в точката

# problem 1
# N(x, y) - expectation, dispersion
# rnorm(100, expectation, sd)

# generate 100 values with normal distribution
x <- rnorm(10000, 5, sqrt(2))
hist(x, probability = T)
lines(density(x))

# theoretical probability
s <- seq(0, 9, 0.01)
lines(s, dnorm(s, 5, sqrt(2)), col='red')

# is the distribution symmetric?
# normal distribution is symmetric
boxplot(x, horizontal = T)

# tails? heavy and light tails. 
# heavy tails - higher probability of outliers
# light tails - lower probability of outliers
# modality

# is a distribution normal? how to find out? find quantiles to find out!
# take a number of the empiric curve and compare it to the theoretical curve
# y = (X - m) / s is from N(0, 1)

# generate random probabilities
p <- ppoints(100)
# theoretical quantiles
qt <- qnorm(p)

# emirical quantiles
qx <- quantile(x, p)
# qqplot
plot(qt, qx)

# check if values are normally distributed
qqnorm(x)
# show the line we are going for
qqline(x)

# x is from U(1, 5)
# Uniform distribution ( равномерно разпределение )
# choosing a random number from 1 and 5
y <- runif(100, 1, 5)
hist(y, probability = T)

# symmetric and uniform distribution has light tails
screen(2)
boxplot(y, horizontal = T)

# N.B. how to split screen e.g. have a couple of plots at the same time
# split.screen(c(2, 1))

# x is from Ex(3)
# exponential distribution
xe <- rexp(100, 3)
hist(xe, probability = T)

# not symmetric, heavy tails
boxplot(xe, horizontal = T)

# not normally distributed
qqnorm(xe)
qqline(xe)

# x is from Г(5, 1) - gamma distribution
# x from χ(5) - chi square distribution
# x from t(5) - student distribution

# mixed distribution
x1 <- rnorm(100, 1, sqrt(2))
x2 <- rnorm(100, 5, sqrt(2))

xm <- c(x1, x2)
hist(xm, probability = T)
# bimodal distribution
lines(density(xm))

boxplot(xm, horizontal = T)

# problem 2
# with uniform distribution
sum_random_variables <- function(n = 100) {
  sum <- rep.int(0, 100)
  for ( i in 1:n ) {
    sum <- sum + runif(100, 1, 5)
  }
  
  return(sum)
}

# the sum of any independent random variables is a normal distribution
# central limit theorem
y <- sum_random_variables()
hist(y, probability = T)
boxplot(y, horizontal = T)
qqnorm(y)
qqline(y)

# problem 3
library(UsingR)

attach(babies)

qqnorm(wt)
qqline(wt)

boxplot(wt, horizontal = T)
hist(wt, probability = T)
lines(density(wt))

# exec.pay
qqnorm(exec.pay)
qqline(exec.pay)

boxplot(exec.pay, horizontal = T)
hist(exec.pay, probability = T)

# let's see the log values, log-normal distribution
# log0 = -Inf we take only the positive values
x <- log(exec.pay[exec.pay > 0])
hist(x, probability = T)
lines(density(x))

qqnorm(x)
qqline(x)

# problem 4
# X is from N(25, 36)
melons_third_world_quality <- function() {
  return(pnorm(20, 25, 6))
}

# qnorm works from -Inf to the quantile value
dims_first_quality <- function() {
  third_quality <- melons_third_world_quality()
  return(qnorm((1 - third_quality) / 2 + third_quality, 25, 6))
}