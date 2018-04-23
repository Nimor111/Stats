n <- 20
x <- rnorm(n, 5, 3)

# We will check if mu is 5.2 or different than 5.2
# Hypothesis - mu = 5.2
# Alternative - mu != 5.2

m <- mean(x)
s <- sd(x)

# v = mean - 5.2 / sqrt(S / n = 20)
v <- (m - 5.2) / ( s / sqrt(n))

# student distribution
pt(v, df = n - 1)
p.v = 2 * v

# same thing
y <- t.test(x, mu = 5.2, alternative = "two.sided")

# When p-value is small - we decline the hypothesis and accept the alternative
# When it's bigger, the hypothesis might be valid

# p-value means that is the probability of the hypothesis being true

shapiro.test(x)

# this checks Ho: X is normally disributed against H1: X is not normally
# distributed

# Exercise 1
# H0 : mu = 24
# H1 : mu != 24

library(UsingR)

# sort of normal distribution
qqnorm(vacation)
qqline(vacation)

shapiro.test(vacation)

# 3% chance for 24 vacation days, we accept alternative
# With H1: mu < 24 -> 1.5%
# With H1: mu > 24 -> 98%
t.test(vacation, mu=24, alternative = "two.sided")

# Exercise 2
# 13% chance 50% to be content of product
prop.test(42, 100, 1/2, alternative = "less")

# Exercise 3
observations <- c(12.8, 3.5, 2.9, 9.4, 8.7, 0.7, 0.2, 2.8, 1.9, 2.8, 3.1, 15.8)

# H0 : talk 5 minutes
# H1 : different than 5 minutes

# Data is not normally distributed
qqnorm(observations)
shapiro.test(observations)

# We use wilcox test
# p-value = 0.5156
# We can't decline hypothesis
wilcox.test(observations, mu = 5, alternative = "greater")

# Exercise 4
boxplot(cancer)
# H0 : mu = 100
# H1 : mu < 100

# not normally distributed
shapiro.test(cancer$stomach)

# we can't decline hypothesis
wilcox.test(cancer$stomach, mu=100, alternative = "less")