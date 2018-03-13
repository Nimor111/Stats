setwd('~/code/Stats/week3')
data <- read.csv('data.txt', sep=' ')

# cov - covariation = sum(xi - mean(x))(yi - mean(y)) / (n-1)
# cor - correlation = cov / sd(x)*sd(y)
# if cor is 1 the data is linearly dependent
c <- cor(data$x, data$y)

attach(anscombe)

# for x and y calculate mean, var, cor(x,y), plot
x <- c(x1, x2, x3, x4)
y <- c(y1, y2, y3, y4)

x_mean <- mean(x1)
y_mean <- mean(y1)

var_x <- var(x)
var_y <- var(y)

cor(x1, y1)
plot(x1, y1)
plot(x2, y2)
plot(x3, y3)
plot(x4, y4) 

# identify(x3, y3, n=1)