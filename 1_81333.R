# 19.2.2018
# problem 1
# c(,,) makes a vector
x = c(8, 3, 8, 7, 15, 9, 12, 4, 9, 10, 5, 1)

# matrix 4x3
m = matrix(x, nrow=4, ncol=3)
rownames(m) = c("r1", "r2", "r3", "r4")
#m = cbind(m, c(1, 3, 5, 7))

# seq does the same as the c operator above
m = cbind(m, seq(1, by=2, length.out=4))

# sort rows by first column
m = m[order(m[,1]),]

# problem 2
library(UsingR)

# attach(homedata) TODO test out

x = homedata$y1970
y = homedata$y2000

highest_price_2000 = y[y == max(y)]
hprice_in_1970 = x[which(y == max(y))]
lowest_price_2000 = y[y == min(y)]
lprice_in_1970 = x[which(y == min(y))]

# return in vector x the items, and in vector y the indexes of the items
five_most_expensive_houses_in_2000 = sort(y, decreasing=T, index.return = T)$x[1:5]

houses_more_expensive_than_750k = y[y > 750000]
number_of_houses_more_expensive_than_750k = length(houses_more_expensive_than_750k)
# finds mean of prices of the houses more expensive than 750k in 1970
mean_of_these = mean(x[which(y>750000)])

prices_that_have_dropped_since_1970 = y[y < x]

biggest_drop_in_price = ((y - x) / x)[1:10]

# problem 3 
library(MASS)

attach(survey)

# how many people haven't inserted their sex in the survey
sum(is.na(survey$Sex))

# removes NA from cols
number_of_men = sum(survey$Sex == 'Male', na.rm = T)

men_smokers = sum(survey$Sex == 'Male' && survey$Smoke != 'Never' )
