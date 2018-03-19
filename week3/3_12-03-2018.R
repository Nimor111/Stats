# X1...XN
# Xmean ( X черта )
# Sn^2 - dispersion = Sum(xi - xmean)^2 / (n - 1)
# Sn - standard deviation, a.k.a sqrt of dispersion

library(MASS)

attach(survey)

avg_height <- mean(Height, na.rm=T)
standard_deviation_height <-
  sqrt(sum((Height - avg_height)^2, na.rm=T) / sum(!is.na(Height)))
# var(X) calculates dispersion
# sd(X) calculates standard deviation

# returns a factor
is_in_one_standard_deviation <- cut(Height,
                                    c(0,
                                      avg_height - standard_deviation_height,
                                      avg_height + standard_deviation_height,
                                      max(Height, na.rm=T)))

# returns number of elements in each level of the factor
t <- table(is_in_one_standard_deviation)

prop <- prop.table(t)

is_in_two_sd <- cut(Height, c(0, avg_height - 2*standard_deviation_height,
                              avg_height + 2*standard_deviation_height,
                              max(Height, na.rm=T)))
t2 <- table(is_in_two_sd)
prop2 <- prop.table(t2)

is_in_three_sd <- cut(Height, c(0, avg_height - 3*standard_deviation_height,
                              avg_height + 3*standard_deviation_height,
                              max(Height, na.rm=T)))
t3 <- table(is_in_three_sd)
prop3 <- prop.table(t3)

# get the men without the na-s
men <- Height[Sex =='Male' & !is.na(Height) & !is.na(Sex)]
women <- Height[Sex == 'Female' & !is.na(Height) & !is.na(Sex)]

# boxplot(men, horizontal = T)
# how wide it is - dispersion
# line - median
boxplot(men, women, horizontal = T)

# histogram
h <- hist(Height, freq=T)
# TODO why does this not work
br <- h$breaks
new_br <- c(br[1:3], 162.5, c(br[4:8], 200))
h <- hist(Height, breaks = new_br)
