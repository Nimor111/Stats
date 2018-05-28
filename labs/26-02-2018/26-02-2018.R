library(MASS)

attach(survey)

# Factor of data - like an enum
# use str(survey$Smoke)
# levels(survey$Smoke)

# problem 1
# P(A) = #R / #N

probability_of_regular_smoker = sum(Smoke == 'Regul', na.rm=T) / nrow(survey)

t = table(Smoke)

probability_of_regular_smoker2 = sum(t['Regul']) / nrow(survey)
p = prop.table(t)
probability_of_regular_smoker3 = sum(p['Regul'])

t2 = table(Smoke, Sex)
p2 = prop.table(t2)
probability_of_regular_male_smoker = p2['Regul', 'Male']

# P(B) = #M & R / #M
# probability_of_male_being_smoker
# sum of all cols will be 1
p3 = prop.table(t2, 2)

# P(C) = #M & R / #R
# sum of all rows will be 1
p4 = prop.table(t2, 1)

# problem 2
y = factor(Smoke, levels=levels(Smoke)[c(2, 3, 4, 1)])
t = table(y)
pie(t, col=c('green', 'red', 'blue' ,'orange'), main='SMOKERS')
barplot(t, col=c('green', 'red', 'blue', 'orange'))

t2 = table(Sex, y)
barplot(t2, col=c('green', 'red'), legend=levels(Sex), beside=T)

t2alt = table(y, Sex)
barplot(t2alt, col=c('green', 'red', 'blue', 'orange'), legend=levels(Smoke), beside=T)

t2prop = prop.table(table(Sex, y), 2)
barplot(t2prop, col=c('green', 'red'), legend=levels(Sex), beside=T)

# problem 3
# average 
# x1...xn, average is sum(xi)/n
average_height = mean(Height, na.rm = T)
median_height = median(Height, na.rm=T)

s = summary(Height)
boxplot(Height, horizontal = T)

boxplot(Height~Sex, horizontal=T)
boxplot(Height[Sex=='Male'], Height[Sex=='Female'], horizontal = T)

# removes 0.1 from both sides of the interval, if we have outlier values it will
# effectively ignore them

# type h in console for more information about it
h = hist(Height)
mean_without_outliers = mean(Height, na.rm=T, trim=0.1)

# gets out frequency density, e.g. frequency / class width
p = hist(Pulse, freq=T)

# cuts the column values into the given intervals
Age.cut <- cut(Age, c(-Inf, 20, 25, +Inf))
# without xaxt="n" it will show mathematical visualizations of the intervals
plot <- barplot(table(Age.cut), main="Age", xaxt="n")
# set names of x axis
axis(1, at=plot, labels=c('< 20', '20-25', '> 25'))