# types of distributions in R
# r.. - generator of random numbers(rngesus), example rbinom x is Bi(n, p)
# d.. - random values P(X=x)
# p.. - quantiles distribution function
# Fx(q) = P(X <= q) = p - has variable q
# q.. - quantiles distribution function 
# Fx(q) = P(X <= q) = p - has variable p

#Examples
# probability one of the experiments to be successful
dbinom(1, 10, 0.5)
# prob all of the experiments to be successful
x <- dbinom(0:10, 10, 0.3)
barplot(x)

# problem 1
# Хвърлят се 30 зара. Каква е вероятността да се паднат по-малко от 5
# шестици? Сравнете теоретичната вероятност с експериментални данни. 

# x is from Bi(30, 1/6)

print(pbinom(4.99, 30, 1/6))
print(sum(dbinom(0:4, 30, 1/6)))

# 100 throws of 30 dice with prob 1/6
x <- rbinom(100, size=30, prob=1/6)
print(sum(x < 5) / 100)

# Можем да твърдим, че с вероятност 0,75 ще се паднат повече от колко шестици?
# lower.tail = F calculates P(X > x), otherwise it's P(X <= x)
print(qbinom(p=0.75, size=30, prob=1/6, lower.tail = F))

# problem 2
# Стрелец уцелва мишена с вероятност 0,2. За да спечели стрелецът
# трябва да направи три точни попадения. Каква е вероятността за това да са му
# необходими:
# а) точно 8 изстрела;
print(dbinom(size=7, prob=0.2, x=2) * 0.2)
# size is the amount of successful experiments, x is the amount of unsuccessful
print(dnbinom(x=5, size=3, prob=0.2))
# б) повече от 6 изстрела;
print(pbinom(2, 6, 0.2))
# more than 3 unsuccessful, exactly 3 successful
print(pnbinom(3, 3, 0.2, lower.tail=F))
# в) между 5 и 8 изстрела, включително?
# unsuccessful ones between 2 and 5, as in min 5 tries and max 8 tries
print(sum(dnbinom(2:5, 3, 0.2)))

# problem 3
# В урна има 7 бели и 6 черни топки. От урната последователно без
# връщане се теглят 8 топки. Нека X е броя на изтеглените бели. Направете
# 1000 симулации и по тях пресметнете: границите, в който се мени X, EX и
# DX. Намерете теоретичните стойности за EX и DX. Представете графично
# емперичното и теоритичното разпределение на X(на една графика).
# hypergeometric distribution - hyper()
# m - number of white balls, n - number of black , k - balls taken out
x <- rhyper(nn=1000, m = 7, n = 6, k = 8)
min_x <- min(x)
max_x <- max(x)

# print(min_x)
# print(max_x)
print(mean(x))
print(var(x))
# print(sd(x))

p <- dhyper(2:7, 7, 6, 8)
theoretical_mean <- sum(p * 2:7)
print(theoretical_mean)
theoretical_var <- sum(p * (2:7)^2) - theoretical_mean^2
print(theoretical_var)

dim(p) <- c(1,6) 
colnames(p) <- 2:7

p_empirical <- prop.table(table(x))
p_both <- rbind(p, p_empirical)

barplot(p_both, beside=T, legend=c('empirical', 'theoretical'))