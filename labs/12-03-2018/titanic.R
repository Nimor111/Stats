library(titanic)

attach(titanic_train)

t <- table(Sex[Survived==1])
p <- prop.table(t)

barplot(p, xlab='Gender', ylab='Chance of survival')

# Women have almost twice the chance to survive that men have!

t2 <- table(Pclass[Survived==1])
p2 <- prop.table(t2)
barplot(p2, xlab='Class', ylab='Chance of survival')

# First class have a highest chance to survive.