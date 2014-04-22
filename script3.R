setwd("~/dev/kaggle/ds2014")

train <- read.csv("groups.train.csv")

cor(train$count, train$howChanged)
plot(train$count, train$howChanged)

hist(train$count)
hist(train$howChanged)
table(train$howChanged)

cor(train$count, train$howLong)
plot(train$count, train$howLong)

train.chg1 <- subset(train, train$howChanged == 1)
train.chg2 <- subset(train, train$howChanged == 2)

plot(train.chg1$chgOpt)

summary(train.chg1$chgOpt)
summary(train.chg2$chgOpt)

summary(as.factor(train$vG))
plot(as.factor(train$vG))
plot(as.factor(train.chg1$vG))

train$isA <- ifelse(train$cA > 0, 1, 0)
train$isB <- ifelse(train$cB > 0, 1, 0)
train$isC <- ifelse(train$cC > 0, 1, 0)
train$isD <- ifelse(train$cD > 0, 1, 0)
train$isE <- ifelse(train$cE > 0, 1, 0)
train$isF <- ifelse(train$cF > 0, 1, 0)
train$isG <- ifelse(train$cG > 0, 1, 0)

sum(train$isA) / 97009
sum(train$isB) / 97009
sum(train$isC) / 97009
sum(train$isD) / 97009
sum(train$isE) / 97009
sum(train$isF) / 97009
sum(train$isG) / 97009

sum(ifelse(train$howPrevPolicy==1, 1, 0)) / 97009

train.notEve <- subset(train, train$howPrevPolicy > 1)
sum(ifelse(train.notEve$howChanged == 1,1,0)) / 29016
train.notEve.chg1 <- subset(train.notEve, train.notEve$howChanged == 1)
sum(ifelse(train.notEve.chg1$chgOpt == "c0000001",1,0)) / 3829

  
  