install.packages("nnet")

setwd("~/dev/kaggle/ds2014")

library(nnet)
library(rpart)
library(partykit)
source("calcAR.R")

data <- read.csv("groups.train.csv")

data.AtoG <- data.frame(data$vA,data$vB,data$vC,data$vD,data$vE,data$vF,data$vG)
cor(data.AtoG)

#            data.vA    data.vB     data.vC      data.vD   data.vE      data.vF    data.vG
# data.vA 1.00000000 0.12797398  0.16535973  0.140935348 0.3301244  0.527621446 0.09282047
# data.vB 0.12797398 1.00000000  0.06687022  0.074909683 0.3993728  0.114625994 0.03009962
# data.vC 0.16535973 0.06687022  1.00000000  0.608320440 0.1938567 -0.029173387 0.16064929
# data.vD 0.14093535 0.07490968  0.60832044  1.000000000 0.1680546 -0.008729798 0.17154204
# data.vE 0.33012440 0.39937285  0.19385666  0.168054568 1.0000000  0.169150353 0.13262351
# data.vF 0.52762145 0.11462599 -0.02917339 -0.008729798 0.1691504  1.000000000 0.10787545
# data.vG 0.09282047 0.03009962  0.16064929  0.171542041 0.1326235  0.107875455 1.00000000
# AとF、CとDがそこそこ相関ありそう

# NAがあるとそのレコードの予測値もNAになってしまうのでチェック
length(which(is.na(data$customer_ID)))
length(which(is.na(data$day)))
length(which(is.na(data$state)))
length(which(is.na(data$location)))
length(which(is.na(data$group_size)))
length(which(is.na(data$homeowner)))
length(which(is.na(data$car_age)))
length(which(is.na(data$car_value)))
length(which(is.na(data$risk_factor)))
length(which(is.na(data$age_oldest)))
length(which(is.na(data$age_youngest)))
length(which(is.na(data$married_couple)))
length(which(is.na(data$c_previous)))
length(which(is.na(data$duration_previous)))
length(which(is.na(data$vA)))
length(which(is.na(data$vB)))
length(which(is.na(data$vC)))
length(which(is.na(data$vD)))
length(which(is.na(data$vE)))
length(which(is.na(data$vF)))
length(which(is.na(data$vG)))
length(which(is.na(data$vA1)))
length(which(is.na(data$vB1)))
length(which(is.na(data$vC1)))
length(which(is.na(data$vD1)))
length(which(is.na(data$vE1)))
length(which(is.na(data$vF1)))
length(which(is.na(data$vG1)))
length(which(is.na(data$cost1)))
length(which(is.na(data$vA2)))
length(which(is.na(data$vB2)))
length(which(is.na(data$vC2)))
length(which(is.na(data$vD2)))
length(which(is.na(data$vE2)))
length(which(is.na(data$vF2)))
length(which(is.na(data$vG2)))
length(which(is.na(data$cost2)))
length(which(is.na(data$cA)))
length(which(is.na(data$cB)))
length(which(is.na(data$cC)))
length(which(is.na(data$cD)))
length(which(is.na(data$cE)))
length(which(is.na(data$cF)))
length(which(is.na(data$cG)))

data2 <- data

data2$isSingle_state <- NULL
data2$isSingle_location <- NULL
data2$isSingle_group_size <- NULL
data2$isSingle_homeowner <- NULL
data2$isSingle_car_age <- NULL
data2$isSingle_car_value <- NULL
data2$isSingle_risk_factor <- NULL
data2$isSingle_age_oldest <- NULL
data2$isSingle_age_youngest <- NULL
data2$isSingle_married_couple <- NULL
data2$isSingle_c_previous <- NULL
data2$isSingle_duration_previous <-NULL
data2$count <- NULL
data2$minCost <- NULL
data2$meanCost <- NULL
data2$maxCost <- NULL
data2$minCostPolicy <- NULL
data2$maxCostPolicy <- NULL
data2$firstPolicy <- NULL
data2$lastPolicy <- NULL
data2$daysCount <- NULL
data2$howLong <- NULL
data2$howPrevPolicy <- NULL
data2$finCost <- NULL
data2$chgOpt <- NULL
data2$howChanged <- NULL

# risk_factorの欠損値埋め
data2.4rf <- data2
data2.4rf$state <- NULL
data2.4rf$evePolicy <- NULL

data2.4rf.rpart <- rpart(as.factor(risk_factor)~., data=data2.4rf, maxdepth=5, cp=-1, minbucket=1000 )
plot(as.party(data2.4rf.rpart))
data2.4rf.pred = predict(data2.4rf.rpart, newdata=data2.4rf, type="class")

sum(ifelse(is.na(data2.4rf$risk_factor), 0, ifelse(data2.4rf$risk_factor == data2.4rf.pred, 1, 0))) / 
  sum(ifelse(is.na(data2.4rf$risk_factor),0,1))
# NAじゃないレコードでの正答率４割
# いまいち低いけどしゃーないか・・・

summary(as.factor(data2$risk_factor))
data2$risk_factor <- ifelse(is.na(data2$risk_factor), data2.4rf.pred, data2$risk_factor)
summary(as.factor(data2$risk_factor))


# c_previousの欠損値埋め
data2.4cp <- data2
data2.4cp$state <- NULL
data2.4cp$evePolicy <- NULL

data2.4cp.rpart <- rpart(as.factor(c_previous)~., data=data2.4cp, maxdepth=5, cp=-1, minbucket=1000 )
plot(as.party(data2.4cp.rpart))
data2.4cp.pred = predict(data2.4cp.rpart, newdata=data2.4cp, type="class")

sum(ifelse(is.na(data2.4cp$c_previous), 0, ifelse(data2.4cp$c_previous == data2.4cp.pred, 1, 0))) / 
  sum(ifelse(is.na(data2.4cp$c_previous),0,1))
# NAじゃないレコードでの正答率7割
# ぼちぼちかな

summary(as.factor(data2$c_previous))
data2$c_previous <- ifelse(is.na(data2$c_previous),data2.4cp.pred,data2$c_previous)
summary(as.factor(data2$c_previous))


# 直前見積もり採用フラグを立てる
data2$isEve <- ifelse(data$howPrevPolicy == 1, 1, 0)

# 州情報とのマージ
states <- read.csv("states_data.csv")
data3 <- merge(x=data2, y=states, by="state")
data3 <- data2

# トレーニングデータと検証データの作成
set.seed(12345)
trainnum <- sample(1:97009, 67906)
data.train <- data3[trainnum, ]  # トレーニングデータ
validnum <- setdiff(1:97009, trainnum)
data.valid <- data3[validnum, ]  # 検証データ

data.train.4 <- data.train
data.train.4$evePolicy <- NULL
data.train.4$vA <- NULL
data.train.4$vB <- NULL
data.train.4$vC <- NULL
data.train.4$vD <- NULL
data.train.4$vE <- NULL
data.train.4$vF <- NULL
data.train.4$vG <- NULL
data.train.4$cA <- NULL
data.train.4$cB <- NULL
data.train.4$cC <- NULL
data.train.4$cD <- NULL
data.train.4$cE <- NULL
data.train.4$cF <- NULL
data.train.4$cG <- NULL

data.valid.4 <- data.valid
data.valid.4$evePolicy <- NULL
data.valid.4$vA <- NULL
data.valid.4$vB <- NULL
data.valid.4$vC <- NULL
data.valid.4$vD <- NULL
data.valid.4$vE <- NULL
data.valid.4$vF <- NULL
data.valid.4$vG <- NULL
data.valid.4$cA <- NULL
data.valid.4$cB <- NULL
data.valid.4$cC <- NULL
data.valid.4$cD <- NULL
data.valid.4$cE <- NULL
data.valid.4$cF <- NULL
data.valid.4$cG <- NULL

# 直前見積もり採用予測モデル
#data.train.isEve.rpart = rpart(as.factor(isEve)~., data=data.train.4, maxdepth=3, cp=-1)
data.train.isEve.rpart = rpart(as.factor(isEve)~., data=data.train.4, maxdepth=5, cp=-1, minbucket=1000 )
plot(as.party(data.train.isEve.rpart))

# 直前見積もり採用予測
data.train.isEve.pred = predict(data.train.isEve.rpart, newdata=data.train)
data.valid.isEve.pred = predict(data.train.isEve.rpart, newdata=data.valid)

sum(data.train$isEve) / 67906
# 0.7018967

# これがひとまず７割くらいにならないと、直前見積もりじゃない人をまったく当てられていないことになる
sum((ifelse(data.train.isEve.pred[,2] > 0.66, 1, 0))) / 67906
sum((ifelse(data.valid.isEve.pred[,2] > 0.66, 1, 0))) / 29103


# trainデータヒット率確認
sum((ifelse(data.train.isEve.pred[,2] > 0.66, 1, 0) + data.train$isEve) == 2, 1, 0) / sum(data.train$isEve)
# 0.8070201

calcAR(X=data.train.isEve.pred[,2], y=data.train$isEve, TARGET="1", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1561848

sum((ifelse(data.valid.isEve.pred > 0.66, 1, 0) + data.valid$isEve) == 2, 1, 0) / sum(data.valid$isEve)
# 0.8033448

calcAR(X=data.valid.isEve.pred[,2], y=data.valid$isEve, TARGET="1", plotCAP=TRUE, plotpr=TRUE)
# AR : 0.1427536

# オーバーフィッティングは無さそう。

# 以降では使わないので消しておく
data.train.4$isEve <- NULL

# オプションAを予測する
data.train.4A <- data.train.4
data.train.4A$A <- data.train$vA

# 3値以上かどうか確認
summary(as.factor(data.train.4A$A))

# stateが入っているとrpartが終わらないので消しておく
data.train.4A$state <- NULL

date()
#data.train.4A.multinom <- multinom(as.factor(A)~., data = data.train.4A, MaxNWts=4000, maxit=50)
data.train.4A.rpart = rpart(as.factor(A)~., data=data.train.4A, maxdepth=5, cp=-1, minbucket=1000 )
date()

plot(as.party(data.train.4A.rpart))

#data.train.4A.pred <- predict(data.train.4A.multinom, newdata=data.train.4A, type="class")
#data.valid.4A.pred <- predict(data.train.4A.multinom, newdata=data.valid.4, type="class")
data.train.4A.pred <- predict(data.train.4A.rpart, newdata=data.train.4A, type="class")
data.valid.4A.pred <- predict(data.train.4A.rpart, newdata=data.valid.4, type="class")

# state==FL ならAはとにかく1にしちゃう
#summary(as.factor(data.train.4A.pred))
#data.train.4A.pred <- replace(data.train.4A.pred, which(data.train$state=="FL"), 1)
#summary(as.factor(data.train.4A.pred))
#
#summary(as.factor(data.valid.4A.pred))
#data.valid.4A.pred <- replace(data.valid.4A.pred, which(data.valid$state=="FL"), 1)
#summary(as.factor(data.valid.4A.pred))
# と、思ったけど、ヒット率下がったのでやめた

length(which(is.na(data.train.4A.pred)))
length(which(is.na(data.valid.4A.pred)))

sum(ifelse(data.train.4A.pred == data.train$vA, 1, 0)) / 67906
# ヒット率 0.832224

sum(ifelse(data.valid.4A.pred == data.valid$vA, 1, 0)) / 29103
# ヒット率 0.8281277


# オプションBを予測する
data.train.4B <- data.train.4
data.train.4B$B <- data.train$vB

# 3値以上かどうか確認
summary(as.factor(data.train.4B$B))

# 2値なんだからrpartでいいじゃん
data.train.4B.rpart = rpart(as.factor(B)~., data=data.train.4B, maxdepth=5, cp=-1, minbucket=1000 )
plot(as.party(data.train.4B.rpart))

data.train.4B.pred <- predict(data.train.4B.rpart, newdata=data.train.4B, type="class")
data.valid.4B.pred <- predict(data.train.4B.rpart, newdata=data.valid.4, type="class")

length(which(is.na(data.train.4B.pred)))
length(which(is.na(data.valid.4B.pred)))

sum(ifelse(data.train.4B.pred == data.train$vB, 1, 0)) / 67906
# ヒット率 0.8377757

sum(ifelse(data.valid.4B.pred == data.valid$vB, 1, 0)) / 29103
# ヒット率 0.8361337


# オプションCを予測する
data.train.4C <- data.train.4
data.train.4C$C <- data.train$vC

# 3値以上かどうか確認
summary(as.factor(data.train.4C$C))

# stateが入っているとrpart終わらないから消しておく
data.train.4C$state <- NULL

date()
#data.train.4C.multinom <- multinom(as.factor(C)~., data = data.train.4C, MaxNWts=4400, maxit=50)
data.train.4C.rpart = rpart(as.factor(C)~., data=data.train.4C, maxdepth=5, cp=-1, minbucket=1000 )
date()

plot(as.party(data.train.4C.rpart))

#data.train.4C.pred <- predict(data.train.4C.multinom, newdata=data.train.4C, type="class")
#data.valid.4C.pred <- predict(data.train.4C.multinom, newdata=data.valid.4, type="class")
data.train.4C.pred <- predict(data.train.4C.rpart, newdata=data.train.4C, type="class")
data.valid.4C.pred <- predict(data.train.4C.rpart, newdata=data.valid.4, type="class")

length(which(is.na(data.train.4C.pred)))
length(which(is.na(data.valid.4C.pred)))

sum(ifelse(data.train.4C.pred == data.train$vC, 1, 0)) / 67906
# ヒット率 0.8072924

sum(ifelse(data.valid.4C.pred == data.valid$vC, 1, 0)) / 29103
# ヒット率 0.802632



# オプションDを予測する
data.train.4D <- data.train.4
data.train.4D$D <- data.train$vD

# 3値以上かどうか確認
summary(as.factor(data.train.4D$D))

data.train.4D$state <- NULL

# Cとの相関がそこそこ高かったので、Cの正解を説明変数に入れてみる
# テストデータに対してやる時は、Cの予測結果を入れることになる
data.train.4D$pC <- data.train$vC

data.valid.4D <- data.valid
data.valid.4D$pC <- data.valid$vC
  
date()
#data.train.4D.multinom <- multinom(as.factor(D)~., data = data.train.4D, MaxNWts=4000, maxit=50)
data.train.4D.rpart = rpart(as.factor(D)~., data=data.train.4D, maxdepth=5, cp=-1, minbucket=1000 )
date()

plot(as.party(data.train.4D.rpart))

#data.train.4D.pred <- predict(data.train.4D.multinom, newdata=data.train.4D, type="class")
#data.valid.4D.pred <- predict(data.train.4D.multinom, newdata=data.valid.4, type="class")
data.train.4D.pred <- predict(data.train.4D.rpart, newdata=data.train.4D, type="class")
data.valid.4D.pred <- predict(data.train.4D.rpart, newdata=data.valid.4D, type="class")

length(which(is.na(data.train.4D.pred)))
length(which(is.na(data.valid.4D.pred)))

sum(ifelse(data.train.4D.pred == data.train$vD, 1, 0)) / 67906
# ヒット率 0.8896563

sum(ifelse(data.valid.4D.pred == data.valid$vD, 1, 0)) / 29103
# ヒット率 0.8897708


# オプションEを予測する
data.train.4E <- data.train.4
data.train.4E$E <- data.train$vE

# 3値以上かどうか確認
summary(as.factor(data.train.4E$E))

# 2値なんだからrpartでいいじゃん
data.train.4E.rpart = rpart(as.factor(E)~., data=data.train.4E, maxdepth=5, cp=-1, minbucket=1000 )
plot(as.party(data.train.4E.rpart))

data.train.4E.pred <- predict(data.train.4E.rpart, newdata=data.train.4E, type="class")
data.valid.4E.pred <- predict(data.train.4E.rpart, newdata=data.valid.4, type="class")

length(which(is.na(data.train.4E.pred)))
length(which(is.na(data.valid.4E.pred)))

sum(ifelse(data.train.4E.pred == data.train$vE, 1, 0)) / 67906
# ヒット率 0.837599

sum(ifelse(data.valid.4E.pred == data.valid$vE, 1, 0)) / 29103
# ヒット率 0.8387451



# オプションFを予測する
data.train.4F <- data.train.4
data.train.4F$F <- data.train$vF

# 3値以上かどうか確認
summary(as.factor(data.train.4F$F))

data.train.4F$state <- NULL

# Aとの相関がそこそこ高かったので、Aの予測も説明変数に加えてみる
# テストデータに対してやる時は、Aの予測結果を入れることになる
data.train.4F$pA <- data.train$vA

data.valid.4F <- data.valid
data.valid.4F$pA <- data.valid$vA



date()
#data.train.4F.multinom <- multinom(as.factor(F)~., data = data.train.4F, MaxNWts=4400, maxit=50)
data.train.4F.rpart = rpart(as.factor(F)~., data=data.train.4F, maxdepth=5, cp=-1, minbucket=1000 )
date()

plot(as.party(data.train.4F.rpart))

#data.train.4F.pred <- predict(data.train.4F.multinom, newdata=data.train.4F, type="class")
#data.valid.4F.pred <- predict(data.train.4F.multinom, newdata=data.valid.4, type="class")
data.train.4F.pred <- predict(data.train.4F.rpart, newdata=data.train.4F, type="class")
data.valid.4F.pred <- predict(data.train.4F.rpart, newdata=data.valid.4F, type="class")

length(which(is.na(data.train.4F.pred)))
length(which(is.na(data.valid.4F.pred)))

sum(ifelse(data.train.4F.pred == data.train$vF, 1, 0)) / 67906
# ヒット率 0.8445793

sum(ifelse(data.valid.4F.pred == data.valid$vF, 1, 0)) / 29103
# ヒット率 0.843109



# オプションGを予測する
data.train.4G <- data.train.4
data.train.4G$G <- data.train$vG

# 3値以上かどうか確認
summary(as.factor(data.train.4G$G))

data.train.4G$state <- NULL

date()
#data.train.4G.multinom <- multinom(as.factor(G)~., data = data.train.4G, MaxNWts=4400, maxit=50)
#data.train.4G.rpart = rpart(as.factor(G)~., data=data.train.4G, maxdepth=3, cp=-1)
data.train.4G.rpart = rpart(as.factor(G)~., data=data.train.4G, maxdepth=5, cp=-1, minbucket=1000 )
date()

plot(as.party(data.train.4G.rpart))

#data.train.4G.pred <- predict(data.train.4G.multinom, newdata=data.train.4G, type="class")
#data.valid.4G.pred <- predict(data.train.4G.multinom, newdata=data.valid.4, type="class")
data.train.4G.pred <- predict(data.train.4G.rpart, newdata=data.train.4G, type="class")
data.valid.4G.pred <- predict(data.train.4G.rpart, newdata=data.valid.4, type="class")

length(which(is.na(data.train.4G.pred)))
length(which(is.na(data.valid.4G.pred)))

# state==FL ならGはとにかく3にしちゃう
#summary(as.factor(data.train.4G.pred))
#data.train.4G.pred <- replace(data.train.4G.pred, which(data.train$state=="FL"), 3)
#summary(as.factor(data.train.4G.pred))
#
#summary(as.factor(data.valid.4G.pred))
#data.valid.4G.pred <- replace(data.valid.4G.pred, which(data.valid$state=="FL"), 3)
#summary(as.factor(data.valid.4G.pred))
# あんまり意味無し。精度下がる。


sum(ifelse(data.train.4G.pred == data.train$vG, 1, 0)) / 67906
# ヒット率 0.747445

sum(ifelse(data.valid.4G.pred == data.valid$vG, 1, 0)) / 29103
# ヒット率 0.7427069




data.train.plan <- paste(data.train$vA,data.train$vB,data.train$vC,data.train$vD,data.train$vE,data.train$vF,data.train$vG,sep="")

data.train.plan.pred <- paste(data.train.4A.pred,data.train.4B.pred,data.train.4C.pred,data.train.4D.pred,data.train.4E.pred,data.train.4F.pred,data.train.4G.pred,sep="")

sum(ifelse(data.train.plan == data.train.plan.pred, 1, 0)) / 67906
# ヒット率 0.3873001

data.valid.plan <- paste(data.valid$vA,data.valid$vB,data.valid$vC,data.valid$vD,data.valid$vE,data.valid$vF,data.valid$vG, sep="")

data.valid.plan.pred <- paste(data.valid.4A.pred,data.valid.4B.pred,data.valid.4C.pred,data.valid.4D.pred,data.valid.4E.pred,data.valid.4F.pred,data.valid.4G.pred, sep="")

sum(ifelse(data.valid.plan == data.valid.plan.pred, 1, 0)) / 29103
# ヒット率 0.3844964

# ニアミスが多いのではないかと想定して、ニアミス度合いを確認する

data.train.4A.pred.wrong <- ifelse(data.train.4A.pred != data.train$vA, 1, 0)
data.train.4B.pred.wrong <- ifelse(data.train.4B.pred != data.train$vB, 1, 0)
data.train.4C.pred.wrong <- ifelse(data.train.4C.pred != data.train$vC, 1, 0)
data.train.4D.pred.wrong <- ifelse(data.train.4D.pred != data.train$vD, 1, 0)
data.train.4E.pred.wrong <- ifelse(data.train.4E.pred != data.train$vE, 1, 0)
data.train.4F.pred.wrong <- ifelse(data.train.4F.pred != data.train$vF, 1, 0)
data.train.4G.pred.wrong <- ifelse(data.train.4G.pred != data.train$vG, 1, 0)

data.train.pred.howWrong <- data.train.4A.pred.wrong + 
                                  data.train.4B.pred.wrong + 
                                  data.train.4C.pred.wrong + 
                                  data.train.4D.pred.wrong + 
                                  data.train.4E.pred.wrong + 
                                  data.train.4F.pred.wrong + 
                                  data.train.4G.pred.wrong

hist(data.train.pred.howWrong)
pie(table(data.train.pred.howWrong))

sum(ifelse(data.train.pred.howWrong < 3,1,0)) / 67906
# 0.8374812

sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4A.pred.wrong == 1, 1, 0), 0))
sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4B.pred.wrong == 1, 1, 0), 0))
sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4C.pred.wrong == 1, 1, 0), 0))
sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4D.pred.wrong == 1, 1, 0), 0))
sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4E.pred.wrong == 1, 1, 0), 0))
sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4F.pred.wrong == 1, 1, 0), 0))
sum(ifelse(data.train.pred.howWrong==1, ifelse(data.train.4G.pred.wrong == 1, 1, 0), 0))
# １個間違えのニアミスの中で、G間違えが妙に多い。
# そもそもGの正答率が低いから当たり前か…


data.valid.4A.pred.wrong <- ifelse(data.valid.4A.pred != data.valid$vA, 1, 0)
data.valid.4B.pred.wrong <- ifelse(data.valid.4B.pred != data.valid$vB, 1, 0)
data.valid.4C.pred.wrong <- ifelse(data.valid.4C.pred != data.valid$vC, 1, 0)
data.valid.4D.pred.wrong <- ifelse(data.valid.4D.pred != data.valid$vD, 1, 0)
data.valid.4E.pred.wrong <- ifelse(data.valid.4E.pred != data.valid$vE, 1, 0)
data.valid.4F.pred.wrong <- ifelse(data.valid.4F.pred != data.valid$vF, 1, 0)
data.valid.4G.pred.wrong <- ifelse(data.valid.4G.pred != data.valid$vG, 1, 0)

data.valid.pred.howWrong <- data.valid.4A.pred.wrong + 
                            data.valid.4B.pred.wrong + 
                            data.valid.4C.pred.wrong + 
                            data.valid.4D.pred.wrong + 
                            data.valid.4E.pred.wrong + 
                            data.valid.4F.pred.wrong + 
                            data.valid.4G.pred.wrong

hist(data.valid.pred.howWrong)
pie(table(data.valid.pred.howWrong))

sum(ifelse(data.valid.pred.howWrong < 3,1,0)) / 29103
# 0.8167543
# validを見ても同様のことが言える。

# 間違ったオプションが、もし顧客にとって悩んでいないオプションであれば、
# テストデータの最初のオプションを採用してしまえばいいよね？という仮説を立ててみる

# 間違ったものが、顧客の悩み対象オプションじゃないかどうかを調べる

# 予測が間違ったもののうち、オプション変更していない顧客がどれだけいるか
sum(ifelse((data.train.4A.pred.wrong + ifelse(data.train$cA == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4A.pred.wrong)
sum(ifelse((data.train.4B.pred.wrong + ifelse(data.train$cB == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4B.pred.wrong)
sum(ifelse((data.train.4C.pred.wrong + ifelse(data.train$cC == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4C.pred.wrong)
sum(ifelse((data.train.4D.pred.wrong + ifelse(data.train$cD == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4D.pred.wrong)
sum(ifelse((data.train.4E.pred.wrong + ifelse(data.train$cE == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4E.pred.wrong)
sum(ifelse((data.train.4F.pred.wrong + ifelse(data.train$cF == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4F.pred.wrong)
sum(ifelse((data.train.4G.pred.wrong + ifelse(data.train$cG == 0, 1, 0)) == 2, 1, 0)) / sum(data.train.4G.pred.wrong)
# 予測が間違ったオプションはほとんど悩んでいる
# 悩ましいものなので、予測も間違う。そりゃそうだ。
# いかにして、間違いやすい傾向を他の手段で予測することができるか…



sum(ifelse((data.valid.4A.pred.wrong + ifelse(data.valid$cA == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4A.pred.wrong)
sum(ifelse((data.valid.4B.pred.wrong + ifelse(data.valid$cB == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4B.pred.wrong)
sum(ifelse((data.valid.4C.pred.wrong + ifelse(data.valid$cC == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4C.pred.wrong)
sum(ifelse((data.valid.4D.pred.wrong + ifelse(data.valid$cD == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4D.pred.wrong)
sum(ifelse((data.valid.4E.pred.wrong + ifelse(data.valid$cE == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4E.pred.wrong)
sum(ifelse((data.valid.4F.pred.wrong + ifelse(data.valid$cF == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4F.pred.wrong)
sum(ifelse((data.valid.4G.pred.wrong + ifelse(data.valid$cG == 0, 1, 0)) == 2, 1, 0)) / sum(data.valid.4G.pred.wrong)
# 予測が間違ったオプションはほぼ全て悩んでいる

# 特に案浮かばず…

# トータルでのモデル適用をやってみる
answer.train <- data.frame(customer_ID=data.train$customer_ID)

# 直前見積もりplanを作成
data.train.plan.eve <- substring(data.train$evePolicy,2,8)

# 直前見積もり予測先であれば、直前見積もりplanを採用し、さもなければオプション毎予測モデルを適用
answer.train$plan <- ifelse(data.train.isEve.pred[,2] > 0.5, data.train.plan.eve, data.train.plan.pred)
sum(ifelse(data.train.plan == answer.train$plan, 1, 0)) / 67906
# 0.6875681


#直前見積もり採用モデルが正解していて、オプション毎予測モデルが外れている件数
sum(ifelse(data.train.isEve.pred[,2] > 0.5, 
       ifelse(data.train$isEve==1, 
              ifelse(data.train.plan != data.train.plan.pred, 1, 0), 0), 0)) / 67906
# 0.3312815

#直前見積もり採用モデルが正解していて、オプション毎予測モデルも正解している件数
sum(ifelse(data.train.isEve.pred[,2] > 0.5, 
           ifelse(data.train$isEve==1, 
                  ifelse(data.train.plan == data.train.plan.pred, 1, 0), 0), 0)) / 67906
# 0.3706153

#直前見積もり採用モデルが外れていて、オプション毎予測モデルが正解している件数
sum(ifelse(data.train.isEve.pred[,2] > 0.5, 
           ifelse(data.train$isEve==0, 
                  ifelse(data.train.plan == data.train.plan.pred, 1, 0), 0), 0)) / 67906
# 0.01979207

#直前見積もり採用モデルが外れていて、オプション毎予測モデルも外れている件数
sum(ifelse(data.train.isEve.pred[,2] > 0.5, 
           ifelse(data.train$isEve==0, 
                  ifelse(data.train.plan != data.train.plan.pred, 1, 0), 0), 0)) / 67906
# 0.2783112

# 直前見積もり採用モデルが外れていて、オプション毎予測モデルが正解している件数が多いと嬉しかったが、
# 残念な結果に。


0.3312815 + 0.3706153 + 0.01979207 + 0.2783112
# ちゃんと１になる


# validデータでもやってみる
answer.valid <- data.frame(customer_ID=data.valid$customer_ID)

# 直前見積もりplanを作成
data.valid.plan.eve <- substring(data.valid$evePolicy,2,8)

# 直前見積もり予測先であれば、直前見積もりplanを採用し、さもなければオプション毎予測モデルを適用
answer.valid$plan <- ifelse(data.valid.isEve.pred[,2] > 0.5, data.valid.plan.eve, data.valid.plan.pred)
sum(ifelse(data.valid.plan == answer.valid$plan, 1, 0)) / 29103
# 0.6849466




# さていよいよテストデータに対してやってみる

test <- read.csv("groups.test.csv")
test <- merge(x=test, y=states, by="state")

test$isSingle_state <- NULL
test$isSingle_location <- NULL
test$isSingle_group_size <- NULL
test$isSingle_homeowner <- NULL
test$isSingle_car_age <- NULL
test$isSingle_car_value <- NULL
test$isSingle_risk_factor <- NULL
test$isSingle_age_oldest <- NULL
test$isSingle_age_youngest <- NULL
test$isSingle_married_couple <- NULL
test$isSingle_c_previous <- NULL
test$isSingle_duration_previous <-NULL
test$count <- NULL
test$minCost <- NULL
test$meanCost <- NULL
test$maxCost <- NULL
test$minCostPolicy <- NULL
test$maxCostPolicy <- NULL
#test$evePolicy <- NULL
test$firstPolicy <- NULL
test$lastPolicy <- NULL
test$daysCount <- NULL
test$howLong <- NULL
test$howPrevPolicy <- NULL
test$finCost <- NULL
test$chgOpt <- NULL
test$howChanged <- NULL

length(which(is.na(test$customer_ID)))
length(which(is.na(test$day)))
length(which(is.na(test$state)))
length(which(is.na(test$location))) # NA有
length(which(is.na(test$group_size)))
length(which(is.na(test$homeowner)))
length(which(is.na(test$car_age)))
length(which(is.na(test$car_value)))
length(which(is.na(test$risk_factor))) # NA有
length(which(is.na(test$age_oldest)))
length(which(is.na(test$age_youngest)))
length(which(is.na(test$age_oldest)))
length(which(is.na(test$married_couple)))
length(which(is.na(test$c_previous)))
length(which(is.na(test$duration_previous)))
length(which(is.na(test$vA)))
length(which(is.na(test$vB)))
length(which(is.na(test$vC)))
length(which(is.na(test$vD)))
length(which(is.na(test$vE)))
length(which(is.na(test$vF)))
length(which(is.na(test$vG)))
length(which(is.na(test$vA1)))
length(which(is.na(test$vB1)))
length(which(is.na(test$vC1)))
length(which(is.na(test$vD1)))
length(which(is.na(test$vE1)))
length(which(is.na(test$vF1)))
length(which(is.na(test$vG1)))
length(which(is.na(test$vA2)))
length(which(is.na(test$vB2)))
length(which(is.na(test$vC2)))
length(which(is.na(test$vD2)))
length(which(is.na(test$vE2)))
length(which(is.na(test$vF2)))
length(which(is.na(test$vG2)))
length(which(is.na(test$cA)))
length(which(is.na(test$cB)))
length(which(is.na(test$cC)))
length(which(is.na(test$cD)))
length(which(is.na(test$cE)))
length(which(is.na(test$cF)))
length(which(is.na(test$cG)))

#欠損値埋め(risk_factor)
test.rf.pred = predict(data2.4rf.rpart, newdata=test, type="class")
sum(ifelse(is.na(test$risk_factor), 0, ifelse(test$risk_factor == test.rf.pred, 1, 0))) / 
  sum(ifelse(is.na(test$risk_factor),0,1))
# NAじゃないレコードでの正答率４割
# いまいち低いけどしゃーないか・・・

summary(as.factor(test$risk_factor))
test$risk_factor <- ifelse(is.na(test$risk_factor), test.rf.pred, test$risk_factor)
summary(as.factor(test$risk_factor))


#欠損値埋め(c_previous)
test.cp.pred = predict(data2.4cp.rpart, newdata=test, type="class")
sum(ifelse(is.na(test$c_previous), 0, ifelse(test$c_previous == test.cp.pred, 1, 0))) / 
  sum(ifelse(is.na(test$c_previous),0,1))
# NAじゃないレコードでの正答率7割

summary(as.factor(test$c_previous))
test$c_previous <- ifelse(is.na(test$c_previous), test.cp.pred, test$c_previous)
summary(as.factor(test$c_previous))

#欠損値埋め(c_previous)
# 数が少ないから適当でいいや
test$location <- ifelse(is.na(test$location),11179,test$location)

# car_value が空のところがあるので、一旦適当な値で埋める
summary(as.factor(test$car_value))
length(which(is.na(data$car_value)))
sum(ifelse(test$car_value=="",1,0))
test$car_value <- replace(test$car_value, which(test$car_value==""), "e")
summary(as.factor(test$car_value))

# 直前見積もり予測モデルを適用
test.isEve.pred = predict(data.train.isEve.rpart, newdata=test)

hist(test.isEve.pred[,2])
summary(test.isEve.pred[,2])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.5445  0.6656  0.7009  0.7002  0.7403  0.8327 

test.4 <- test
test.4$vA <- NULL
test.4$vB <- NULL
test.4$vC <- NULL
test.4$vD <- NULL
test.4$vE <- NULL
test.4$vF <- NULL
test.4$vG <- NULL
test.4$cA <- NULL
test.4$cB <- NULL
test.4$cC <- NULL
test.4$cD <- NULL
test.4$cE <- NULL
test.4$cF <- NULL
test.4$cG <- NULL

test.4A <- test.4
test.4A$A <- test$vA
test.4A.pred <- predict(data.train.4A.rpart, newdata=test.4A, type="class")
test.4A.pred <- ifelse(test$state=="FL", "1", test.4A.pred)

length(which(is.na(test.4A.pred)))

test.4B <- test.4
test.4B$B <- test$vB
test.4B.pred <- predict(data.train.4B.rpart, newdata=test.4B, type="class")
length(which(is.na(test.4B.pred)))

test.4C <- test.4
test.4C$C <- test$vC
test.4C.pred <- predict(data.train.4C.rpart, newdata=test.4C, type="class")
length(which(is.na(test.4C.pred)))

test.4D <- test.4
test.4D$D <- test$vD
test.4D$pC <- as.numeric(test.4C.pred)
test.4D.pred <- predict(data.train.4D.rpart, newdata=test.4D, type="class")
length(which(is.na(test.4D.pred)))

test.4E <- test.4
test.4E$E <- test$vE
test.4E.pred <- predict(data.train.4E.rpart, newdata=test.4E, type="class")
length(which(is.na(test.4E.pred)))

test.4F <- test.4
test.4F$F <- test$vF
test.4F$pA <- as.numeric(test.4A.pred)
test.4F.pred <- predict(data.train.4F.rpart, newdata=test.4F, type="class")
length(which(is.na(test.4F.pred)))

test.4G <- test.4
test.4G$G <- test$vG
test.4G.pred <- predict(data.train.4G.rpart, newdata=test.4G, type="class")
length(which(is.na(test.4G.pred)))


test.plan.eve <- substring(test$evePolicy,2,8)

test.plan.pred <- paste(test.4A.pred, test.4B.pred, test.4C.pred,
                        test.4D.pred, test.4E.pred, test.4F.pred,
                        test.4G.pred, sep="")

answer.test <- data.frame(customer_ID=test$customer_ID)
answer.test$plan <- ifelse(test.isEve.pred[,2] > 0.5, test.plan.eve, test.plan.pred)

# 直前見積もり採用モデルが何割適用されたかを確認
sum(ifelse(test.isEve.pred[,2] > 0.5, 1, 0)) / 55716

write.csv(answer.test, "answer_20140506_2.csv", quote=FALSE, row.names=FALSE)

# 結局、全レコード直前見積もり採用モデルのなのにベンチマーク超えした…
# ベンチマークよりも、0.00006ポイントアップ。３〜４レコード分改善した。
# 直前見積もりプランの選択ロジックがいつの間にか改善しただけ？？？謎。





sum(ifelse(test.plan.eve==test.plan.pred, 1, 0)) / 55716
# 直前見積もりプランと、予測モデルプランとが一致しているのは、１割以下

sum(ifelse(test.plan.eve == paste(test$vA, test$vB, test$vC, test$vD, test$vE, test$vF, test$vG, sep=""),1,0)) / 55716
# 念のため、vAとかとの合体が、evePolicyと一致するかを確認。大丈夫だった。

test.4A.pred.diff <- ifelse(test.4A.pred != test$vA, 1, 0)
test.4B.pred.diff <- ifelse(test.4B.pred != test$vB, 1, 0)
test.4C.pred.diff <- ifelse(test.4C.pred != test$vC, 1, 0)
test.4D.pred.diff <- ifelse(test.4D.pred != test$vD, 1, 0)
test.4E.pred.diff <- ifelse(test.4E.pred != test$vE, 1, 0)
test.4F.pred.diff <- ifelse(test.4F.pred != test$vF, 1, 0)
test.4G.pred.diff <- ifelse(test.4G.pred != test$vG, 1, 0)

test.pred.howDiff <- test.4A.pred.diff + 
  test.4B.pred.diff + 
  test.4C.pred.diff + 
  test.4D.pred.diff + 
  test.4E.pred.diff + 
  test.4F.pred.diff + 
  test.4G.pred.diff

hist(test.pred.howDiff)
pie(table(test.pred.howDiff))
# 直前見積もりから予測モデルが相違しているのは、多くが１個だった。
