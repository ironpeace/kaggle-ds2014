setwd("~/dev/kaggle/ds2014")

histories <- read.csv("train.csv")
groups <- read.csv("groups.csv")

summary(groups$state)
plot(groups$state)

summary(as.factor(groups$location))
plot(as.factor(groups$location))

summary(as.factor(groups$group_size))
plot(as.factor(groups$group_size))

summary(as.factor(groups$homeowner))

summary(groups$car_age)
hist(groups$car_age)

summary(groups$car_value)
plot(groups$car_value)

summary(as.factor(groups$risk_factor))

summary(groups$age_oldest)
hist(groups$age_oldest)

summary(groups$age_youngest)
hist(groups$age_youngest)

summary(as.factor(groups$married_couple))

summary(as.factor(groups$c_previous))

summary(as.factor(groups$duration_previous))
plot(as.factor(groups$duration_previous))

summary(groups$isSingle_state1)
summary(groups$isSingle_location)
summary(groups$isSingle_group_size)
summary(groups$isSingle_homeowner)
summary(groups$isSingle_car_age)
summary(groups$isSingle_car_value)
summary(groups$isSingle_risk_factor)
summary(groups$isSingle_age_oldest)
summary(groups$isSingle_age_youngest)
summary(groups$isSingle_married_couple)
summary(groups$isSingle_c_previous)
summary(groups$isSingle_duration_previous)

summary(groups$howPrevPolicy)
summary(as.factor(groups$howPrevPolicy))
hist(groups$howPrevPolicy)

summary(groups$count)
summary(as.factor(groups$count))
hist(groups$count)

summary(groups$minCost)
hist(groups$minCost)

summary(groups$meanCost)
hist(groups$meanCost)

summary(groups$maxCost)
hist(groups$maxCost)

# 直前見積もりを採用しなかった人
notEve <- subset(groups, howPrevPolicy != 1)
summary(as.factor(notEve$howPrevPolicy)) # 当然１がいないことを確認

# 大体７割程度の人が直前見積もりを採用

notEve$selectMin <- ifelse(notEve$finCost == notEve$minCost,1,0)
summary(as.factor(notEve$selectMin))
# 直前見積もりを採用しなかった人で、最小見積もりを採用した人は、7052/29016

notEve$selectMax <- ifelse(notEve$finCost == notEve$maxCost,1,0)
summary(as.factor(notEve$selectMax))
# 直前見積もりを採用しなかった人で、最大見積もりを採用した人は、8171/29016

notEve$selectRatio <- (notEve$finCost - notEve$minCost) / (notEve$maxCost - notEve$minCost)
summary(notEve$selectRatio)
hist(notEve$selectRatio)

# 直前見積もりを採用しなかった人で、
# 最大見積もりを採用した人は、28%
# 最小見積もりを採用した人は、24%
# 中間値の見積もりを採用した人は、48%
# 中間値のどの見積もりかは特徴が無さそう

groups2 <- groups

# 直前見積もりを採用したか、そうでないかで分類
groups2$howPrevPolicy01 <- ifelse(groups2$howPrevPolicy == 1, 1, 0)
groups2$howPrevPolicy <- NULL

# testデータで持ち得ないカラムを削除
groups2$finCost <- NULL

set.seed(12345)
trainnum <- sample(1:97009, 67906)
groups2.train <- groups2[trainnum, ]  # トレーニングデータ
validnum <- setdiff(1:97009, trainnum)
groups2.valid <- groups2[validnum, ]  # 検証データ


library(rpart)
library(partykit)


groups2.tree = rpart(howPrevPolicy01~., data=groups2.train, maxdepth=3, cp=-1)
plot(as.party(groups2.tree))
# かなりはっきりしたモデルだけど...

# source("MakeTreeDiagram.R")
# MakeTreeDiagram(groups2.tree, "groups2.tree.csv")

groups2.train.prob <- predict(groups2.tree, newdata=groups2.train)

source("calcAR.R")

calcAR(X=groups2.train.prob, 
       y=groups2.train$howPrevPolicy01, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.2056413

groups2.valid.prob <- predict(groups2.tree, newdata=groups2.valid)

calcAR(X=groups2.valid.prob, 
       y=groups2.valid$howPrevPolicy01, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.1957801

# オーバーフィッティングはしていないが、モデルの割にAR値は低い気がする…

groups2.train.prob.01 <- ifelse(groups2.train.prob > 0.72, 1, 0)
sum(groups2.train.prob.01) / 67906
 
groups2.valid.prob.01 <- ifelse(groups2.valid.prob > 0.72, 1, 0)
sum(groups2.valid.prob.01) / 29103

# 大体0.72より大きいレコードを直前見積もり採用組と判断すると
# 全体の７割くらいになりそう

# ひとまずこれで、直前見積もり採用組を予測するモデルは出来上がりとする


groups3 <- groups # 最大見積もり選択モデル作成用データ
groups3$selectMax <- ifelse(groups3$finCost == groups3$maxCost, 1, 0)

# 効いてくるのが当たり前の変数を潰しておく
groups3$finCost <- NULL
groups3$howPrevPolicy <- NULL

groups3.train <- groups3[trainnum, ]  # トレーニングデータ
groups3.valid <- groups3[validnum, ]  # 検証データ


groups3.tree = rpart(selectMax~., data=groups3.train, maxdepth=3, cp=-1)
plot(as.party(groups3.tree))

groups3.train.prob <- predict(groups3.tree, newdata=groups3.train)

calcAR(X=groups3.train.prob, 
       y=groups3.train$selectMax, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.2656169

groups3.valid.prob <- predict(groups3.tree, newdata=groups3.valid)

calcAR(X=groups3.valid.prob, 
       y=groups3.valid$selectMax, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.2604701

# オーバーフィッティングはしてなさそう。

# これで最大見積もり選択モデルの作成は完了


groups4 <- groups # 最大見積もり選択モデル作成用データ
groups4$selectMin <- ifelse(groups4$finCost == groups4$minCost, 1, 0)

# 効いてくるのが当たり前の変数を潰しておく
groups4$finCost <- NULL
groups4$howPrevPolicy <- NULL

groups4.train <- groups4[trainnum, ]  # トレーニングデータ
groups4.valid <- groups4[validnum, ]  # 検証データ

groups4.tree = rpart(selectMin~., data=groups4.train, maxdepth=3, cp=-1)
plot(as.party(groups4.tree))

groups4.train.prob <- predict(groups4.tree, newdata=groups4.train)

calcAR(X=groups4.train.prob, 
       y=groups4.train$selectMin, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.1963398

groups4.valid.prob <- predict(groups4.tree, newdata=groups4.valid)

calcAR(X=groups4.valid.prob, 
       y=groups4.valid$selectMin, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.1669494

# オーバーフィッティングはしてなさそう。

# これで最小見積もり選択モデルの作成は完了


test <- read.csv("groups.test.csv")

test.eve.prob <- predict(groups2.tree, newdata=test)
test.max.prob <- predict(groups3.tree, newdata=test)
test.min.prob <- predict(groups4.tree, newdata=test)

testlen <- 55716

test.prob <- rep(0, testlen)
test.prob.flg <- rep(0, testlen)

for(i in 1:testlen){
  if(test.eve.prob[i] > 0.58){
    test.prob[i] <- test$evePolicy[i]
    test.prob.flg[i] <- "eve"
    
  }else if(test.max.prob[i] > 0.3){
    test.prob[i] <- test$maxCostPolicy[i]
    test.prob.flg[i] <- "max"
    
  }else if(test.min.prob[i] > 0.2){
    test.prob[i] <- test$minCostPolicy[i]
    test.prob.flg[i] <- "min"
    
  }else{
    test.prob[i] <- test$evePolicy[i]
    test.prob.flg[i] <- "eve"
    
  }
}

table(test.prob.flg)

answer <- data.frame(customer_ID=test$customer_ID, plan=test.prob)

write.csv(answer, "answer.csv", row.names=F)



