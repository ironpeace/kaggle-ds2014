setwd("~/dev/kaggle/ds2014")

histories <- read.csv("train.csv")
groups <- read.csv("groups.csv")

summary(as.factor(groups$day))
hist(groups$day)

summary(groups$state)
plot(groups$state)

summary(groups$day)
plot(groups$day)

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

summary(groups$isSingle_state)
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

summary(as.factor(groups$howPrevPolicy))
hist(groups$howPrevPolicy)

# 1以外、つまり直前見積もり採用以外は思った以上に偏りが少ない

summary(groups$count)
summary(as.factor(groups$count))
hist(groups$count)

summary(groups$minCost)
hist(groups$minCost)

summary(groups$meanCost)
hist(groups$meanCost)

summary(groups$maxCost)
hist(groups$maxCost)

summary(groups$daysCount)
hist(groups$daysCount)

summary(groups$howLong)
hist(groups$howLong)

# 直前見積もりを採用しなかった人
notEve <- subset(groups, howPrevPolicy != 1)
summary(as.factor(notEve$howPrevPolicy)) # 当然１がいないことを確認

# 大体７割程度の人が直前見積もりを採用

whichSelected <- ifelse(groups$finCost < groups$minCost, "underMin",
                  ifelse(groups$finCost == groups$minCost, "eqMin",
                  ifelse(groups$finCost == groups$maxCost, "eqMax",
                  ifelse(groups$finCost > groups$maxCost, "overMax", "mid"))))

table(whichSelected)

# eqMax    eqMin      mid  overMax underMin 
# 15741     9623    44339    15377    11929 
#   16%      10%      46%      16%      12%

whichSelectedInNotEve <- ifelse(notEve$finCost < notEve$minCost, "underMin",
                        ifelse(notEve$finCost == notEve$minCost, "eqMin",
                        ifelse(notEve$finCost == notEve$maxCost, "eqMax",
                        ifelse(notEve$finCost > notEve$maxCost, "overMax", "mid"))))

table(whichSelectedInNotEve)

# eqMax    eqMin      mid  overMax underMin 
#   899      735    13812     7253     6317 
#    3%     2.5%      48%      25%      22%

# 直前見積もりを採用しなかった人は、
# 最大見積もりか、最小見積もりかのどちらかを選択するかと
# 思ったが、それは直前見積もりを採用しなかった人の5.5%しかいないので
# あては外れた


hist(notEve$howPrevPolicy)
sum(ifelse(notEve$howPrevPolicy == notEve$count, 1, 0)) / 29016
# 0.8628343

# 直前見積もりを採用しなかった人は
# 最初の見積もりを最終的に選択する可能性が極めて高いことが分かる


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


groups3 <- groups # 最初の見積もり選択モデル作成用データ
groups3$selectFirst <- ifelse(groups3$howPrevPolicy == groups3$count, 1, 0)

# 効いてくるのが当たり前の変数を潰しておく
groups3$finCost <- NULL
groups3$howPrevPolicy <- NULL

groups3.train <- groups3[trainnum, ]  # トレーニングデータ
groups3.valid <- groups3[validnum, ]  # 検証データ


groups3.tree = rpart(selectFirst~., data=groups3.train, maxdepth=3, cp=-1)
plot(as.party(groups3.tree))

groups3.train.prob <- predict(groups3.tree, newdata=groups3.train)

calcAR(X=groups3.train.prob, 
       y=groups3.train$selectFirst,
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.2778828

groups3.valid.prob <- predict(groups3.tree, newdata=groups3.valid)

calcAR(X=groups3.valid.prob, 
       y=groups3.valid$selectFirst, 
       TARGET="1", 
       plotCAP=TRUE, 
       plotpr=TRUE)

# AR : 0.2684538

# オーバーフィッティングはしてなさそう。

# これで最初の見積もり選択モデルの作成は完了


test <- read.csv("groups.test.csv")

test.eve.prob <- predict(groups2.tree, newdata=test)
test.1st.prob <- predict(groups3.tree, newdata=test)

testlen <- 55716

test.prob <- rep(0, testlen)
test.prob.flg <- rep(0, testlen)


for(i in 1:testlen){
  if(test.eve.prob[i] > 0.58){
    test.prob[i] <- as.character(test$evePolicy[i])
    test.prob.flg[i] <- "eve"
    
  }else if(test.1st.prob[i] > 0.5){
    test.prob[i] <- as.character(test$firstPolicy[i])
    test.prob.flg[i] <- "1st"
    
  }else{
    # どちらにもあてはまらない人はえいやで直前見積もりにしてしまう
    test.prob[i] <- as.character(test$evePolicy[i])
    test.prob.flg[i] <- "eve"
    
  }
}

table(test.prob.flg)

answer <- data.frame(customer_ID=test$customer_ID, plan=substr(test.prob, 2, 8))

write.csv(answer, "answer.csv", row.names=F)
# 最後に
# sed -e 's/\"//g' answer.csv > answer2.csv
# をして、ダブルクオートを削除して提出


