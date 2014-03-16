# 作業ディレクトリの設定
setwd("~/dev/kaggle/ds2014")

# CSVファイルの読み込み
train <- read.csv("train.csv")
test <- read.csv("test.csv")

# summary関数で変数の概要が把握できる
summary(train$duration_previous)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   2.000   5.000   6.004   9.000  15.000   18711

# 属性値が数値の場合は、as.factorを付けて明示的に属性値として扱えるようにする
summary(as.factor(train$A))
#      0      1      2 
# 143691 426067  95491 

pdf("hist_grouped_cost.pdf", paper="a4")
hist(train$cost)
dev.off()