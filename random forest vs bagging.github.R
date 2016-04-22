# The R-script compares the testing errors (not out-of-bag errors) from the bagging and random 
# forest methods in predicting median housing price at the Boston area. 
# 
# When implementing random forests, I set several different values for the number of predictors 
# allowed at each split (the "mtry" parameters). The I plot the result of each random forests as well 
# as bagging.
# 
# It appears to me that the best performer is that with "mtry" equal to 5, slightly less than half of
# the number of predictors.


library(randomForest)
library(boot)
library(MASS)
library(tree)
library(rpart)
library(ISLR)

# We use the "Boston" dataset from the "ISLR" package
names(Boston)

dim(Boston)
# 14 variables, one outcome and thirteen predictors

# Built a training set
train = sample(1:nrow(Boston), nrow(Boston)/2)

# Built a test set with the actua data value in that set
boston.test=Boston[-train,"medv"]

# Run random forests, allowing for 3 predictors selected at each split, i.e., "mtry" = 3
mse_3 = rep(0,400-25+1)
set.seed(3)
for (i in 25:400) {
  rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=3, ntree =i, importance=TRUE)
  yhat.rf = predict(rf.boston,newdata=Boston[-train,])
  mse_3[i-24] = mean((yhat.rf-boston.test)^2)
}

# "mtry" = 5
mse_5 = rep(0,400-25+1)
set.seed(4)
for (i in 25:400) {
  rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=5, ntree =i, importance=TRUE)
  yhat.rf = predict(rf.boston,newdata=Boston[-train,])
  mse_5[i-24] = mean((yhat.rf-boston.test)^2)
}

# "mtry" = 7
mse_7 = rep(0,400-25+1)
set.seed(5)
for (i in 25:400) {
  rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=7, ntree =i, importance=TRUE)
  yhat.rf = predict(rf.boston,newdata=Boston[-train,])
  mse_7[i-24] = mean((yhat.rf-boston.test)^2)
}

# "mtry" = 9
mse_9 = rep(0,400-25+1)
set.seed(9)
for (i in 25:400) {
  rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=9, ntree =i, importance=TRUE)
  yhat.rf = predict(rf.boston,newdata=Boston[-train,])
  mse_9[i-24] = mean((yhat.rf-boston.test)^2)
}

# "mtry" = 13, i.e. use all the 13 predictors. That is "bagging" is a special case of random forest
set.seed(13)
mse_bag = rep(0,400-25+1)
for (i in 25:400) {
  rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13, ntree =i, importance=TRUE)
  yhat.rf = predict(rf.boston,newdata=Boston[-train,])
  mse_bag[i-24] = mean((yhat.rf-boston.test)^2)
}

ntree = 25:400

png(file = "random forest vs bagging.github.png")
# line chart:http://www.statmethods.net/graphs/line.html
plot(ntree,mse_3, col = "red", xlab = "number of trees", ylab="test errors", type = "l")
lines(ntree,mse_5,col="green")
lines(ntree,mse_7,col="blue")
lines(ntree,mse_9,col="orange")
lines(ntree,mse_bag,col="black")
legend("topright", c("m=3", "m=5", "m=7", "m=9","bagging"), col=c("red","green", "blue", "orange","black"), cex=1, lty=1)
dev.off()

