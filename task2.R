library(readr)
M1 <- read_csv("M1_final.csv")
summary(M1)
unique(M1$MONTH)
unique(M1$DAY_OF_MONTH)
ls(M1)
unique(M1$OP_UNIQUE_CARRIER)
unique(M1$TAIL_NUM)
unique(M1$`Dew Point`)
unique(M1$DEST)
unique(M1$Wind)
unique(M1$Condition)
library(caTools)#for splitting
library(CatEncoders) # label & one hot encoding
library(lmtest) 
library(e1071)# For svr model
library(Metrics)
library(caret)
library(car)
library(dummies)
library(superml)
#Splitting data into 90:10 train and test data  

split <- sample.split(M1$TAXI_OUT,.90)
train <- subset(M1,split==TRUE)
test <- subset(M1,split==FALSE)
dim(train)
dim(test)
#label encoding
na_cols <- colSums(is.na(train)) / nrow(train)
na_cols <- names(na_cols[which(na_cols > 0.9)])

cat_cols <- names(train)[sapply(train, is.character)]

for(c in cat_cols){
  lbl <- LabelEncoder$new()
  lbl$fit(c(train[[c]], test[[c]]))
  train[[c]] <- lbl$transform(train[[c]])
  test[[c]] <- lbl$transform(test[[c]])
}

# fill missing value with  0
train[is.na(train)] <- 0
test[is.na(test)] <- 0

#boxplot
boxplot(train)
boxplot(train$DISTANCE)
#outliers
Q <- quantile(train$DISTANCE,probs = c(.25,.75),na.rm = TRUE)
iqr <- IQR(train$DISTANCE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???
eliminated<- subset(train, train$DISTANCE > (Q[1] - 1.5*iqr) & train$DISTANCE < (Q[2]+1.5*iqr))
dim(eliminated)
#Comparing with and without outliers
X1 <- lm(train$TAXI_OUT~.,data = train)
summary(X1)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
#plot(X1)
X2 <- lm(eliminated$TAXI_OUT~.,eliminated)
summary(X2)
par(mfrow=c(2,2)) # init 4 charts in 1 panel
#plot(X2)

#As we can see that there is very less difference between
#after and before removing the outliers and its distance so we keep it

#1linear
lf <- LMTrainer$new(family="gaussian")
lf$fit(X = train, y = "TAXI_OUT")
predictions <- lf$predict(df = test)
r11 <- rmse(actual = test$TAXI_OUT, predicted = predictions)
summary(lf$model)


#2Lasso regression
lf <- LMTrainer$new(family = "gaussian", alpha = 1, lambda = 1000)
lf$fit(X = train, y = "TAXI_OUT")
predictions <- lf$predict(df = test)
r12 <- rmse(actual = test$TAXI_OUT, predicted = predictions)
#3ridge regression
lf <- LMTrainer$new(family = "gaussian", alpha=0)
lf$fit(X = train, y = "TAXI_OUT")
predictions <- lf$predict(df = test)
r13 <- rmse(actual = test$TAXI_OUT, predicted = predictions)
#4KNN
knn <- KNNTrainer$new(k = 2,prob = T,type = 'reg')
knn$fit(train = train, test = test, y = "TAXI_OUT")
probs <- knn$predict(type = 'prob')
labels <- knn$predict(type='raw')

r14 <- rmse(actual = test$TAXI_OUT, predicted=labels)
#5SVR
svm <- svm(train$TAXI_OUT~.,train)
print(svm)
pred <- predict(svm,test)
r15 <- rmse(actual = test$TAXI_OUT, predicted = pred)
#6random forest
rf <- RFTrainer$new(n_estimators = 500,classification = 0)
rf$fit(X = train, y = "TAXI_OUT")
pred <- rf$predict(df = test)
rf$get_importance()
r16 <- rmse(actual = test$TAXI_OUT, predicted = pred)
#7 Naive bayes
model <- naiveBayes(train$TAXI_OUT~.,train)
pred3 <-as.numeric( predict(model,test))
r17 <- rmse(test$TAXI_OUT,pred3)
#8Light gbm
gbm <- XGBTrainer$new(objective = "reg:linear"
                      , n_estimators = 500
                      , eval_metric = "rmse"
                      , maximize = F
                      , learning_rate = 0.1
                      ,max_depth = 6)
gbm$fit(X = train, y = "TAXI_OUT", valid = test)
predx <- gbm$predict(test)
r18 <- rmse(actual = test$TAXI_OUT, predicted = predx)


#-------------------ONE HOT ENCODING------------------#
M12 <- read_csv("M1_final.csv")
summary(M12)
#Label encoding column with 1000 unique values
lbl <- LabelEncoder$new()
lbl$fit(M12$TAIL_NUM)
M12$TAIL_NUM <- lbl$fit_transform(M12$TAIL_NUM)
View(M12)

library(caret)
library(dummies)
#One hot encoding
dmy <- dummyVars("~.", data = M12, fullRank = T)

dat<- data.frame(predict(dmy, newdata = M12))
View(dat)
split <- sample.split( dat$TAXI_OUT,.90)
train <- subset(dat,split==TRUE)
test <- subset(dat,split==FALSE)
dim(train)
dim(test)
names(train)
# fill missing value with  0
train[is.na(train)] <- 0
test[is.na(test)] <- 0


#1linear
lf <- LMTrainer$new(family="gaussian")
lf$fit(X = train, y = "TAXI_OUT")
predictions <- lf$predict(df = test)
r21 <- rmse(actual = test$TAXI_OUT, predicted = predictions)
summary(lf$model)


#2Lasso regression
lf <- LMTrainer$new(family = "gaussian", alpha = 1, lambda = 1000)
lf$fit(X = train, y = "TAXI_OUT")
predictions <- lf$predict(df = test)
r22 <- rmse(actual = test$TAXI_OUT, predicted = predictions)
#3ridge regression
lf <- LMTrainer$new(family = "gaussian", alpha=0)
lf$fit(X = train, y = "TAXI_OUT")
predictions <- lf$predict(df = test)
r23 <- rmse(actual = test$TAXI_OUT, predicted = predictions)
#4KNN
knn <- KNNTrainer$new(k = 2,prob = T,type = 'reg')
knn$fit(train = train, test = test, y = "TAXI_OUT")
probs <- knn$predict(type = 'prob')
labels <- knn$predict(type='raw')

r24 <- rmse(actual = test$TAXI_OUT, predicted=labels)
#5SVR
svm <- svm(train$TAXI_OUT~.,train)
pred <- predict(svm,test)
r25 <- rmse(actual = test$TAXI_OUT, predicted = pred)
#6random forest
rf <- RFTrainer$new(n_estimators = 500,classification = 0)
rf$fit(X = train, y = "TAXI_OUT")
pred <- rf$predict(df = test)
rf$get_importance()
r26 <- rmse(actual = test$TAXI_OUT, predicted = pred)
#7 Naive bayes
model <- naiveBayes(train$TAXI_OUT~.,train)
pred3 <-as.numeric( predict(model,test))
r27 <- rmse(test$TAXI_OUT,pred3)
#8Light gbm
gbm <- XGBTrainer$new(objective = "reg:linear"
                      , n_estimators = 500
                      , eval_metric = "rmse"
                      , maximize = F
                      , learning_rate = 0.1
                      ,max_depth = 6)
gbm$fit(X = train, y = "TAXI_OUT", valid = test)
predx <- gbm$predict(test)
r28 <- rmse(actual = test$TAXI_OUT, predicted = predx)

RMS1 <- c(r11,r12,r13,r14,r15,r16,r17,r18)
RMS2 <- c(r21,r22,r23,r24,r25,r26,r27,r28)
nm <- c("Linear","Lasso","Ridge","Knn","SVR","Random forest","Naive bayes","LightGbm")
dfr <- data.frame(RMS1,RMS2,nm)
View(dfr)
library(car)
plot(x=seq(nm), y=RMS1, type="b", lty=1,pch=19,col="red", ylim=c(5,20),
     axes=F, bty="n", xaxs="i", yaxs="i", main="Comparison",
     xlab="", ylab="RMS")
lines(x=seq(nm), y=RMS2, pch=19,type="b",col="blue",lty=2)
# add axes
axis(side=1, labels=nm, at=seq(nm))
axis(side=2, at=seq(5,20,1), las=1)

# add legend
par(xpd=TRUE)
legend(x=1.5, y=2, legend=c("RMS1", "RMS2"),col=c("red","blue"), lty=1:2, cex=.8,box.lty=0, ncol=2)

