install.packages("glmnet")
install.packages("Metrics")
library(ISLR)
library(caret)
library(dplyr)
library(glmnet)
library(Metrics)
search()
options(scipen = 100)

college = na.omit(College)
names(college)
head(college)
str(college)
summary(college)

View(college%>%  filter(Grad.Rate>100))
college = college%>%  
  mutate(Grad.Rate = replace(Grad.Rate, Grad.Rate>100, median(Grad.Rate)))

attach(college)
set.seed(800)
trainIndex <- createDataPartition(college$Apps, p = 0.7, list = FALSE, times = 1)
caret_train <- college[ trainIndex,]
caret_test <- college[-trainIndex,]
summary(caret_train)
summary(caret_test)

train_x <-model.matrix(Grad.Rate~.,caret_train)[,-1]
test_x<-model.matrix(Grad.Rate~.,caret_test)[,-1]
train_y <-caret_train$Grad.Rate
test_y<-caret_test$Grad.Rate


set.seed(800)
cv.lasso<-cv.glmnet(train_x, train_y, nfolds=10)
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)

plot(cv.lasso)

#Not regularize model 
ols<-lm(Grad.Rate~., data=caret_train)
coef(ols)
pred.ols<-predict(ols, new=caret_test)
pred.ols
rmse(caret_test$Grad.Rate, pred.ols)

#alpha=0 for ridge l1
model.min<-glmnet(train_x, train_y, alpha=0, lambda=cv.lasso$lambda.min)
model.min
coef(model.min)

model.1se<-glmnet(train_x, train_y, alpha=0, lambda=cv.lasso$lambda.1se)
model.1se
coef(model.1se)


preds.train<-predict(model.1se, newx = train_x)
train.rmse<-rmse(train_y, preds.train)
train.rmse
R2(train_y, preds.train)

preds.test<-predict(model.1se, newx = test_x)
test.rmse<-rmse(test_y, preds.test)
test.rmse
R2(test_y, preds.test)

#alpha =1 for lasso l2
model.min<-glmnet(train_x, train_y, alpha=1, lambda=cv.lasso$lambda.min)
model.min
coef(model.min)
model.1se<-glmnet(train_x, train_y, alpha=1, lambda=cv.lasso$lambda.1se)
model.1se
coef(model.1se)


preds.train<-predict(model.1se, newx = train_x)
train.rmse<-rmse(train_y, preds.train)
train.rmse
R2(train_y, preds.train)

preds.test<-predict(model.1se, newx = test_x)
test.rmse<-rmse(test_y, preds.test)
test.rmse
R2(test_y, preds.test)


model_back <- step(lm(Grad.Rate ~ ., data = caret_train), direction = 'backward')
summary(model_back)




detach(college)
rm(college)


