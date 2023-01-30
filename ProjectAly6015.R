library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(pROC)
library(MASS)
library(glmnet)
library(Metrics)
HotelDb <- read.csv("hotel.csv")
summary(HotelDb)
str(HotelDb)
View(head(HotelDb))
dim(HotelDb)
colSums(is.na(HotelDb))
summary(HotelDb$children)
HotelDb$children[is.na(HotelDb$children)] <- 0
sum(is.na(HotelDb))
names(HotelDb)
names(HotelDb)[names(HotelDb) == 'adr'] <- 'Avg_daily_rate'
names(HotelDb %>% dplyr::select(where(is.numeric)))
names(HotelDb %>% dplyr::select(where(is.character)))
HotelDb <- HotelDb %>% slice(-c(14970))
summary(HotelDb$Avg_daily_rate)
view(HotelDb%>%  filter(Avg_daily_rate>700))
HotelDb = HotelDb%>%  
  mutate(Avg_daily_rate = replace(Avg_daily_rate, Avg_daily_rate>700, median(Avg_daily_rate)))

attach(HotelDb)
HotelDb$is_canceled <-as.factor(HotelDb$is_canceled )
sortingorder = order(table(arrival_date_month))
barplot(table(arrival_date_month)[sortingorder], 
        xlim=c(0,16000),col = 'grey', cex.name = 0.8, horiz=T, las=1)
barplot(table(customer_type), col = 'blue',cex.name = 0.8)
barplot(table(assigned_room_type), col='lightgreen')
barplot(table(meal), col='lightgreen')
barplot(table(reservation_status), col = 'lightyellow')
HotelDb%>%
  ggplot(aes(x=arrival_date_year,fill=(as.factor(is_canceled))))+
  geom_bar()
HotelDb%>%
  ggplot(aes(x=hotel,fill=(as.factor(is_canceled))))+
  geom_bar()
HotelDb%>%
  ggplot(aes(x=deposit_type,fill=(as.factor(is_canceled))))+
  geom_bar()
barplot(table(deposit_type), col="green")

aggregate(HotelDb$reservation_status, by=list(HotelDb$reservation_status), FUN=length)
aggregate(HotelDb$distribution_channel, by=list(HotelDb$distribution_channel), FUN=length)
aggregate(HotelDb$customer_type, by=list(HotelDb$customer_type), FUN=length)
aggregate(HotelDb$deposit_type, by=list(HotelDb$deposit_type), FUN=length)
boxplot(HotelDb$Avg_daily_rate)
t.test(HotelDb$Avg_daily_rate, mu=102, alternative = "two.sided")
my_num_data <- HotelDb[, sapply(HotelDb, is.numeric)]
ggcorr(my_num_data, size = 2, label = TRUE, label_size = 2, label_round = 2, 
       label_alpha = TRUE)
HotelDb_WithParking <- HotelDb %>% filter(required_car_parking_spaces !=0)
HotelDb_WithoutParking <- HotelDb %>% filter(required_car_parking_spaces ==0)
t.test(HotelDb_WithParking$Avg_daily_rate,HotelDb_WithoutParking$Avg_daily_rate, alternative = "greater")

set.seed(4000)
trainIndex <- createDataPartition(HotelDb$Avg_daily_rate, p = 0.7, list = FALSE, times = 1)
caret_train <- HotelDb[ trainIndex,]
caret_test <- HotelDb[-trainIndex,]
summary(caret_train)
summary(caret_test)
barplot(table(caret_train$reservation_status), col = 'lightyellow')
barplot(table(caret_test$reservation_status), col = 'lightyellow')
caret_train_check<- caret_train[c('hotel','is_canceled','lead_time','adults',
                                  'arrival_date_month','arrival_date_day_of_month',
                                  'stays_in_weekend_nights','stays_in_week_nights',
                                  'distribution_channel','is_repeated_guest',
                                  'previous_cancellations','meal','market_segment',
                                  'previous_bookings_not_canceled',
                                  'reserved_room_type','deposit_type',
                                  'days_in_waiting_list','total_of_special_requests',
                                  'required_car_parking_spaces',
                                  'customer_type','Avg_daily_rate')]



model1<-glm(is_canceled~.,family="binomial",data=caret_train_check)
summary(model1)
predication_train<- predict(model1, newdata = caret_train_check, type="response")
predicited.classes.min <-as.factor(ifelse(predication_train>=0.5, "1","0"))
confusionMatrix(predicited.classes.min, caret_train_check$is_canceled, positive ='1')

model2<-glm(is_canceled~+deposit_type+lead_time+Avg_daily_rate+market_segment+
              arrival_date_month+total_of_special_requests+arrival_date_day_of_month+
              +previous_cancellations+ stays_in_week_nights+customer_type+required_car_parking_spaces
            +hotel+adults+distribution_channel+stays_in_weekend_nights+is_repeated_guest+
              previous_bookings_not_canceled+deposit_type, family="binomial",data=caret_train_check)

summary(model2)
predication_train<- predict(model2, newdata = caret_train_check, type="response")
predicited.classes.min <-as.factor(ifelse(predication_train>=0.5, "1","0"))
confusionMatrix(predicited.classes.min, caret_train_check$is_canceled, positive ='1')

predication_test<- predict(model2, newdata = caret_test, type="response")
predicited.classes.min <-as.factor(ifelse(predication_test>=0.5, "1","0"))
confusionMatrix(predicited.classes.min, caret_test$is_canceled, positive ='1')

ROC_curve<- roc(caret_test$is_canceled, predication_test)
plot(ROC_curve, col="red", ylab="TP rate", xlab="FP rate ")

area_under_curve<- auc(ROC_curve)
area_under_curve




train_x <-model.matrix(is_canceled~.,caret_train_check)[,-1]
test_x<-model.matrix(is_canceled~.,caret_test)[,-1]
train_y <-caret_train_check$is_canceled
test_y<-caret_test$is_canceled

set.seed(800)
cv.lasso<-cv.glmnet(train_x, train_y, nfolds=35)
log(cv.lasso$lambda.min)
log(cv.lasso$lambda.1se)
plot(cv.lasso)

model.1se<-glmnet(train_x, train_y, alpha=1, lambda=cv.lasso$lambda.1se)
model.1se
coef(model.1se)


model3<-glm(is_canceled~+lead_time+arrival_date_month++stays_in_weekend_nights+
              stays_in_week_nights+distribution_channel+
              previous_cancellations+meal+market_segment+
              reserved_room_type+deposit_type+total_of_special_requests+
              required_car_parking_spaces
            +customer_type+Avg_daily_rate, family="binomial",data=caret_train_check)
summary(model3)

predication_train<- predict(model3, newdata = caret_train_check, type="response")
predicited.classes.min <-as.factor(ifelse(predication_train>=0.5, "1","0"))
confusionMatrix(predicited.classes.min, caret_train_check$is_canceled, positive ='1')

predication_test<- predict(model3, newdata = caret_test, type="response")
predicited.classes.min <-as.factor(ifelse(predication_test>=0.5, "1","0"))
confusionMatrix(predicited.classes.min, caret_test$is_canceled, positive ='1')

ROC_curve<- roc(caret_test$is_canceled, predication_test)
plot(ROC_curve, col="red", ylab="TP rate", xlab="FP rate ")
area_under_curve<-auc(caret_test$is_canceled, predication_test)
area_under_curve

detach(HotelDb)
rm(HotelDb)
