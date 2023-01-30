library(dplyr)
library(tidyverse)
lapply(c("plyr","psych","car","gmodels"),require, character.only = TRUE)
search()
baseball <- read.csv("baseball.csv")
baseball$Decade <- baseball$Year - (baseball$Year %% 10)
summary(baseball)
baseball1<-baseball %>%  select(Decade,W)
baseball1<-aggregate(baseball1$W, by=list(Decade=baseball1$Decade), FUN=sum)
baseball1
baseball1 <-head(baseball1, -1)
baseball1


#The hypotheses regarding Baseball is stated as follows.
#H0: There is no difference in number of wins per decade
#H1: There is a difference in number of wins per decade
alpha <- 0.05
#vector of values
observed<- baseball1$x
#vector of probabilities 
p<-c(0.2,0.2,0.2,0.2,0.2)
#Chi-square test
result<- chisq.test(x=observed, p=p)
#result's properties
result$statistic
result$parameter
result
#critical value for degree of freedom 4 at 0.05= 9.488
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")

#The hypotheses regarding crop is stated as follows.
#H0: There is no impact of fertilizer and density over yield
#H1: There is a difference in number of wins per decade
crop_data <- read.csv("crop_data.csv")
crop_data$density <-as.factor(crop_data$density)
crop_data$fertilizer <-as.factor(crop_data$fertilizer)

crop_data
result <- aov(yield~density+fertilizer +density:fertilizer, data=crop_data)
summary(result)
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")
