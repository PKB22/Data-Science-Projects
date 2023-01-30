#The hypotheses regarding Game Attendance is stated as follows.
#H0: median = 3000
#H1: median !=3000
alpha <- 0.05
median <- 3000
#vector of values
attendance<-c(6210,3150,2700,3012,4875,3540,6127,2581,2642,
              2573,2792,2800,2500,3700,6030,5437,2758,3490,2851,2720)
difference <-attendance-median
difference
pos<-length(difference[difference>0])
pos
neg<-length(difference[difference<0])
neg
testvalue <- min(pos, neg)
testvalue
result <- binom.test(x=c(pos,neg), alternative= "two.sided")
result
result$p.value
#critical value for n=20 two-tail at 0.05= 5, 
#which is less than the test value 10 , can't reject the null hypothesis
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")


#The hypotheses regarding Lottery Ticket Sales is stated as follows.
#H0: median = 200
#H1: median < 200
alpha <- 0.05
median <- 200
#vector of values
pos<-25
neg<-15
testvalue <- min(pos, neg)
testvalue
result <- binom.test(x=c(pos,neg), alternative= "less")
result
result$p.value
#critical value for n=40 left-tail at 0.05= -1.65,
#which is less than the test value -1.42, can't reject the null hypothesis
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")



#The hypotheses 
#H0: no sentence difference 
#H1: there is difference in sentences 
alpha <- 0.05
male<-c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
female<-c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)
result <- wilcox.test(x=male, y=female, alternative= "two.sided")
result
result$p.value
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")





#The hypotheses 
#H0: no difference 
#H1: there is difference 
alpha <- 0.05
NL<-c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL<-c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)
result <- wilcox.test(x=NL, y=AL, alternative= "two.sided")
result
result$p.value
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, 
       "Fail to reject the null hypothesis","Reject the null hypothesis")


#Test value <= Critical value
#13-16  Reject the null hypothesis
#32-117  Reject the null hypothesis
#65-60  Fail to reject the null hypothesis
#22-26  Reject the null hypothesis





#The hypotheses regarding crop is stated as follows.
#H0: there is no difference in means
#H1: there is a difference in means
alpha <- 0.05
WH<- data.frame( scores  = c(527,406, 474, 381, 411), group=rep("WH",5))
E<-  data.frame(scores = c( 520, 510, 513, 548, 496),group=rep("E",5))
EA<- data.frame(scores  = c(523,547,547 ,391 ,549),group=rep("EA",5))
data<-rbind(WH,E,EA)
data
result <- kruskal.test(scores ~group , data=data)
result
result$p.value
ifelse(result$p.value>alpha, 
       "Fail to reject the null hypothesis","Reject the null hypothesis")




#The hypotheses regarding crop is stated as follows.
#H0: No liner correlation between the variables 
#H1: liner correlation between the variables
alpha <- 0.05
city<- c(1, 2, 3, 4, 5, 6)
Subway<-c( 845, 494, 425, 313, 108, 41)
Rail<- c(39, 291, 142, 103, 33, 38)
data<-data.frame(city = city,Subway= Subway,Rail =Rail)
result <- cor.test(data$Subway,data$Rail , method="spearman" )
result$p.value
result$estimate
ifelse(result$p.value>alpha, 
       "Fail to reject the null hypothesis","Reject the null hypothesis")


floor(runif(40, min=100000, max=999999))


