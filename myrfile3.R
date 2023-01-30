lapply(c("plyr","dplyr","psych","ggplot2","car","gmodels"),require, character.only = TRUE)
search()
StudentsPerformance <- read.csv("StudentsPerformance.csv")
summary(StudentsPerformance)
head(StudentsPerformance)
StudentsPerformance <- StudentsPerformance %>% rename( test_status= test.preparation.course)
StudentsPerformance=select(StudentsPerformance, -3,-4,-8)
names(StudentsPerformance)
attach(StudentsPerformance)
boxplot(StudentsPerformance$math.score)
t.test(StudentsPerformance$math.score, mu=66.09, alternative = "two.sided")
boxplot(StudentsPerformance$reading.score)
t.test(StudentsPerformance$reading.score, mu=68, alternative = "greater")
detach(StudentsPerformance)
rm(StudentsPerformance)




#describe(StudentsPerformance)
#str(StudentsPerformance)
#names(StudentsPerformance)
#datagroupbyTestStatus<-describeBy(StudentsPerformance, StudentsPerformance$test_status, skew= FALSE)
#datagroupbyTestStatus
#datagroupbyGender<-describeBy(StudentsPerformance, StudentsPerformance$gender, skew= FALSE)
#datagroupbyGender
#Students_Withtest <- StudentsPerformance %>% filter(test_status =="completed")
#Students_Withouttest <- StudentsPerformance %>% filter(test_status =="none")
#summary(Students_Withtest)
#summary(Students_Withouttest)
#t.test(Students_Withtest$math.score, mu=66.09 )
#t.test(Students_Withouttest$math.score, mu=66.09 )
