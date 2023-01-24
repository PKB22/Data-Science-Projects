install.packages("psych")
lapply(c("plyr","dplyr","psych","ggplot2","car","gmodels"),require, character.only = TRUE)
search()
StudentsPerformance <- read.csv("StudentsPerformance.csv")
sapply(StudentsPerformance, function(a)(sum(is.na(a)))) # NA counts
head(StudentsPerformance)
names(StudentsPerformance)
StudentsPerformance <- StudentsPerformance %>% rename( Highest_education= parental.level.of.education,race=race.ethnicity, test=test.preparation.course)

names(StudentsPerformance)
str(StudentsPerformance)
summary(StudentsPerformance)
attach(StudentsPerformance)

# Milestone 1
hist(math.score, main="Math's marks")
shapiro.test(math.score) 
hist(writing.score, main="Writing marks")
shapiro.test(writing.score) 
hist(reading.score, main="Reading marks")
shapiro.test(reading.score) 
boxplot(math.score,main="math marks")
boxplot(reading.score,main="Reading marks")
boxplot(writing.score,main="Writing marks")
boxplot(math.score~test,col=c("red","darkgreen"), xlab="Test Course Status", ylab="Math's Score")
boxplot(reading.score~test,col=c("red","darkgreen"), xlab="Test Course Status", ylab="Reading Score")
boxplot(writing.score~test,col=c("red","darkgreen"), xlab="Test Course Status", ylab="Writing Score")
plot(math.score~reading.score, main="Scatterplot of Math and reading score",pch=16, xlab="Maths", ylab="Reading")
plot(math.score~writing.score, main="Scatterplot of Math and writing score",pch=16, xlab="Maths", ylab="Writing")
datagroupbyEduction<-describeBy(StudentsPerformance, StudentsPerformance$Highest_education, skew= FALSE)
datagroupbyEduction
datagroupbyGender<-describeBy(StudentsPerformance, StudentsPerformance$gender, skew= FALSE)
datagroupbyGender
datagroupbyRace<-describeBy(StudentsPerformance, StudentsPerformance$race, skew= FALSE)
datagroupbyRace


#Milestone 2

#one sample test
boxplot(StudentsPerformance$math.score)
t.test(StudentsPerformance$math.score, mu=66.09, alternative = "two.sided")
boxplot(StudentsPerformance$reading.score)
t.test(StudentsPerformance$reading.score, mu=68, alternative = "greater")
boxplot(StudentsPerformance$writing.score)
t.test(StudentsPerformance$writing.score, mu=68.05, alternative = "two.sided")
# two sample test
Students_Withtest <- StudentsPerformance %>% filter(test =="completed")
Students_Withouttest <- StudentsPerformance %>% filter(test =="none")
t.test(Students_Withtest$math.score,Students_Withouttest$math.score)
t.test(Students_Withtest$writing.score,Students_Withouttest$writing.score)
t.test(Students_Withtest$reading.score,Students_Withouttest$reading.score)
Students_Female <- StudentsPerformance %>% filter(gender=="female")
Students_male <- StudentsPerformance %>% filter(gender =="male")
t.test(Students_Female$reading.score,Students_male$reading.score)


#Coerrelation and regression 
cor(StudentsPerformance[,c("reading.score", "writing.score", "math.score")])
pairs.panels(StudentsPerformance[6:8])
cor(Students_Female[,c("reading.score", "writing.score", "math.score")]) 
pairs.panels(Students_Female[6:8])
cor(Students_male[,c("reading.score", "writing.score", "math.score")]) 
pairs.panels(Students_male[6:8])




cor.test(reading.score, math.score, alternative="greater")
cor.test(Students_Female$reading.score, Students_Female$math.score, alternative="greater")
cor.test(Students_male$reading.score, Students_male$math.score, alternative="greater")


#scatter plot 
plot(reading.score,writing.score, main="Writing/Reading marks of all Students",xlab="Reading marks",
     ylab="Writing marks", pch=16, col= "blue")
cor.test(reading.score, writing.score, alternative="greater")
cor.test(Students_Female$reading.score, Students_Female$writing.score, alternative="greater")
cor.test(Students_male$reading.score, Students_male$writing.score, alternative="greater")

plot(reading.score,writing.score, main="Writing/Reading marks of all Students",xlab="Reading marks",
     ylab="Writing marks", pch=16, col= c("red","blue"))

regr=lm(reading.score ~ writing.score)
abline(regr,col= "green")
summary(regr)
ggplot(StudentsPerformance, aes(x=reading.score, y=writing.score, col=gender,lty=gender)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)

regr1=lm( Students_Female$writing.score ~ Students_Female$reading.score)
regr2=lm( Students_male$writing.score ~ Students_male$reading.score)
summary(regr1)
summary(regr2)
detach(StudentsPerformance)
rm(StudentsPerformance)
