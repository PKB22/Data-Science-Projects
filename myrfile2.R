install.packages("psych")
lapply(c("plyr","dplyr","psych","ggplot2","car","gmodels"),
       require, character.only = TRUE)
search()
StudentsPerformance <- read.csv("StudentsPerformance.csv")
str(StudentsPerformance)
summary(StudentsPerformance)
head(StudentsPerformance)
describe(StudentsPerformance)
names(StudentsPerformance)
StudentsPerformance=select(StudentsPerformance, -4, -5, -7,-8)
StudentsPerformance <- StudentsPerformance %>% rename( Highest_education= parental.level.of.education)
names(StudentsPerformance)
datagroupbyEduction<-describeBy(StudentsPerformance, StudentsPerformance$Highest_education, skew= FALSE)
datagroupbyEduction
datagroupbyGender<-describeBy(StudentsPerformance, StudentsPerformance$gender, skew= FALSE)
datagroupbyGender
datagroupbyRace<-describeBy(StudentsPerformance, StudentsPerformance$race.ethnicity, skew= FALSE)
datagroupbyRace
attach(StudentsPerformance)
StudentsPerformance %>% 
  ggplot(aes(x=gender, y=math.score)) + 
  geom_boxplot(width=0.5,lwd=1.5,aes(color=Highest_education)) +
  labs(subtitle="Coloring Boxplot, without jitter points")
StudentsPerformance %>% 
  ggplot(aes(x=gender, y=math.score)) + 
  geom_boxplot(width=0.5,lwd=1.5,aes(color=Highest_education)) +
  geom_jitter(width=0.25)+
  labs(subtitle="Coloring Boxplot, with jitter points")
boxplot(math.score~gender*race.ethnicity, col=c("red", "darkgreen"))
boxplot(StudentsPerformance$math.score)
ggplot(data=StudentsPerformance, aes(x=race.ethnicity, y=math.score)) + 
  geom_point(pch=20, color="green", size=2) + 
  labs(title="Math's Score with Race", x="Groups", y="Math Score")
detach(StudentsPerformance)
rm(StudentsPerformance)
