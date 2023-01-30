#significance level
alpha <- 0.10
#vector of values
observed<- c(12,8,24,6)
#vector of probabilities 
p<-c(0.20,0.28,0.36,0.16)

#Chi-square test
result<- chisq.test(x=observed, p=p)

#result's properties
result$statistic
result$p.value
result$parameter
result

#critical value for degree of freedom 3 at 90% CT= 6.251
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")



############### ONTIME AIR 


#significance level
alpha <- 0.05
#vector of values
observed<- c(125,10,25,40)
#vector of probabilities 
p<-c(0.708,0.082,0.09,0.12)

#Chi-square test
result<- chisq.test(x=observed, p=p)

#result's properties
result$statistic
result$p.value
result$parameter
result

#critical value for degree of freedom 3 at 95% CT= 7.815
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")




############### eTHNICITY AND moVIE


#significance level
alpha <- 0.05
#vector of EACH YEAR ROW
r1<- c(724,335,174,107)
r2<-c(370,292,152,140)
#NUMBER OF ROWS
ROWS =2

MTRX =matrix(c(r1,r2), nrow=ROWS, byrow = TRUE)

rownames(MTRX)=c("2013","2014")
colnames(MTRX)=c("Caucasian","Hispanic","African American","Other")

MTRX

#Chi-square test
result<- chisq.test(MTRX)
#result's properties
result$statistic
result$p.value
result$parameter
result

#critical value for degree of freedom 3 at 95% CT= 7.815
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")





############### Women in the militray


#significance level
alpha <- 0.05
#vector of EACH Row
r1<- c(10791,62491)
r2<-c(7816,42750)
r3<- c(932,9525)
r4<-c(11819, 54344)


#NUMBER OF ROWS
ROWS =4

MTRX =matrix(c(r1,r2,r3,r4), nrow=ROWS, byrow = TRUE)

rownames(MTRX)=c("Army","Navy","Marine Corps", "Air Force")
colnames(MTRX)=c("Officers","Enlisted")

MTRX

#Chi-square test
result<- chisq.test(MTRX)
#result's properties
result$statistic
result$p.value
result$parameter
result

#critical value for degree of freedom 3 at 95% CT= 7.815
#check for hypothesis for final decision 
ifelse(result$p.value>alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")



########################Sodium content 


#significance level
alpha <- 0.05


#dataframe for each food with sodium level
Sodiumelevel <- c(270,130,230, 180, 80, 70,200)
food <- rep('Condiments',7)
Condiments<-data.frame(Sodiumelevel,food)

Sodiumelevel <- c(260,220,290,290,200,320,140)
food <- rep('Cereals',7)
Cereals<- data.frame(Sodiumelevel,food)

Sodiumelevel <- c(100, 180, 250, 250, 300, 360, 300, 160)
food <- rep('Desserts',8)
Desserts<-data.frame(Sodiumelevel,food)

# combine data frames in one
fooddata<- rbind(Condiments,Cereals, Desserts)

#make food column as factor type
fooddata$food <-as.factor(fooddata$food)

# perform Anova testing
result <- aov(Sodiumelevel~food, data=fooddata)
summary(result)
p.value <- summary(result)[[1]][[1,"Pr(>F)"]]

#critical value for D.N.F(2) and D.F.D.(19) at alpha 0.05 is 3.52
#check for hypothesis for final decision 
ifelse(p.value >alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")






########################Sales for Leading Companies 


#significance level
alpha <- 0.01
#dataframe for each food with sales in millions
Sales <- c(578,320,264,249,237)
food <- rep('Cereals',5)
Cereals<-data.frame(Sales,food)

Sales <- c(311,106,109, 125,173)
food <- rep('Chocolate Candy',5)
Chocolate_Candy <- data.frame(Sales,food)

Sales <- c(261,185,302,689)
food <- rep('Coffee',4)
Coffee<-data.frame(Sales,food)

# combine data frames in one
Salesdata<- rbind(Cereals,Chocolate_Candy, Coffee)

#make food column as factor type
Salesdata$food <-as.factor(Salesdata$food)

# perform Anova testing
result <- aov(Sales~food, data=Salesdata)
summary(result)
p.value <- summary(result)[[1]][[1,"Pr(>F)"]]

#critical value for D.N.F(2) and D.F.D.(11) at alpha 0.01 is 7.21
#check for hypothesis for final decision 
ifelse(p.value >alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")
TukeyHSD(result)









#############per pupil Expenditures 



#significance level
alpha <- 0.05
#dataframe for each section with Expenditures in $
Expenditures <- c(4946,5953,6202, 7243, 6113)
section <- rep('Eastern third',5)
Eastern<-data.frame(Expenditures,section)

Expenditures <- c(6149, 7451, 6000, 6479)
section <- rep('Middle third',4)
Middle <- data.frame(Expenditures,section)

Expenditures <- c(5282,8605,6528,6911)
section <- rep('Western third',4)
Western<-data.frame(Expenditures,section)

# combine data frames in one
Expendituresdata<- rbind(Eastern,Middle, Western)

#make food column as factor type
Expendituresdata$section <-as.factor(Expendituresdata$section)
Expendituresdata
# perform Anova testing
result <- aov(Expenditures~section, data=Expendituresdata)
summary(result)
p.value <- summary(result)[[1]][[1,"Pr(>F)"]]

#critical value for D.N.F(2) and D.F.D.(10) at alpha 0.05 is 4.10
#check for hypothesis for final decision 
ifelse(p.value >alpha, "Fail to reject the null hypothesis","Reject the null hypothesis")
TukeyHSD(result)


