lapply(c("plyr","dplyr","plotrix","ggplot2","gmodels","crosstable"),
       require, character.only = TRUE)
search()

LungCapData <- read.csv("LungCapDataCSV.csv")

head(LungCapData)
tail(LungCapData)
str(LungCapData)

names(LungCapData)
LungCapData <- LungCapData %>% rename(Lung_Capacity = LungCap)
colnames(LungCapData)[1]
names(LungCapData)


summary(LungCapData)

class(LungCapData)

attach(LungCapData)

LungCapData=select(LungCapData, -6)
names(LungCapData)

LungCapData$Smoke <- gsub("(?i)y|(?i)yes", "1", LungCapData$Smoke)
LungCapData$Smoke <- gsub("(?i)n|(?i)no", "0", LungCapData$Smoke)
unique(LungCapData$Smoke)
typeof(LungCapData$Smoke)
LungCapData$Smoke <-as.factor(LungCapData$Smoke)
levels(LungCapData$Smoke)




count(LungCapData, Gender) 
count(LungCapData, Smoke) 
count(LungCapData, Age) 
count(LungCapData, Gender,Smoke) 


ftable( Age,Gender,Smoke)






CrossTable(LungCapData$Gender, LungCapData$Smoke, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)






Datasetcrosstable1 = crosstable(LungCapData, c(Lung_Capacity,Age, Height, Smoke), by=Gender, 
                    total="both",percent_pattern="{n} ({p_row}/{p_col})", percent_digits=0) %>%
                    as_flextable()
Datasetcrosstable1

Datasetcrosstable2 = crosstable(LungCapData, c(Lung_Capacity,Age, Height,Gender),
                                by=Smoke, total="both", 
                 percent_pattern="{n} ({p_row}/{p_col})", percent_digits=0) %>%  as_flextable()
Datasetcrosstable2



hist(Lung_Capacity,main = "Lung Capacity Distribution", 
     xlab = "Lung Capacity", col = "lightblue", border = "white",las =1)

hist(Age,main = "Age Distribution", xlab = "Age", col = "Blue", border = "white",las =1)


hist(Height,main = "Height Distribution",xlab = "Height",
     col = "lightGreen", border = "white",las =1)


plot(Lung_Capacity~Height, main="Test", xlab="Height",ylab="Lung_Capacity", xlim= c(40,80),ylim=c(0,20), pch=20)

plot(Lung_Capacity~Age, main="Test", xlab="Age (yrs)",ylab="Lung_Capacity", xlim= c(0,20),ylim=c(0,20), pch=20)
plot(Lung_Capacity~Age, main="Test", xlab="Smoke",ylab="Lung_Capacity", xlim= c(0,50),ylim=c(0,20), pch=20)


detach(LungCapData)
rm(LungCapData)


#col=c("blue","green","red")
#LungCapData$Gender <-as.factor(LungCapData$Gender)
#levels(LungCapData$Gender)
#CrossTable(LungCapData$Lung_Capacity, LungCapData$Smoke, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
#CrossTable(LungCapData$Lung_Capacity, LungCapData$Height, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
#CrossTable(LungCapData$Lung_Capacity, LungCapData$Age, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)
#CrossTable(LungCapData$Lung_Capacity, LungCapData$Gender, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

#( Lung_Capacity,Gender)
#ftable( Lung_Capacity,Smoke)
