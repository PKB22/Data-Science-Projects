# Printing my name on top of Script 
myNameWithPrefix <- ("Plotting Basics : Pavan Kumar Bansal")
print(myNameWithPrefix)

#import the given libraries
install.packages(c("plyr","FSA","FSAdata","magrittr","dplyr","plotrix","ggplot2","moments"))

lapply(c("plyr","FSA","FSAdata","magrittr","dplyr","plotrix","ggplot2","moments"), require, character.only = TRUE)
search()

#3.	Load the BullTroutRML2 dataset:
myDataFrame <- BullTroutRML2
myDataFrame

#4 Print top and last 3 entries from Dataset
headtail(myDataFrame, n = 3)

myDataFrame <- filter(myDataFrame, lake == "Harrison" )
myDataFrame

headtail(myDataFrame, n = 3)

str(myDataFrame)

#Display summary and save it to txt file
summary(myDataFrame)
summaryTable <- summary(myDataFrame)
capture.output(summaryTable, file="myDataFrameSummary.txt")

#plot scatter graph for task 9
attach(myDataFrame)
plot(age~fl, main="Plot 1: Harrison Lake Trout", xlab="Fork Length (mm)",
     ylab="Age (yrs)", xlim= c(0,500), ylim=c(0,15), pch=20)

hist(age, main="Plot 2: Harrison Fish Age Distribution", xlab="Age (yrs)", ylab="Frequency",
     xlim= c(0,15), ylim=c(0,15), col="cadetblue", col.main="cadetblue")

plot(age~fl, main="Plot 3: Harrison Density Shaded by Era", 
     xlab="Fork Length (mm)",ylab="Age (yrs)", xlim= c(0,500),
     ylim=c(0,15), pch=20,col=c("blue","green"))

tmp <- headtail(BullTroutRML2, n = 3)
tmp
tmp$era

#Create a pchs vector with the argument values for + and x. Then create a cols vector 
#with the two elements "red" and "gray60"

pchs<- c("+","*")
cols<- c("red","grey60")
pchs
cols

tmp$era <-as.numeric(tmp$era)
is.numeric(tmp$era)

initialize(cols, tmp$era)


plot(age ~ fl,  main="Plot 4: Plot 4: Symbol & Color by Era", xlab="Fork Length (mm)", 
     ylab="Age (yrs)", xlim=c(0,500),ylim=c(0,15),pch=pchs, col=cols)

plot(age ~ fl,  main="Plot 5: With Regression Line", xlab="Fork Length (mm)",
     ylab="Age (yrs)", xlim=c(0,500),ylim=c(0,15),pch=pchs, col=cols)
abline(lm(age ~ fl),  lty=2,lwd=2, col="cadetblue")


plot(age ~ fl,  main="Plot 6: With legend details", xlab="Fork Length (mm)",
     ylab="Age (yrs)", xlim=c(0,500),ylim=c(0,15),pch=pchs, col=cols)
abline(lm(age ~ fl),  lty=2,lwd=2, col="cadetblue")
legend(x = "topleft", inset=.05, levels(myDataFrame$era),
       pch = pchs, col = cols, cex=0.75,bty = "n")




