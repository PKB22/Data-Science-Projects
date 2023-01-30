# Printing my name on top of Script and importing the libraries
myName <- ("Pavan Kumar Bansal")
print(myName)
install.packages("tidyverse")
lapply(c("plyr","FSA","FSAdata","magrittr","dplyr","tidyverse","tidyr"),
       require, character.only = TRUE)
search()
# importing CSV
bio <- read.csv("inchBio.csv")
#Display the head, tail and structure of <bio>
head(bio)
tail(bio)
str(bio)
#Create an object, <counts>, that counts and lists all the species records
counts <- cbind(length(bio$species), list(bio$species))
str(counts)
#Display just the 8 levels (names) of the species
unique(bio$species)
#Create a <tmp> object that displays the different species and the number of record of 
#each species in the dataset. 
tmp <- table(bio$species)
tmp
#Create a subset, <tmp2>, of just the species variable and display the first five records
tmp2 <- subset(bio,select = species)
head(tmp2, n=5)
#Create a table, <w>, of the species variable. Display the class of w
w <- table(bio$species)
class(w)
#Convert <w> to a data frame named <t> and display the results
t <- as.data.frame(w)
t
#Extract and display the frequency values from the <t> data frame
t$Freq
#Create a table named <cSpec> from the bio species attribute (variable)
cSpec <-table(bio$species)
class(cSpec)
cSpec
#Create a table named <cSpecPct> that displays the species and percentage of records
cSpecPct <- round(prop.table(cSpec), digit =3)
cSpecPct
class(cSpecPct)
#Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data 
u<- as.data.frame(cSpecPct)
class(u)
u
#Create a barplot of <cSpec> titled Fish Count
barplot(cSpec, main = "Fish Counts",  ylab = "COUNTS",ylim = c(0, 250),
        col="LightGreen",las=2, cex.axis = 0.6, cex.names=0.5)
# Create a barplot of <cSpecPct>
barplot(cSpecPct, main = "Fish Relative Frequency",  ylab = "COUNTS",cex.axis = 0.6
        ,ylim = c(0, 0.4),cex.names=0.5, las=2)
axis(2, col.axis="LightBlue",cex.axis = 0.6, las =2)
#Rearrange the <u> cSpec Pct data frame in descending order of relative frequency.
d<-u %>% arrange(desc(Freq))
d
class(d)
#Rename the <d> columns Var 1 to Species, and Freq to RelFreq
names(d)<-c("Species", "RelFreq")
d
#Add new variables to <d> and call them cumfreq,
d$cumfreq <- cumsum(d$RelFreq)
d
#Create a parameter variable <def_par> to store parameter variables
def_par <-par(font.lab=3, cex.lab=1.5, font.main=4, cex.main=2)
def_par  
