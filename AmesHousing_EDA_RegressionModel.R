install.packages("leaps")
library(dplyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(GGally)
library(tidyverse)
library(leaps)
lapply(c("plyr","psych","car","gmodels"),
       require, character.only = TRUE)
search()

AmesHousing <- read.csv("AmesHousing.csv")
attach(AmesHousing)
summary(AmesHousing)
dim(AmesHousing)
str(AmesHousing)
head(AmesHousing)
names(AmesHousing)
AmesHousing=select(AmesHousing, -1)
options(scipen = 100)
names(AmesHousing %>% dplyr::select(where(is.numeric)))
names(AmesHousing %>% dplyr::select(where(is.character)))

summary(AmesHousing)

sapply(AmesHousing %>% dplyr::select(where(is.numeric)), function(a)(sum(is.na(a))))
AmesHousing$Lot.Frontage[is.na(AmesHousing$Lot.Frontage)] <- mean(AmesHousing$Lot.Frontage, na.rm = TRUE)
names(AmesHousing %>% dplyr::select(where(is.numeric)))
names(AmesHousing)

#histogram of Sales prices

ggplot(AmesHousing, aes(x = SalePrice)) +
  geom_histogram(color = "lightgrey",bins = 50) + 
  scale_x_continuous(labels = comma) +
  labs(title = "Sales prices", x = "Price", y = "Number of entries") +
  theme_minimal()

#histogram of Living Area
ggplot(AmesHousing, aes(x = Gr.Liv.Area)) +
  geom_histogram(color = "lightgreen",fill="white", bins = 50) + 
  scale_x_continuous(labels = comma) +
  labs(title = " Details of House sizes", x = "Ground Living area in (sqft)", 
  y = "occurrence") +
  theme_minimal()


neighbour = sort(tapply(AmesHousing$SalePrice,
                        AmesHousing$Neighborhood, median),decreasing = TRUE)
#Dot chart of of neighborhood
dotchart(neighbour, pch = 16, 
         cex = 0.70,xlab="Avg house price",main = " Price based on neighborhood?")


#Dot chart of of overall conditions
barplot(table(AmesHousing$Overall.Cond), 
        main = "House rating based on condition?", 
        xlab = "Rating",
        ylab = "Houses",
        col = brewer.pal(10, "RdYlGn"))



boxplot(AmesHousing$SalePrice,main="SalePrice")
boxplot(AmesHousing$Lot.Area,main="Lot.Area")


numeric_data = AmesHousing %>% select(where(is.numeric))
ggcorr(numric_data)



new_AmesHousing = numeric_data %>% select(SalePrice, Open.Porch.SF, Garage.Area, 
          Garage.Cars, Fireplaces, TotRms.AbvGrd, Half.Bath, Full.Bath,
          Bsmt.Full.Bath, Gr.Liv.Area, X2nd.Flr.SF, X1st.Flr.SF,
          BsmtFin.SF.1, Mas.Vnr.Area, Year.Built,
          Year.Remod.Add, Overall.Qual, Lot.Area, Lot.Frontage, Enclosed.Porch, PID)

ggcorr(new_AmesHousing)
ggcorr(new_AmesHousing, size = 2, label = TRUE, label_size = 2, label_round = 2, 
       label_alpha = TRUE)

plot(AmesHousing$SalePrice, AmesHousing$Enclosed.Porch, xlab = "SalePrice",ylab = "Porch in sqft",
     main = "Correaltion between price and porch")

ggplot(AmesHousing, aes(SalePrice, Gr.Liv.Area)) + 
  geom_point(size = 2, color = "black") + 
  theme_minimal() +
  geom_smooth(method = lm, color = "green", size = 2) + 
  scale_x_continuous(labels = comma)

plot(AmesHousing$SalePrice, AmesHousing$Mas.Vnr.Area, xlab = "Price",
     ylab = "Masonry veneer area",
     main = "Relationship price v/s masonry veneer area")

AmesHousingregression = AmesHousing %>% select(SalePrice,Gr.Liv.Area,Garage.Area,X1st.Flr.SF )

pairs(AmesHousingregression,main = "Relation in Ames housing data", 
      pch = 21,labels = c("Price","Living Area","Garage Area","1st Floor Area"),
      bg=c("green"),font.labels = 2,cex.labels = 2) 


model = lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + X1st.Flr.SF,
           data = AmesHousing)
summary(model)
dim(AmesHousing)
dim(AmeHousing_without_Outliers )
plot(model)
vif(model)

car::outlierTest(model)

AmeHousing_without_Outliers =  AmesHousing[-c(1499,2181,2182,1768,1761,45,1064,1638,434,2446),]

updatedmodel = lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + Garage.Area + X1st.Flr.SF,data = AmeHousing_without_Outliers)
summary(updatedmodel)

bestmodel <- regsubsets(SalePrice ~ Overall.Qual + Gr.Liv.Area + 
                          Garage.Area + X1st.Flr.SF+Year.Built+Full.Bath,
                        data = AmeHousing_without_Outliers, nvmax=6)
summary(bestmodel)

 regsum<- summary(bestmodel)
data.frame(
  Adj.R2 = which.max(regsum$adjr2),
  CP = which.min(regsum$cp),
  BIC = which.min(regsum$bic)
)

bestregmodel=lm(SalePrice ~ Overall.Qual + Gr.Liv.Area + 
                          Garage.Area + X1st.Flr.SF+Year.Built+Full.Bath,
                        data = AmeHousing_without_Outliers)
summary(bestregmodel)

a = c(AIC(updatedmodel), BIC(updatedmodel), AIC(bestregmodel), BIC(bestregmodel))
a
rm(AmesHousing)
