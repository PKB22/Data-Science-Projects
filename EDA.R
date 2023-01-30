library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(lubridate)

imdb <- read.csv("imdb_top_1000.csv")

summary(imdb)
str(imdb)
View(imdb)
dim(imdb)

colSums(is.na(imdb))
names(imdb)

summary(imdb$Meta_score)
imdb$Meta_score[is.na(imdb$Meta_score)] <- 79

sum(is.na(imdb))

imdb <- imdb %>% slice(-c(967))
imdb$Released_Year <- as.factor(imdb$Released_Year)

view(head(imdb))
imdb<-imdb[!duplicated(imdb$Series_Title), ]

imdb$Meta_score  <- as.factor(imdb$Meta_score)
imdb$Genre  <- as.factor(imdb$Genre)
imdb$Certificate[imdb$Certificate==""]<-"Unrated"
imdb$Certificate <- as.factor(imdb$Certificate)


imdb$Gross<-as.numeric(gsub(",", "", imdb$Gross))
imdb$Gross[is.na(imdb$Gross)] <- 23530892


imdb <- transform(imdb, Runtime = case_when(
  str_detect(Runtime, 'min') ~ as.numeric(str_replace(Runtime, 'min', ''))
))

###############################
options(scipen = 100)
summary(imdb)
str(imdb)

attach(imdb)
my_num_data <- imdb[, sapply(imdb, is.numeric)]

ggcorr(my_num_data, size = 2, label = TRUE, label_size = 2, label_round = 2, 
       label_alpha = TRUE)



imdb%>%ggplot( aes(x = IMDB_Rating) )+ geom_bar(fill='blue')


imdb %>%ggplot(aes(x = IMDB_Rating, y = Gross)) +
  geom_jitter(height=0.4, width=0.3, alpha = 0.3) +
  labs(title='IMDB Rating Distribution with respect to the Gross collection ')

barplot(table(Certificate)[sortingorder], col = 'grey')
table(Certificate)


ggplot(imdb, aes(x = IMDB_Rating, fill = Certificate))+
  geom_bar(stat = "Count")+
  xlab("IMDB_Rating")+
  ylab("Total number of Movies")+ 
  labs(title = "Rating distribution with the Certificate")


ggplot(imdb, aes(x = Certificate))+
  geom_bar(fill = 'lightgreen')+
  coord_flip()+
  ggtitle("Movie Certificate")

temp <- imdb %>%                                      
  arrange(desc(IMDB_Rating)) %>% 
  slice(1:5)
temp[,c('Series_Title','Released_Year','Certificate','Runtime','IMDB_Rating','Director','Gross')]




detach(imdb)
rm(imdb)
