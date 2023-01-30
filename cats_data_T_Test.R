install.packages("MASS")
install.packages("PairedData")
library(MASS)
library(dplyr)
search()

str(cats)
summary(cats)
head(cats)

cats_male <- cats %>% filter(Sex =="M")
cats_female <- cats %>% filter(Sex =="F")
summary(cats_female)
summary(cats_male)



t.test(cats_female$Bwt,cats_male$Bwt)




beforemediate <-c(4.6, 7.8, 9.1, 5.6, 6.9, 8.5, 5.3, 7.1, 3.2, 4.4)
aftermediate <-c(6.6, 7.7, 9.0, 6.2, 7.8, 8.3, 5.9, 6.5, 5.8, 4.9)
boxplot(aftermediate,beforemediate, names=c("after","before"))

t.test(aftermediate, beforemediate,alternative = "greater",paired = TRUE)

t.test(aftermediate, beforemediate,alternative = "greater",conf.level = 0.90,paired = TRUE)
