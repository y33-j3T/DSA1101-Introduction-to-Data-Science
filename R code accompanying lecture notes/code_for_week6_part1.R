fruit.dat= read.csv("fruit.csv")
fruit.dat=fruit.dat[,-1]
fruit.dat<- data.frame(lapply(fruit.dat, as.factor))
head(fruit.dat)

#Install package 'e1071' first
library(e1071)

model <- naiveBayes(Fruit ~ Long+Yellow+Sweet,
fruit.dat)

newdata <- data.frame(Long=1,Sweet=1, Yellow=0)
newdata <- data.frame(lapply(newdata, as.factor))

results <- predict (model,newdata,"raw")
results

results <- predict (model,newdata,"class")
results
