library("rpart") # load libraries
library("rpart.plot")

play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
head(play_decision)

fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
method="class",
data=play_decision,
control=rpart.control(minsplit=1),
parms=list(split='information'))

rpart.plot(fit, type=4, extra=2)

#Prediction
newdata <- data.frame(Outlook="rainy", Temperature="mild",
Humidity="high", Wind=FALSE)

predict(fit,newdata=newdata,type="prob")
predict(fit,newdata=newdata,type="class")