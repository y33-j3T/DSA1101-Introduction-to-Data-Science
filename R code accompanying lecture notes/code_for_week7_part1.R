#Bank data example

bankdata = read.csv("bank-sample.csv", header=TRUE)
head(bankdata)

install.packages("rpart")
install.packages("rpart.plot") 
library("rpart") 
library("rpart.plot")

fit <- rpart(subscribed ~job + marital + education + default + housing + loan + contact + poutcome,
             method="class", data=bankdata, control=rpart.control(minsplit=1),parms=list(split='information'))


rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)

#Iris classfication example

iris= read.csv("iris.csv",header=FALSE)
names(iris)= c("sepalLength","sepalWidth",
               "petalLength","petalWidth","Species")


fit.iris <- rpart(Species ~sepalLength +sepalWidth+petalLength+petalWidth,
                  method="class",data=iris,control=rpart.control(minsplit=1), parms=list(split='information'))

rpart.plot(fit.iris, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)


#Compare the decision tree with the visual plots

install.packages("ggplot2")
install.packages("magrittr")
library(ggplot2)
library(magrittr)
# sepal width vs. sepal length
ggplot(iris, aes(x=sepalLength, y=sepalWidth, color=Y)) +
geom_point()+
labs(x = "sepal length")+  labs(y = "sepal width") 
# petal width vs. petal length
ggplot(iris, aes(x=petalLength, y=petalWidth, color=Y)) +
geom_point()+
labs(x = "petal length")+  labs(y = "petal width") 
