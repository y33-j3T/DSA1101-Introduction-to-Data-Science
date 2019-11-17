##Stock market example

market = read.csv("Smarket.csv")
dim(market)

summary(market[,2:10])


train =(market$Year <2005)
train.data = market[train,]
test.data  = market[!train ,]

dim(test.data)

library(class)


train.x = train.data[,c("Lag1","Lag2","Lag3","Lag4","Lag5")]
test.x = test.data[,c("Lag1","Lag2","Lag3","Lag4","Lag5")]
train.y = train.data[,c("Direction")]
test.y = test.data[,c("Direction")]
set.seed(1)
knn.pred = knn(train.x,test.x,train.y,k=1)
confusion.matrix=table(knn.pred, test.y)
confusion.matrix


knn.pred = knn(train.x,test.x,train.y,k=10)
confusion.matrix=table(knn.pred, test.y)
confusion.matrix


##Caravan example

caravan = read.csv("Caravan.csv")

head(caravan$Purchase)
summary(caravan$Purchase)
plot(caravan$Purchase)

caravan=caravan[,-1]
standardized.X= scale(caravan[,-86])

var(caravan [,1])
var(caravan [,2])
var(standardized.X[,1])
var(standardized.X[,2])

test=1:1000
train.X=standardized.X[-test ,]
test.X =standardized.X[test ,]
train.Y=caravan$Purchase[-test]
test.Y =caravan$Purchase[test]



set.seed (1)

knn.pred = knn(train.X,test.X,train.Y,k=1)
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix


knn.pred = knn(train.X,test.X,train.Y,k=3)
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix


knn.pred = knn(train.X,test.X,train.Y,k=5)
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix

##Customer churn example

churn = read.csv("churn.CSV")

summary(as.factor(churn$Churned))

#Remove ID column
churn= churn[,-1]
#Standardize continuous variables
churn[,c("Age","Cust_years","Churned_contacts")]=
scale(churn[,c("Age","Cust_years","Churned_contacts")])

churn.X = churn[,-1]

test=1:4000
train.X=churn.X[-test,]
test.X =churn.X[test ,]
train.Y=churn$Churned[-test]
test.Y =churn$Churned[test]

set.seed (1)
knn.pred = knn(train.X,test.X,train.Y,k=1)
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix
sum(diag(confusion.matrix))/sum(confusion.matrix)

knn.pred = knn(train.X,test.X,train.Y,k=10)
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix
sum(diag(confusion.matrix))/sum(confusion.matrix)

knn.pred = knn(train.X,test.X,train.Y,k=50)
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix
sum(diag(confusion.matrix))/sum(confusion.matrix)

