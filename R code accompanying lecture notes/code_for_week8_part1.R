#Bank data example

bankdata = read.csv("bank-sample.csv", header=TRUE)
head(bankdata)

install.packages("rpart")
install.packages("rpart.plot") 
library("rpart") 
library("rpart.plot")

# Fit decision tree
fit <- rpart(subscribed ~job + marital + education 
+ default + housing + loan + contact + poutcome,
method="class",
data=bankdata,
control=rpart.control(minsplit=1),
parms=list(split='information'))

# Plot decision tree
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)

#Compute entropy

length(bankdata$poutcome)
table(bankdata$poutcome)

# Split at success versus non-success (other, unknown, failure)
x1=which(bankdata$poutcome!="success")
x2=which(bankdata$poutcome=="success")
table(bankdata$subscribed[x1])
table(bankdata$subscribed[x2])

# Split at failure versus non-failure (success, other, unknown)
x1=which(bankdata$poutcome!="failure")
x2=which(bankdata$poutcome=="failure")
table(bankdata$subscribed[x1])
table(bankdata$subscribed[x2])


length(bankdata$education)
table(bankdata$education)
x1=which(bankdata$education=="tertiary")
x2=which(bankdata$education!="tertiary")
table(bankdata$subscribed[x1])
table(bankdata$subscribed[x2])


#Wine example

wine_df <- read.csv("wine.csv", header = TRUE)

wine_df <- read.csv("wine.csv", header = TRUE)
fit <- rpart(Wine ~.,
method="class",
data=wine_df,
control=rpart.control(maxdepth=4),
parms=list(split='information'))

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)


# Build tree with Gini index criterion
wine_df <- read.csv("wine.csv", header = TRUE)
fit <- rpart(Wine ~.,
method="class",
data=wine_df,
control=rpart.control(maxdepth=4),
parms=list(split='gini'))

rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen=0)










